!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Physics parameters YAML reader
!
!  YAML block: physics:        (all optional; defaults = fully-nonlinear Boussinesq)
!    dispersion:
!      scheme:      <string>   fully_nonlinear | weakly_nonlinear | linear | nswe,
!                              default fully_nonlinear.  Presets Gamma1/2/3:
!                                fully_nonlinear  -> 1, 1, 1
!                                weakly_nonlinear -> 1, 1, 0
!                                linear           -> 1, 0, 1
!                                nswe             -> 0, 0, 1  + dispersion terms off
!      gamma1:      <real>     dispersion coefficient — explicit override of the preset
!      gamma2:      <real>     nonlinearity coefficient (CART) — override
!      gamma3:      <real>     linearity switch coefficient — override
!      beta_ref:    <real>     reference level (CART/ZALPHA),  default -0.531
!      swe_eta_dep: <real>     SWE transition depth fraction,  default 0.8
!                              (project standard since 2026-07-16; legacy
!                              default was 0.7)
!
!  Also read here as a stop-gap adapter (final owner comes later in the
!  config reorg; see design notes):
!    grid: > coriolis:             f-plane rotation for non-georeferenced
!      f: <real>                   Coriolis parameter (1/s); wins over latitude
!      latitude: <real>            centre latitude (deg), f = pi*sin(lat)/21600
!                                  (a geographic CRS will derive f per cell
!                                  once the metric provider exists)
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_physics_mod
   use core_constants_mod, only: SP, PI
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_PHYSICS_DISPERSION_BETA_REF, &
                                        DEF_PHYSICS_DISPERSION_SCHEME, &
                                        DEF_PHYSICS_DISPERSION_SWE_ETA_DEP, &
                                        DEF_PHYSICS_DISPERSION_SWE_ETA_RAMP

   implicit none

   private
   public :: type_model_physics

   character(len=16), parameter :: DISPERSION_SCHEMES(4) = &
                                   [character(len=16) :: "fully_nonlinear", &
                                                          "weakly_nonlinear", "linear", "nswe"]

   type, extends(type_model_base) :: type_model_physics

      logical  :: periodic = .false.   ! y-axis periodic (south-north wrap)
      logical  :: periodic_x = .false. ! x-axis periodic (west-east wrap)
      logical  :: dispersion = .true.
      real(SP) :: Gamma1 = 1.0_SP
      real(SP) :: Gamma2 = 1.0_SP
      ! semi-implicit Gamma2 LHS correction: only the .false. chain is
      ! ported (kernel_etauv), so no YAML key until the feature lands
      logical  :: disp_time_left = .false.
      real(SP) :: Beta_ref = -0.531_SP
      real(SP) :: Gamma3 = 1.0_SP
      logical  :: viscosity_breaking = .true.   ! set from breaking.model in model_setup
      real(SP) :: SWE_ETA_DEP = 0.80_SP
      ! smoothstep taper width below SWE_ETA_DEP; 0 = legacy hard switch.
      ! Initializer must track the registry default -- the reads sit inside
      ! the dispersion: block guard, so block-less decks land here
      real(SP) :: SWE_ETA_RAMP = 0.1_SP

      ! f-plane Coriolis (legacy has the source term in the spherical
      ! branch only; [[design-grid-crs]] decouples f from the metric —
      ! the future CRS provider fills the per-cell array from this)
      logical  :: coriolis_on = .false.
      real(SP) :: coriolis_f = 0.0_SP

   contains
      procedure :: read_input => physics_read_input
   end type type_model_physics

contains

   subroutine physics_read_input(this, env)
      class(type_model_physics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env, grid_env
      type(type_yaml_reader) :: disp_yaml, cor_yaml
      character(:), allocatable :: scheme
      logical :: is_empty, no_key, no_grid, no_disp, no_cor, no_f, no_lat
      real(SP) :: lat, g_tmp

      ! boundaries.periodic is read by model_boundaries_mod, which writes
      ! this%periodic AFTER this reader runs — keep this the storage slot
      ! only (stepper/geometry consume it)

      ! grid.coriolis — f-plane escape hatch for non-georeferenced grids:
      !   $$ f = \frac{\pi \sin\varphi}{21600} = 2\Omega\sin\varphi,
      !      \quad \Omega = \frac{2\pi}{86400} $$
      ! same discrete constant as the legacy spherical fill (init.F)
      grid_env = get_sub_env(env, "grid", no_grid)
      if (.not. no_grid) then
         cor_yaml = grid_env%yaml%cast_dictionary("coriolis", no_cor)
         if (.not. no_cor) then
            this%coriolis_on = .true.
            call cor_yaml%read("f", silent=no_f, val=this%coriolis_f, default="0.0")
            if (no_f) then
               call cor_yaml%read("latitude", silent=no_lat, val=lat, default="0.0")
               if (no_lat) then
                  call env%log%exit_on_error( &
                     "grid/coriolis: f or latitude required")
               end if
               this%coriolis_f = PI*sin(lat*PI/180.0_SP)/21600.0_SP
            end if
         end if
      end if

      sub_env = get_sub_env(env, "physics", is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      disp_yaml = sub_env%yaml%cast_dictionary("dispersion", no_disp)
      if (.not. no_disp) then
         call disp_yaml%read_enum("scheme", DISPERSION_SCHEMES, val=scheme, &
                                  default=DEF_PHYSICS_DISPERSION_SCHEME)
         select case (trim(scheme))
         case ("fully_nonlinear")
            ! declaration defaults already 1, 1, 1
         case ("weakly_nonlinear")
            this%Gamma3 = 0.0_SP
         case ("linear")
            this%Gamma2 = 0.0_SP
         case ("nswe")
            this%Gamma1 = 0.0_SP
            this%Gamma2 = 0.0_SP
            this%dispersion = .false.
         end select
         ! explicit coefficient overrides on top of the scheme preset.
         ! NOTE: yaml read val is intent(out) — a silent-miss WIPES the
         ! passed component (the slope_cap->0 trap), so read into a temp
         ! and assign only when the key is present
         call disp_yaml%read("gamma1", silent=no_key, val=g_tmp)
         if (.not. no_key) this%Gamma1 = g_tmp
         call disp_yaml%read("gamma2", silent=no_key, val=g_tmp)
         if (.not. no_key) this%Gamma2 = g_tmp
         call disp_yaml%read("gamma3", silent=no_key, val=g_tmp)
         if (.not. no_key) this%Gamma3 = g_tmp
         call disp_yaml%read("beta_ref", silent=no_key, val=this%Beta_ref, &
                             default=DEF_PHYSICS_DISPERSION_BETA_REF)
         call disp_yaml%read("swe_eta_dep", silent=no_key, val=this%SWE_ETA_DEP, &
                             default=DEF_PHYSICS_DISPERSION_SWE_ETA_DEP)
         call disp_yaml%read("swe_eta_ramp", silent=no_key, val=this%SWE_ETA_RAMP, &
                             default=DEF_PHYSICS_DISPERSION_SWE_ETA_RAMP)
      end if

   end subroutine physics_read_input

end module model_physics_mod
