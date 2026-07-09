!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D model configuration aggregator
!
!  Orchestrates all 3D model component readers.
!  Analogous to type_model_main in src/model/2d/main.f90.
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_config_mod

   use core_env_mod,            only: type_env, new_env, get_sub_env

   use model_3d_geometry_mod,   only: type_model_3d_geometry
   use model_3d_simulation_mod, only: type_model_3d_simulation
   use model_3d_physics_mod,    only: type_model_3d_physics
   use model_3d_turbulence_mod, only: type_model_3d_turbulence
   use model_3d_solver_mod,     only: type_model_3d_solver
   use model_3d_wavemaker_mod,  only: type_model_3d_wavemaker
   use model_3d_bc_mod,         only: type_model_3d_bc
   use model_3d_sponge_mod,     only: type_model_3d_sponge
   use model_3d_hotstart_mod,   only: type_model_3d_hotstart
   use model_3d_baroclinic_mod, only: type_model_3d_baroclinic
   use model_3d_output_mod,     only: type_model_3d_output
   use model_coupling_mod,      only: type_model_coupling

   implicit none

   type, public :: type_model_3d_config
      type(type_env) :: env

      type(type_model_3d_geometry)   :: geometry
      type(type_model_3d_simulation) :: simulation
      type(type_model_3d_physics)    :: physics
      type(type_model_3d_turbulence) :: turbulence
      type(type_model_3d_solver)     :: solver
      type(type_model_3d_wavemaker)  :: wavemaker
      type(type_model_3d_bc)         :: bc
      type(type_model_3d_sponge)     :: sponge
      type(type_model_3d_hotstart)   :: hotstart
      type(type_model_3d_baroclinic) :: baroclinic
      type(type_model_3d_output)     :: output
      type(type_model_coupling)      :: coupling

# if defined (SEDIMENT)
      character(80) :: sed_type   = ""
      character(80) :: sed_load   = ""
      logical  :: couple_fs       = .false.
      real     :: sd50            = 0.0
      real     :: shields_c       = 0.0
      real     :: af              = 0.0
      real     :: tau_ce          = 0.0
      real     :: tau_cd          = 0.0
      real     :: erate           = 0.0
      real     :: mud_visc        = 0.0
      real     :: tim_sedi        = 0.0
      logical  :: bed_change      = .false.
# endif

# if defined (OBSTACLE)
      character(80) :: mask3d_file = ""
# endif

   contains
      procedure :: init => config_3d_init
   end type type_model_3d_config

contains

   subroutine config_3d_init(this)
      class(type_model_3d_config), intent(inout) :: this

      character(2048) :: yaml_path
      type(type_env)  :: sed_env, obs_env
      logical :: no_key, obs_empty

      call getarg(1, yaml_path)
      call new_env(this%env, label="3d_config", yaml_path=trim(yaml_path), log_path="LOG.txt")

      call this%geometry%read_input(this%env)
      call this%simulation%read_input(this%env)
      call this%physics%read_input(this%env)
      call this%turbulence%read_input(this%env)
      call this%solver%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%bc%read_input(this%env)
      call this%sponge%read_input(this%env)
      call this%hotstart%read_input(this%env)
      call this%baroclinic%read_input(this%env)
      call this%output%read_input(this%env)
      call this%coupling%read_input(this%env)

# if defined (SEDIMENT)
      sed_env = get_sub_env(this%env, "sediment")
      call sed_env%yaml%read("sed_type",   silent=no_key, val=this%sed_type,   default="")
      call sed_env%yaml%read("sed_load",   silent=no_key, val=this%sed_load,   default="")
      call sed_env%yaml%read("couple_fs",  silent=no_key, val=this%couple_fs,  default="NO")
      call sed_env%yaml%read("d50",        silent=no_key, val=this%sd50,       default="0.0")
      call sed_env%yaml%read("shields_c",  silent=no_key, val=this%shields_c,  default="0.0")
      call sed_env%yaml%read("af",         silent=no_key, val=this%af,         default="0.0")
      call sed_env%yaml%read("tau_ce",     silent=no_key, val=this%tau_ce,     default="0.0")
      call sed_env%yaml%read("tau_cd",     silent=no_key, val=this%tau_cd,     default="0.0")
      call sed_env%yaml%read("erate",      silent=no_key, val=this%erate,      default="0.0")
      call sed_env%yaml%read("mud_visc",   silent=no_key, val=this%mud_visc,   default="0.0")
      call sed_env%yaml%read("tim_sedi",   silent=no_key, val=this%tim_sedi,   default="0.0")
      call sed_env%yaml%read("bed_change", silent=no_key, val=this%bed_change, default="NO")
# endif

# if defined (OBSTACLE)
      obs_env = get_sub_env(this%env, "obstacle", obs_empty)
      if (.not. obs_empty) then
         call obs_env%yaml%read("mask3d_file", silent=no_key, val=this%mask3d_file, default="")
      end if
# endif

      call this%env%yaml%finalize()

   end subroutine config_3d_init

end module model_3d_config_mod
