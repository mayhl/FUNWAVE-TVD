!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Bottom friction parameters YAML reader and physics compute.
!
!  YAML block: friction:       (top-level; omit for no friction)
!    friction_matrix: <bool>   use spatially varying Cd file, default NO
!    friction_file:   <path>   required when friction_matrix: YES
!    manning:         <bool>   Manning roughness formula, default NO
!    Cd:              <real>   constant drag coefficient (or Manning n), default 0.0
!
!  When manning: YES, Cd holds Manning n.  Call update_cd(h, min_depth_frc)
!  each timestep to overwrite Cd with g*n²/H^(1/3) before cal_sources.
!  cal_sources always uses Cd as a plain linear drag; Manning formula is
!  invisible to the kernel.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam  (read_input)
!    06/01/2026  Michael-Angelo Y.H. Lam  (init_compute, free)
!    06/02/2026  Michael-Angelo Y.H. Lam  (manning flag, update_cd)
!
!-------------------------------------------------

module model_friction_mod
   use core_constants_mod, only: SP, N_GHOST, GRAV
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_FRICTION_CD, DEF_FRICTION_FRICTION_MATRIX, &
                                        DEF_FRICTION_MANNING

   implicit none

   private
   public :: type_model_friction

   type, extends(type_model_base) :: type_model_friction

      logical  :: friction_matrix = .false.
      logical  :: no_cd_file = .true.
      logical  :: manning = .false.
      type(type_path) :: cd_file

      ! Cd_fixed: constant Manning n (manning=YES) or drag coefficient (manning=NO).
      real(SP) :: Cd_fixed = 0.0_SP

      ! Ghost-inclusive drag coefficient: (local_nx+2*N_GHOST, local_ny+2*N_GHOST).
      ! Allocated by init_compute; nil when friction is not active.
      ! When manning=YES, update_cd overwrites this with g*n²/H^(1/3) each timestep.
      real(SP), allocatable :: Cd(:, :)

   contains
      procedure :: read_input => friction_read_input
      procedure :: init_compute => friction_init_compute
      procedure :: update_cd => friction_update_cd
      procedure :: free => friction_free
   end type type_model_friction

contains

   subroutine friction_read_input(this, env)
      class(type_model_friction), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_fr, no_key

      sub_env = get_sub_env(env, "friction", is_empty=no_fr)
      this%is_activated = .not. no_fr
      if (.not. this%is_activated) return

      call sub_env%yaml%read("friction_matrix", val=this%friction_matrix, default=DEF_FRICTION_FRICTION_MATRIX)
      call sub_env%yaml%read_input_path("friction_file", silent=this%no_cd_file, val=this%cd_file)
      call sub_env%yaml%read("manning", silent=no_key, val=this%manning, default=DEF_FRICTION_MANNING)
      call sub_env%yaml%read("Cd", silent=no_key, val=this%Cd_fixed, default=DEF_FRICTION_CD)

   end subroutine friction_read_input

   subroutine friction_init_compute(this, grid)
      class(type_model_friction), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid

      integer :: ng, mloc_g, nloc_g

      call this%free()

      ng = N_GHOST
      mloc_g = grid%local_nx + 2*ng
      nloc_g = grid%local_ny + 2*ng

      ! No friction: block => zero drag; Cd is always allocated so
      ! cal_sources can take it unconditionally (Cd_fixed defaults 0).
      allocate (this%Cd(mloc_g, nloc_g), source=this%Cd_fixed)

      ! TODO: overwrite with spatially varying values read from this%cd_file
      ! when this%friction_matrix is true.

   end subroutine friction_init_compute

   ! Recompute effective drag from Manning n and current total depth H.
   ! No-op when manning=.false. or friction not active.
   ! Call once per timestep after update_h, before cal_sources.
   subroutine friction_update_cd(this, h, min_depth_frc)
      class(type_model_friction), intent(inout) :: this
      real(SP), intent(in) :: h(:, :)
      real(SP), intent(in) :: min_depth_frc

      integer :: i, j

      if (.not. this%is_activated .or. .not. this%manning) return

      ! TODO: spatially varying Manning n (friction_matrix=YES) needs a
      ! separate n_raw(:,:) array populated from cd_file in init_compute.
      do j = 1, size(this%Cd, 2)
         do i = 1, size(this%Cd, 1)
            this%Cd(i, j) = GRAV*this%Cd_fixed**2 &
                            /max(h(i, j), min_depth_frc)**(0.333333_SP)
         end do
      end do

   end subroutine friction_update_cd

   subroutine friction_free(this)
      class(type_model_friction), intent(inout) :: this
      if (allocated(this%Cd)) deallocate (this%Cd)
   end subroutine friction_free

end module model_friction_mod
