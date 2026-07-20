!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Bottom friction parameters YAML reader and physics compute.
!
!  YAML block: friction:       (top-level; omit for zero drag)
!    cd:      <real>   constant drag coefficient
!    manning: <real>   Manning n itself; Cd = g*n²/H^(1/3) each timestep
!    file:    <path>   spatially varying Cd map (nee IN_Cd + CD_FILE);
!                      init-gated PENDING -- the map read was never implemented
!
!  Exactly one of cd | manning | file (exclusive value keys; kills the old
!  dual-use Cd and the manning/friction_matrix bools).  With manning, call
!  update_cd(h, min_depth_frc) each timestep before cal_sources; cal_sources
!  always uses Cd as a plain linear drag, so the Manning formula is invisible
!  to the kernel.
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

      ! cd_base: time-constant drag base, ghost-inclusive.  Holds the constant
      ! drag coefficient (manning=NO) plus any sponge friction merged in by
      ! sponge%merge_friction; Manning mode carries a roughness in Cd_fixed
      ! (not a drag), so its constant part is only the merged sponge term.
      real(SP), allocatable :: cd_base(:, :)

      ! Ghost-inclusive effective drag cal_sources reads: (local_nx+2*N_GHOST,
      ! local_ny+2*N_GHOST).  Starts at cd_base; when manning=YES, update_cd
      ! rebuilds it as cd_base + g*n²/H^(1/3) each timestep.
      real(SP), allocatable :: Cd(:, :)

   contains
      procedure :: read_input => friction_read_input
      procedure :: init_compute => friction_init_compute
      procedure :: sync_base => friction_sync_base
      procedure :: update_cd => friction_update_cd
      procedure :: free => friction_free
   end type type_model_friction

contains

   subroutine friction_read_input(this, env)
      class(type_model_friction), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_fr, no_cd, no_mn, no_file
      real(SP) :: tmp_r
      integer :: n_keys

      sub_env = get_sub_env(env, "friction", is_empty=no_fr)
      this%is_activated = .not. no_fr
      if (.not. this%is_activated) return

      call sub_env%yaml%read("cd", silent=no_cd, val=tmp_r)
      if (.not. no_cd) this%Cd_fixed = tmp_r
      call sub_env%yaml%read("manning", silent=no_mn, val=tmp_r)
      if (.not. no_mn) then
         this%Cd_fixed = tmp_r
         this%manning = .true.
      end if
      call sub_env%yaml%read_input_path("file", silent=no_file, val=this%cd_file)
      this%no_cd_file = no_file
      this%friction_matrix = .not. no_file

      n_keys = count([.not. no_cd,.not. no_mn,.not. no_file])
      if (n_keys /= 1) then
         call env%log%exit_on_error( &
            "friction: exactly one of cd | manning | file is required")
      end if

   end subroutine friction_read_input

   subroutine friction_init_compute(this, grid)
      class(type_model_friction), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid

      integer :: ng, mloc_g, nloc_g

      call this%free()

      ! FUTURE: read the spatially varying map from cd_file; gated until wired
      if (this%friction_matrix) then
         error stop "friction: file (spatially varying Cd) is pending -- the map read is not implemented"
      end if

      ng = N_GHOST
      mloc_g = grid%local_nx + 2*ng
      nloc_g = grid%local_ny + 2*ng

      ! Constant drag base: the plain Cd (manning=NO) or zero (manning carries
      ! a roughness in Cd_fixed, not a drag).  sponge%merge_friction adds its
      ! contribution here; sync_base then pushes it into the effective Cd.
      if (this%manning) then
         allocate (this%cd_base(mloc_g, nloc_g), source=0.0_SP)
      else
         allocate (this%cd_base(mloc_g, nloc_g), source=this%Cd_fixed)
      end if

      ! Effective drag; Cd is always allocated so cal_sources can take it
      ! unconditionally (base defaults 0 => zero drag).
      allocate (this%Cd(mloc_g, nloc_g), source=this%cd_base)

   end subroutine friction_init_compute

   ! Push the constant drag base into the effective Cd.  Call after any
   ! sponge%merge_friction so the merged sponge drag reaches cal_sources on
   ! the non-Manning path (update_cd rebuilds Cd from the base otherwise).
   subroutine friction_sync_base(this)
      class(type_model_friction), intent(inout) :: this
      if (allocated(this%Cd) .and. allocated(this%cd_base)) this%Cd = this%cd_base
   end subroutine friction_sync_base

   ! Recompute effective drag as the constant base plus the Manning term from
   ! the current total depth H.  No-op when manning=.false. (Cd already holds
   ! cd_base via sync_base) or friction not active.
   ! Call once per timestep after update_h, before cal_sources.
   subroutine friction_update_cd(this, h, min_depth_frc)
      class(type_model_friction), intent(inout) :: this
      real(SP), intent(in) :: h(:, :)
      real(SP), intent(in) :: min_depth_frc

      integer :: i, j

      if (.not. this%is_activated .or. .not. this%manning) return

      ! FUTURE: spatially varying Manning n (friction_matrix=YES) needs a
      ! separate n_raw(:,:) array populated from cd_file in init_compute.
      do j = 1, size(this%Cd, 2)
         do i = 1, size(this%Cd, 1)
            this%Cd(i, j) = this%cd_base(i, j) + GRAV*this%Cd_fixed**2 &
                            /max(h(i, j), min_depth_frc)**(0.333333_SP)
         end do
      end do

   end subroutine friction_update_cd

   subroutine friction_free(this)
      class(type_model_friction), intent(inout) :: this
      if (allocated(this%Cd)) deallocate (this%Cd)
      if (allocated(this%cd_base)) deallocate (this%cd_base)
   end subroutine friction_free

end module model_friction_mod
