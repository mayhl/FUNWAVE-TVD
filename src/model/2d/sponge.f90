!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Sponge layer parameters YAML reader and physics compute.
!
!  Three sponge types (set at most one per run):
!    direct     -- Larsen-Dancy (1983) post-step damping of eta/p/q.
!    friction   -- CDsponge-based momentum drag term in the RHS (sources.f90).
!    diffusion  -- Csp-based lateral viscosity term in the RHS (sources.f90).
!
!  Only the direct sponge has a standalone apply() here.
!  friction/diffusion coefficient arrays are initialised in init_compute() so
!  sources.f90 can reference them; their apply is deferred to that refactor.
!
!  YAML block: sponge:       (top-level; omit for no sponge)
!    diffusion_sponge: <bool>   default NO
!    direct_sponge:    <bool>   default NO
!    friction_sponge:  <bool>   default NO
!    Csp:              <real>   diffusion coefficient,       default 0.1
!    CDsponge:         <real>   friction drag coefficient,   default 5.0
!    Sponge_west_width:  <length>   default 0
!    Sponge_east_width:  <length>   default 0
!    Sponge_south_width: <length>   default 0
!    Sponge_north_width: <length>   default 0
!    R_sponge:  <real>   sponge relaxation rate,  default 0.85
!    A_sponge:  <real>   sponge amplitude factor, default 5.0
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam  (read_input)
!    05/28/2026  Michael-Angelo Y.H. Lam  (init_compute, apply)
!
!-------------------------------------------------

module model_sponge_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_env_mod,       only: type_env, get_sub_env
   use core_grid_mod,      only: type_grid_2d
   use model_base_mod,     only: type_model_base
   use model_fields_2d_mod, only: type_fields_2d

   implicit none

   private
   public :: type_model_sponge

   type, extends(type_model_base) :: type_model_sponge

      ! ── YAML parameters ───────────────────────────────────────────
      logical  :: diffusion_sponge = .false.
      logical  :: direct_sponge    = .false.
      logical  :: friction_sponge  = .false.

      real(SP) :: Csp      = 0.1_SP
      real(SP) :: CDsponge = 5.0_SP

      real(SP) :: Sponge_west_width  = 0.0_SP
      real(SP) :: Sponge_east_width  = 0.0_SP
      real(SP) :: Sponge_south_width = 0.0_SP
      real(SP) :: Sponge_north_width = 0.0_SP

      real(SP) :: R_sponge = 0.85_SP
      real(SP) :: A_sponge = 5.0_SP

      ! ── Computed state ────────────────────────────────────────────
      ! Ghost-inclusive arrays: (local_nx+2*N_GHOST, local_ny+2*N_GHOST).
      ! coeff:     direct sponge damping ratio (>= 1; 1.0 = no damping).
      ! cd_sponge: friction sponge drag coefficient (>= 0).
      ! nu_sponge: diffusion sponge kinematic viscosity (>= 0).
      real(SP), allocatable :: coeff    (:,:)
      real(SP), allocatable :: cd_sponge(:,:)
      real(SP), allocatable :: nu_sponge(:,:)

   contains
      procedure :: read_input   => sponge_read_input
      procedure :: init_compute => sponge_init_compute
      procedure :: apply        => sponge_apply
      procedure :: free         => sponge_free
   end type type_model_sponge

contains

   ! ── YAML ──────────────────────────────────────────────────────────────────

   subroutine sponge_read_input(this, env)
      class(type_model_sponge), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_sp, no_key

      sub_env = get_sub_env(env, 'sponge', is_empty=no_sp)
      this%is_activated = .not. no_sp
      if (.not. this%is_activated) return

      call sub_env%yaml%read('diffusion_sponge', val=this%diffusion_sponge, default='NO')
      call sub_env%yaml%read('direct_sponge',    val=this%direct_sponge,    default='NO')
      call sub_env%yaml%read('friction_sponge',  val=this%friction_sponge,  default='NO')

      call sub_env%yaml%read('Csp',      silent=no_key, val=this%Csp,      default='0.1')
      call sub_env%yaml%read('CDsponge', silent=no_key, val=this%CDsponge, default='5.0')

      call sub_env%yaml%read('Sponge_west_width',  silent=no_key, val=this%Sponge_west_width,  default='0.0')
      call sub_env%yaml%read('Sponge_east_width',  silent=no_key, val=this%Sponge_east_width,  default='0.0')
      call sub_env%yaml%read('Sponge_south_width', silent=no_key, val=this%Sponge_south_width, default='0.0')
      call sub_env%yaml%read('Sponge_north_width', silent=no_key, val=this%Sponge_north_width, default='0.0')

      call sub_env%yaml%read('R_sponge', silent=no_key, val=this%R_sponge, default='0.85')
      call sub_env%yaml%read('A_sponge', silent=no_key, val=this%A_sponge, default='5.0')

   end subroutine sponge_read_input

   ! ── Initialisation ────────────────────────────────────────────────────────

   !> Allocate and compute sponge coefficient arrays.
   !>
   !> Direct sponge (coeff):    Larsen-Dancy exponential damping ratio.
   !> Friction sponge (cd):     tanh-profile drag coefficient, max-folded into cd_inout
   !>                           when provided (eliminates separate cd_sponge allocation).
   !> Diffusion sponge (nu):    tanh-profile lateral viscosity.
   !>
   !> cd_inout: optional ghost-inclusive Cd array from type_model_friction.  When
   !>   present, friction sponge values are max-merged in-place and cd_sponge is NOT
   !>   allocated.  When absent (legacy bridge path), cd_sponge is allocated as before.
   !>
   !> All arrays ghost-inclusive.  Called once at init_compute time, before
   !> the first timestep and after grid%setup() + grid%init_spacing() have run.
   subroutine sponge_init_compute(this, grid, cd_inout)
      class(type_model_sponge), intent(inout)        :: this
      type(type_grid_2d),       intent(in)           :: grid
      real(SP), optional,       intent(inout)        :: cd_inout(:,:)

      integer  :: ng, nx, ny, mloc_g, nloc_g
      real(SP) :: ref_dx, ref_dy

      if (.not. this%is_activated) return

      ng     = N_GHOST
      nx     = grid%local_nx
      ny     = grid%local_ny
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng

      ref_dx = grid%dx0
      ref_dy = grid%dy0

      call this%free()

      if (this%direct_sponge) then
         allocate(this%coeff(mloc_g, nloc_g), source=1.0_SP)
         call compute_direct_coeff(this%coeff, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                   this%Sponge_west_width, this%Sponge_east_width,  &
                                   this%Sponge_south_width, this%Sponge_north_width, &
                                   this%R_sponge, this%A_sponge,                    &
                                   grid%ibegin, grid%iproc, grid%nx_proc, nx,       &
                                   grid%jbegin, grid%jproc, grid%ny_proc, ny)
      end if

      if (this%friction_sponge) then
         if (present(cd_inout)) then
            ! Max-merge sponge profile into the caller's Cd array; no separate allocation.
            call compute_friction_coeff(cd_inout, size(cd_inout,1), size(cd_inout,2), &
                                        ng, ref_dx, ref_dy,                           &
                                        this%Sponge_west_width, this%Sponge_east_width, &
                                        this%Sponge_south_width, this%Sponge_north_width, &
                                        this%CDsponge,                                 &
                                        grid%ibegin, grid%iproc, grid%nx_proc, nx,    &
                                        grid%jbegin, grid%jproc, grid%ny_proc, ny)
         else
            allocate(this%cd_sponge(mloc_g, nloc_g), source=0.0_SP)
            call compute_friction_coeff(this%cd_sponge, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                        this%Sponge_west_width, this%Sponge_east_width,      &
                                        this%Sponge_south_width, this%Sponge_north_width,    &
                                        this%CDsponge,                                       &
                                        grid%ibegin, grid%iproc, grid%nx_proc, nx,           &
                                        grid%jbegin, grid%jproc, grid%ny_proc, ny)
         end if
      end if

      if (this%diffusion_sponge) then
         allocate(this%nu_sponge(mloc_g, nloc_g), source=0.0_SP)
         call compute_diffusion_coeff(this%nu_sponge, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                      this%Sponge_west_width, this%Sponge_east_width,      &
                                      this%Sponge_south_width, this%Sponge_north_width,    &
                                      this%Csp,                                            &
                                      grid%ibegin, grid%iproc, grid%nx_proc, nx,           &
                                      grid%jbegin, grid%jproc, grid%ny_proc, ny)
      end if

   end subroutine sponge_init_compute

   ! ── Apply (direct sponge only) ────────────────────────────────────────────

   !> Apply Larsen-Dancy (1983) post-step damping to the prognostic state.
   !>
   !> Divides eta/p/q by coeff(i,j) at every ghost-inclusive cell.
   !> eta is only damped at wet cells (mask > 0).
   !> p and q are always damped (consistent with FUNWAVE-TVD legacy behaviour).
   !>
   !> Friction and diffusion sponge terms are RHS source contributions;
   !> their apply is in sources.f90 (refactor TODO).
   subroutine sponge_apply(this, fields, grid)
      class(type_model_sponge), intent(in)    :: this
      type(type_fields_2d),     intent(inout) :: fields
      type(type_grid_2d),       intent(in)    :: grid

      integer :: i, j, mloc_g, nloc_g, ng

      if (.not. this%is_activated) return
      if (.not. this%direct_sponge) return

      ng     = N_GHOST
      mloc_g = grid%local_nx + 2*ng
      nloc_g = grid%local_ny + 2*ng

      do j = 1, nloc_g
         do i = 1, mloc_g
            if (fields%mask(i, j) > 0) &
               fields%eta(i, j) = fields%eta(i, j) / this%coeff(i, j)
            fields%p(i, j) = fields%p(i, j) / this%coeff(i, j)
            fields%q(i, j) = fields%q(i, j) / this%coeff(i, j)
         end do
      end do

   end subroutine sponge_apply

   ! ── Teardown ──────────────────────────────────────────────────────────────

   subroutine sponge_free(this)
      class(type_model_sponge), intent(inout) :: this
      if (allocated(this%coeff))     deallocate(this%coeff)
      if (allocated(this%cd_sponge)) deallocate(this%cd_sponge)
      if (allocated(this%nu_sponge)) deallocate(this%nu_sponge)
   end subroutine sponge_free

   ! ── Private coefficient computation ───────────────────────────────────────

   !> Direct (Larsen-Dancy) sponge coefficient.
   !> Exponential profile: coeff >= 1; 1 = no damping.
   !> dim1 = local_nx + 2*N_GHOST, dim2 = local_ny + 2*N_GHOST.
   !> Values below the decay floor A_sponge^(R_sponge^50) are reset to 1.
   !>
   !> Bug-fix vs. legacy: each axis is combined independently so that a west-only
   !> sponge is not silently zeroed by the empty south/north combine pass.
   subroutine compute_direct_coeff(coeff, dim1, dim2, ng, ref_dx, ref_dy, &
                                    w_width, e_width, s_width, n_width,    &
                                    R_sp, A_sp,                            &
                                    ibegin, iproc, nx_proc, local_nx,      &
                                    jbegin, jproc, ny_proc, local_ny)
      real(SP), intent(inout) :: coeff(dim1, dim2)
      integer,  intent(in)    :: dim1, dim2, ng
      real(SP), intent(in)    :: ref_dx, ref_dy
      real(SP), intent(in)    :: w_width, e_width, s_width, n_width
      real(SP), intent(in)    :: R_sp, A_sp
      integer,  intent(in)    :: ibegin, iproc, nx_proc, local_nx
      integer,  intent(in)    :: jbegin, jproc, ny_proc, local_ny

      real(SP), allocatable :: tmp1(:,:), tmp2(:,:)
      real(SP) :: ri, lim, floor_val
      integer  :: i, j, iwidth

      allocate(tmp1(dim1, dim2), tmp2(dim1, dim2))
      floor_val = A_sp**(R_sp**50)

      ! ── west / east — combine only when at least one side is active ──
      if (w_width > 0.0_SP .or. e_width > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (w_width > 0.0_SP) then
            iwidth = int(w_width / ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(coeff(i, j), 1.0_SP)
                  ri  = R_sp**(50.0_SP * real(i + ibegin - 2, SP) / real(iwidth - 1, SP))
                  tmp1(i, j) = max(A_sp**ri, lim)
               end do
            end do
         end if

         if (e_width > 0.0_SP) then
            iwidth = int(e_width / ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(coeff(i, j), 1.0_SP)
                  ri  = R_sp**(50.0_SP * real(dim1 - i + (nx_proc - iproc - 1)*local_nx, SP) &
                                       / real(iwidth - 1, SP))
                  tmp2(i, j) = max(A_sp**ri, lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               coeff(i, j) = max(tmp1(i, j), tmp2(i, j))
               if (coeff(i, j) < floor_val) coeff(i, j) = 1.0_SP
            end do
         end do
      end if

      ! ── south / north — combine only when at least one side is active ──
      if (s_width > 0.0_SP .or. n_width > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (s_width > 0.0_SP) then
            iwidth = int(s_width / ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(coeff(i, j), 1.0_SP)
                  ri  = R_sp**(50.0_SP * real(j + jbegin - 2, SP) / real(iwidth - 1, SP))
                  tmp1(i, j) = max(A_sp**ri, lim)
               end do
            end do
         end if

         if (n_width > 0.0_SP) then
            iwidth = int(n_width / ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(coeff(i, j), 1.0_SP)
                  ri  = R_sp**(50.0_SP * real(dim2 - j + (ny_proc - jproc - 1)*local_ny, SP) &
                                       / real(iwidth - 1, SP))
                  tmp2(i, j) = max(A_sp**ri, lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               coeff(i, j) = max(tmp1(i, j), tmp2(i, j))
               if (coeff(i, j) < floor_val) coeff(i, j) = 1.0_SP
            end do
         end do
      end if

      deallocate(tmp1, tmp2)
   end subroutine compute_direct_coeff

   !> Friction sponge drag coefficient (cd_sponge >= 0).
   !> Linear ramp via tanh profile.  No floor reset — zero outside sponge is correct.
   subroutine compute_friction_coeff(cd, dim1, dim2, ng, ref_dx, ref_dy, &
                                      w_width, e_width, s_width, n_width, &
                                      CDsp,                               &
                                      ibegin, iproc, nx_proc, local_nx,   &
                                      jbegin, jproc, ny_proc, local_ny)
      real(SP), intent(inout) :: cd(dim1, dim2)
      integer,  intent(in)    :: dim1, dim2, ng
      real(SP), intent(in)    :: ref_dx, ref_dy
      real(SP), intent(in)    :: w_width, e_width, s_width, n_width
      real(SP), intent(in)    :: CDsp
      integer,  intent(in)    :: ibegin, iproc, nx_proc, local_nx
      integer,  intent(in)    :: jbegin, jproc, ny_proc, local_ny

      real(SP), allocatable :: tmp1(:,:), tmp2(:,:)
      real(SP) :: ri, lim
      integer  :: i, j, iwidth

      allocate(tmp1(dim1, dim2), tmp2(dim1, dim2))

      if (w_width > 0.0_SP .or. e_width > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (w_width > 0.0_SP) then
            iwidth = int(w_width / ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(cd(i, j), 0.0_SP)
                  ri  = max(0.0_SP, real(iwidth - i - (ibegin - 1), SP))
                  tmp1(i, j) = max(CDsp * tanh(ri / 10.0_SP), lim)
               end do
            end do
         end if

         if (e_width > 0.0_SP) then
            iwidth = int(e_width / ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(cd(i, j), 0.0_SP)
                  ri  = max(0.0_SP, real(iwidth - dim1 + i - (nx_proc - iproc - 1)*local_nx, SP))
                  tmp2(i, j) = max(CDsp * tanh(ri / 10.0_SP), lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               cd(i, j) = max(tmp1(i, j), tmp2(i, j))
            end do
         end do
      end if

      if (s_width > 0.0_SP .or. n_width > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (s_width > 0.0_SP) then
            iwidth = int(s_width / ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(cd(i, j), 0.0_SP)
                  ri  = max(0.0_SP, real(iwidth - j - (jbegin - 1), SP))
                  tmp1(i, j) = max(CDsp * tanh(ri / 10.0_SP), lim)
               end do
            end do
         end if

         if (n_width > 0.0_SP) then
            iwidth = int(n_width / ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(cd(i, j), 0.0_SP)
                  ri  = max(0.0_SP, real(iwidth - dim2 + j - (ny_proc - jproc - 1)*local_ny, SP))
                  tmp2(i, j) = max(CDsp * tanh(ri / 10.0_SP), lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               cd(i, j) = max(tmp1(i, j), tmp2(i, j))
            end do
         end do
      end if

      deallocate(tmp1, tmp2)
   end subroutine compute_friction_coeff

   !> Diffusion sponge lateral viscosity (nu_sponge >= 0).
   !> Same tanh profile as friction sponge, amplitude = Csp.
   subroutine compute_diffusion_coeff(nu, dim1, dim2, ng, ref_dx, ref_dy, &
                                       w_width, e_width, s_width, n_width, &
                                       Csp_val,                            &
                                       ibegin, iproc, nx_proc, local_nx,   &
                                       jbegin, jproc, ny_proc, local_ny)
      real(SP), intent(inout) :: nu(dim1, dim2)
      integer,  intent(in)    :: dim1, dim2, ng
      real(SP), intent(in)    :: ref_dx, ref_dy
      real(SP), intent(in)    :: w_width, e_width, s_width, n_width
      real(SP), intent(in)    :: Csp_val
      integer,  intent(in)    :: ibegin, iproc, nx_proc, local_nx
      integer,  intent(in)    :: jbegin, jproc, ny_proc, local_ny

      call compute_friction_coeff(nu, dim1, dim2, ng, ref_dx, ref_dy, &
                                   w_width, e_width, s_width, n_width, &
                                   Csp_val,                            &
                                   ibegin, iproc, nx_proc, local_nx,   &
                                   jbegin, jproc, ny_proc, local_ny)
   end subroutine compute_diffusion_coeff

end module model_sponge_mod
