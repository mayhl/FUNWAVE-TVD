!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Obstacle and breakwater parameters YAML reader
!
!  YAML block: obstacle:       (top-level; omit for no obstacle or breakwater)
!    obstacle_file:         <path>   optional; presence enables obstacle
!    breakwater_file:       <path>   optional; presence enables breakwater
!    BreakWaterAbsorbCoef:  <real>   default 10.0
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_obstacle_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_OBSTACLE_BREAKWATERABSORBCOEF

   implicit none

   private
   public :: type_model_obstacle

   type, extends(type_model_base) :: type_model_obstacle

      type(type_path) :: obstacle_file
      type(type_path) :: breakwater_file

      logical  :: obstacle = .false.
      logical  :: breakwater = .false.

      real(SP) :: BreakWaterAbsorbCoef = 10.0_SP

      ! Friction-type breakwater drag (legacy CD_breakwater), local
      ! ghost-inclusive window; allocated by init_compute when active
      real(SP), allocatable :: cd_breakwater(:, :)

   contains
      procedure :: read_input => obstacle_read_input
      procedure :: init_compute => obstacle_init_compute
   end type type_model_obstacle

contains

   subroutine obstacle_read_input(this, env)
      class(type_model_obstacle), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_obs, no_bw, no_key

      sub_env = get_sub_env(env, "obstacle", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (.not. this%is_activated) return

      call sub_env%yaml%read_input_path("obstacle_file", silent=no_obs, val=this%obstacle_file)
      call sub_env%yaml%read_input_path("breakwater_file", silent=no_bw, val=this%breakwater_file)

      this%obstacle = .not. no_obs
      this%breakwater = .not. no_bw

      call sub_env%yaml%read("BreakWaterAbsorbCoef", silent=no_key, &
                             val=this%BreakWaterAbsorbCoef, default=DEF_OBSTACLE_BREAKWATERABSORBCOEF)

   end subroutine obstacle_read_input

   ! ----------------------------------------------------------------
   ! Breakwater drag map (legacy sponge.F CALCULATE_CD_BREAKWATER):
   ! every cell with a positive width W spreads
   !   $$ c_d(r) = C_{abs}\,\tanh\!\frac{W - r}{0.3\,W}, \quad r \le W $$
   ! over its neighbourhood, keeping the pointwise max.  Legacy reads
   ! the GLOBAL width field on rank 0, computes there, and scatters
   ! ghost-inclusive windows; every rank redoing the identical global
   ! compute and slicing its own window is bitwise the same and
   ! MPI-free (init-time only, like read_field_ascii).
   ! Bug-for-bug notes vs legacy:
   !   1. NOTE: search radius Iwidth = INT(W/dx) truncates, but the
   !      keep test is ri <= W — cells at the clipped corners inside
   !      radius W but outside the index box are missed exactly alike
   !   2. NOTE: width ghosts replicate edges (y walls over interior
   !      columns first, then x walls over all rows), so a breakwater
   !      touching the boundary bleeds its drag into the ghost ring
   ! ----------------------------------------------------------------
   subroutine obstacle_init_compute(this, grid, dx, dy, env)
      class(type_model_obstacle), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: dx, dy
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: width(:, :), cd_glob(:, :)
      real(SP) :: ri, tmp_2d
      integer :: i, j, ib, jb, iwidth, jwidth
      integer :: mp, np, unit
      logical :: exists

      if (.not. this%breakwater) return

      associate (m => grid%M, n => grid%N, ng => N_GHOST)
         mp = m + 2*ng
         np = n + 2*ng

         inquire (file=this%breakwater_file%root, exist=exists)
         if (.not. exists) then
            call env%log%exit_on_error( &
               "obstacle: cannot find "//this%breakwater_file%root)
         end if

         allocate (width(mp, np), source=0.0_SP)
         open (newunit=unit, file=this%breakwater_file%root, &
               status="old", action="read")
         do j = ng + 1, n + ng
            read (unit, *) (width(i, j), i=ng + 1, m + ng)
         end do
         close (unit)

         ! ghost replication, legacy order (corners land on edge values
         ! via the second loop)
         do i = ng + 1, m + ng
            do j = 1, ng
               width(i, j) = width(i, ng + 1)
            end do
            do j = n + ng + 1, np
               width(i, j) = width(i, n + ng)
            end do
         end do
         do j = 1, np
            do i = 1, ng
               width(i, j) = width(ng + 1, j)
            end do
            do i = m + ng + 1, mp
               width(i, j) = width(m + ng, j)
            end do
         end do

         allocate (cd_glob(mp, np), source=0.0_SP)
         do j = 1, np
            do i = 1, mp
               if (width(i, j) > 0.0_SP) then
                  iwidth = int(width(i, j)/dx)
                  jwidth = int(width(i, j)/dy)
                  do jb = max(1, j - jwidth), min(np, j + jwidth)
                     do ib = max(1, i - iwidth), min(mp, i + iwidth)
                        ri = sqrt(((ib - i)*dx)**2 + ((jb - j)*dy)**2)
                        if (ri <= width(i, j)) then
                           tmp_2d = this%BreakWaterAbsorbCoef* &
                                    tanh((width(i, j) - ri)/(0.3_SP*width(i, j)))
                           if (tmp_2d > cd_glob(ib, jb)) cd_glob(ib, jb) = tmp_2d
                        end if
                     end do
                  end do
               end if
            end do
         end do

         ! this rank's ghost-inclusive window (legacy ykchoi scatter)
         associate (lp => grid%lp)
            allocate (this%cd_breakwater(lp%mloc, lp%nloc))
            do j = 1, lp%nloc
               do i = 1, lp%mloc
                  this%cd_breakwater(i, j) = &
                     cd_glob(grid%ibegin - 1 + i, grid%jbegin - 1 + j)
               end do
            end do
         end associate
      end associate

   end subroutine obstacle_init_compute

end module model_obstacle_mod
