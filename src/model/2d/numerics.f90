!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Numerics parameters YAML reader
!
!  YAML block: numerics:
!    cfl:            <real>    CFL number,                             default 0.5
!    dt:             <time>    presence = fixed timestep (s); absent = adaptive
!                              CFL stepping.  Exclusive with an explicit cfl:
!                              the default cfl still caps a fixed dt by halving
!                              (nee simulation: time_stepping: fixed_dt/dt)
!    flux_solver:    <string>  'hllc' | 'hll',                         default hllc
!    froude_cap:     <real>    maximum Froude number,                  default 3.0
!    min_depth:      <real>    wet/dry + friction floor (m),           default 0.1
!    reconstruction: <string>  'fourth' | 'fminmod' | 'weno' | 'mlp' | 'basic',
!                              default fourth
!    tridiag:                  system-tuned solver knobs, bitwise-neutral:
!      chunk:            <int> pipelined-sweep chunk width,            default 48
!      transpose_min_py: <int> all-to-all y-solve rank threshold,      default 40
!
!  Enum values are lowercase in YAML and upcased here for the prefix
!  dispatch in kernel_fluxes.  min_depth is a single floor: legacy folded
!  MinDepth/MinDepthFrc to their minimum (old io.F), so the pair was one
!  value in practice.
!
!  Note: the arrival time map moved to output: > arrival_time: (it is an
!  output product).
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_numerics_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_NUMERICS_CFL, DEF_NUMERICS_FLUX_SOLVER, &
                                        DEF_NUMERICS_FROUDE_CAP, DEF_NUMERICS_MIN_DEPTH, &
                                        DEF_NUMERICS_RECONSTRUCTION, &
                                        DEF_NUMERICS_TRIDIAG_CHUNK, &
                                        DEF_NUMERICS_TRIDIAG_TRANSPOSE_MIN_PY

   implicit none

   private
   public :: type_model_numerics

   type, extends(type_model_base) :: type_model_numerics

      character(:), allocatable :: construction   ! YAML key: flux_solver
      character(:), allocatable :: high_order     ! YAML key: reconstruction

      real(SP) :: CFL = 0.5_SP
      ! dt: presence derives fixed-step mode (nee simulation time_stepping)
      logical  :: fixed_dt = .false.
      real(SP) :: dt_fixed = 0.0_SP
      real(SP) :: FroudeCap = 3.0_SP
      real(SP) :: MinDepth = 0.1_SP
      real(SP) :: MinDepthFrc = 0.1_SP   ! kept == MinDepth (single min_depth key)

   contains
      procedure :: read_input => numerics_read_input
      procedure :: estimate_dt => numerics_estimate_dt
   end type type_model_numerics

contains

   subroutine numerics_read_input(this, env)
      use core_yaml_file_mod, only: type_yaml_reader
      use core_solver_tridiag_mod, only: trid_configure
      class(type_model_numerics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: tri_yaml
      integer :: tri_chunk, tri_min_py
      character(96) :: msg
      logical :: no_num, no_key, no_tri, no_cfl, no_dt

      ! Set string defaults before possible early return so consumers always get valid values
      this%construction = "HLLC"
      this%high_order = "FOURTH"

      sub_env = get_sub_env(env, "numerics", is_empty=no_num)
      this%is_activated = .not. no_num
      if (.not. this%is_activated) return

      call sub_env%yaml%read("flux_solver", val=this%construction, default=DEF_NUMERICS_FLUX_SOLVER)
      call sub_env%yaml%read("reconstruction", val=this%high_order, default=DEF_NUMERICS_RECONSTRUCTION)
      ! kernel_fluxes dispatches on uppercase prefixes
      this%construction = upcase(this%construction)
      this%high_order = upcase(this%high_order)

      call sub_env%yaml%read("cfl", silent=no_cfl, val=this%CFL, default=DEF_NUMERICS_CFL)
      ! dt presence = fixed-step mode; an EXPLICIT cfl alongside it is
      ! contradictory config (the default cfl still caps dt by halving)
      call sub_env%yaml%read("dt", silent=no_dt, val=this%dt_fixed)
      this%fixed_dt = .not. no_dt
      if (this%fixed_dt) then
         if (this%dt_fixed <= 0.0_SP) &
            call sub_env%log%exit_on_error("numerics: dt must be > 0")
         if (.not. no_cfl) call sub_env%log%exit_on_error( &
            "numerics: cfl and dt are exclusive -- dt fixes the step"// &
            " (the default cfl caps it); drop one")
      end if
      call sub_env%yaml%read("froude_cap", silent=no_key, val=this%FroudeCap, default=DEF_NUMERICS_FROUDE_CAP)
      ! single wet/dry + friction floor; legacy folded the MinDepth/
      ! MinDepthFrc pair to their minimum so they were one value in practice
      call sub_env%yaml%read("min_depth", silent=no_key, val=this%MinDepth, default=DEF_NUMERICS_MIN_DEPTH)
      this%MinDepthFrc = this%MinDepth

      ! tridiag: system-tuned solver knobs (bitwise-neutral); absent
      ! keys keep the wheat-measured defaults compiled into the solver
      tri_yaml = sub_env%yaml%cast_dictionary("tridiag", no_tri)
      if (.not. no_tri) then
         call tri_yaml%read("chunk", silent=no_key, val=tri_chunk, &
                            default=DEF_NUMERICS_TRIDIAG_CHUNK)
         call tri_yaml%read("transpose_min_py", silent=no_key, val=tri_min_py, &
                            default=DEF_NUMERICS_TRIDIAG_TRANSPOSE_MIN_PY)
         call trid_configure(tri_chunk, tri_min_py)
         write (msg, '(a,i0,a,i0)') "numerics/tridiag: chunk=", tri_chunk, &
            ", transpose_min_py=", tri_min_py
         call sub_env%log%info(trim(msg))
      end if

   end subroutine numerics_read_input

   pure function upcase(s) result(u)
      character(*), intent(in) :: s
      character(len(s)) :: u
      integer :: i, c

      do i = 1, len(s)
         c = iachar(s(i:i))
         if (c >= iachar("a") .and. c <= iachar("z")) c = c - 32
         u(i:i) = achar(c)
      end do
   end function upcase

   ! ----------------------------------------------------------------
   ! Adaptive CFL timestep (legacy ESTIMATE_DT, old/misc.F):
   !   $$ \Delta t = \mathrm{CFL} \cdot \min_{i,j}\left(
   !        \frac{\Delta x}{|u| + c},\ \frac{\Delta y}{|v| + c}\right),
   !      \qquad c = \sqrt{g\,\max(H, d_{frc})} $$
   ! minimised over this grid's ranks (allreduce on cart_comm).
   ! Fixed-dt mode keeps output times commensurate by halving:
   !   $$ \Delta t = \Delta t_{fix} / 2^n, \quad
   !      n = \min\{n \ge 0 : \Delta t \le \Delta t_{CFL}\} $$
   ! The scan covers interior cells only; legacy scans ghosts too, but
   ! mirror/periodic/halo ghosts replicate interior values, so the
   ! global minimum is unchanged.  Unlike legacy, TIME is not advanced
   ! here — the stepper owns time.
   ! ----------------------------------------------------------------
   subroutine numerics_estimate_dt(this, grid, u, v, h, dt)
      use, intrinsic :: iso_fortran_env, only: real64
      use core_constants_mod, only: GRAV, SMALL, LARGE, MPI_SP
      use core_grid_mod, only: type_grid_2d
      use mpi_f08, only: MPI_Allreduce, MPI_MIN, MPI_IN_PLACE, MPI_Wtime
      use core_comm_timers_mod, only: comm_t, comm_n, CT_DT_REDUCE
      class(type_model_numerics), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: u(:, :), v(:, :), h(:, :)
      real(SP), intent(out) :: dt

      real(SP) :: dt_min, celerity, speed, dt_cfl
      real(real64) :: ct0
      integer :: i, j, ierr

      dt_min = LARGE
      associate (lp => grid%lp)
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            celerity = sqrt(GRAV*max(h(i, j), this%MinDepthFrc))
            speed = max(abs(u(i, j)) + celerity, SMALL)
            dt_min = min(dt_min, grid%dx(i - lp%ib + 1, j - lp%jb + 1)/speed)
            speed = max(abs(v(i, j)) + celerity, SMALL)
            dt_min = min(dt_min, grid%dy(i - lp%ib + 1, j - lp%jb + 1)/speed)
         end do
      end do
      end associate

      ct0 = MPI_Wtime()
      call MPI_Allreduce(MPI_IN_PLACE, dt_min, 1, MPI_SP, MPI_MIN, grid%cart_comm, ierr)
      comm_t(CT_DT_REDUCE) = comm_t(CT_DT_REDUCE) + (MPI_Wtime() - ct0)
      comm_n(CT_DT_REDUCE) = comm_n(CT_DT_REDUCE) + 1
      dt_cfl = this%CFL*dt_min

      if (this%fixed_dt) then
         dt = this%dt_fixed
         do while (dt > dt_cfl)
            dt = dt/2.0_SP
         end do
      else
         dt = dt_cfl
      end if

   end subroutine numerics_estimate_dt

end module model_numerics_mod
