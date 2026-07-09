!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Numerics parameters YAML reader
!
!  YAML block: numerics:
!    Time_Scheme:  <string>   'Runge_Kutta' | 'Predictor_Corrector',  default Runge_Kutta
!    CONSTRUCTION: <string>   'HLLC' | 'HLL' | ...,                   default HLLC
!    HIGH_ORDER:   <string>   'FOURTH' | 'SECOND' | ...,              default FOURTH
!    CFL:          <real>     CFL number,                              default 0.5
!    FroudeCap:    <real>     maximum Froude number,                   default 3.0
!    MinDepth:     <real>     minimum wet depth (m),                   default 0.1
!    MinDepthFrc:  <real>     minimum depth for friction (m),          default 0.1
!    OUT_Time:     <bool>     record wave arrival time,                default NO
!    ArrTimeMinH:  <real>     wave height threshold for arrival (m),   default 0.001
!
!  Note: fixed_dt / dt live in simulation: > time_stepping:.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_numerics_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_NUMERICS_ARRTIMEMINH, DEF_NUMERICS_CFL, &
                                        DEF_NUMERICS_CONSTRUCTION, DEF_NUMERICS_FROUDECAP, &
                                        DEF_NUMERICS_HIGH_ORDER, DEF_NUMERICS_MINDEPTH, &
                                        DEF_NUMERICS_MINDEPTHFRC, DEF_NUMERICS_OUT_TIME, &
                                        DEF_NUMERICS_TIME_SCHEME

   implicit none

   private
   public :: type_model_numerics

   type, extends(type_model_base) :: type_model_numerics

      character(:), allocatable :: Time_Scheme
      character(:), allocatable :: construction   ! YAML key: CONSTRUCTION → CONSTR
      character(:), allocatable :: high_order     ! YAML key: HIGH_ORDER

      real(SP) :: CFL = 0.5_SP
      real(SP) :: FroudeCap = 3.0_SP
      real(SP) :: MinDepth = 0.1_SP
      real(SP) :: MinDepthFrc = 0.1_SP

      logical  :: OUT_Time = .false.
      real(SP) :: ArrTimeMin = 0.001_SP

   contains
      procedure :: read_input => numerics_read_input
      procedure :: estimate_dt => numerics_estimate_dt
   end type type_model_numerics

contains

   subroutine numerics_read_input(this, env)
      class(type_model_numerics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_num, no_key

      ! Set string defaults before possible early return so io.F always gets valid values
      this%Time_Scheme = "Runge_Kutta"
      this%construction = "HLLC"
      this%high_order = "FOURTH"

      sub_env = get_sub_env(env, "numerics", is_empty=no_num)
      this%is_activated = .not. no_num
      if (.not. this%is_activated) return

      call sub_env%yaml%read("Time_Scheme", val=this%Time_Scheme, default=DEF_NUMERICS_TIME_SCHEME)
      call sub_env%yaml%read("CONSTRUCTION", val=this%construction, default=DEF_NUMERICS_CONSTRUCTION)
      call sub_env%yaml%read("HIGH_ORDER", val=this%high_order, default=DEF_NUMERICS_HIGH_ORDER)

      call sub_env%yaml%read("CFL", silent=no_key, val=this%CFL, default=DEF_NUMERICS_CFL)
      call sub_env%yaml%read("FroudeCap", silent=no_key, val=this%FroudeCap, default=DEF_NUMERICS_FROUDECAP)
      call sub_env%yaml%read("MinDepth", silent=no_key, val=this%MinDepth, default=DEF_NUMERICS_MINDEPTH)
      call sub_env%yaml%read("MinDepthFrc", silent=no_key, val=this%MinDepthFrc, default=DEF_NUMERICS_MINDEPTHFRC)

      call sub_env%yaml%read("OUT_Time", val=this%OUT_Time, default=DEF_NUMERICS_OUT_TIME)
      call sub_env%yaml%read("ArrTimeMinH", silent=no_key, val=this%ArrTimeMin, default=DEF_NUMERICS_ARRTIMEMINH)

   end subroutine numerics_read_input

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
   subroutine numerics_estimate_dt(this, grid, u, v, h, fixed_dt, dt_fixed, dt)
      use core_constants_mod, only: GRAV, SMALL, LARGE, MPI_SP
      use core_grid_mod, only: type_grid_2d
      use mpi_f08, only: MPI_Allreduce, MPI_MIN, MPI_IN_PLACE
      class(type_model_numerics), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: u(:, :), v(:, :), h(:, :)
      logical, intent(in) :: fixed_dt
      real(SP), intent(in) :: dt_fixed
      real(SP), intent(out) :: dt

      real(SP) :: dt_min, celerity, speed, dt_cfl
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

      call MPI_Allreduce(MPI_IN_PLACE, dt_min, 1, MPI_SP, MPI_MIN, grid%cart_comm, ierr)
      dt_cfl = this%CFL*dt_min

      if (fixed_dt) then
         dt = dt_fixed
         do while (dt > dt_cfl)
            dt = dt/2.0_SP
         end do
      else
         dt = dt_cfl
      end if

   end subroutine numerics_estimate_dt

end module model_numerics_mod
