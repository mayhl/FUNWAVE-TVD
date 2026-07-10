!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Stepper engine: owns the time-loop *skeleton* only — time control,
!  dt negotiation, the output call site, per-step trace, and the
!  blow-up exit — and delegates all physics to an abstract
!  type_stepper_model (deferred: pre_step, estimate_dt, stage,
!  post_step).  Dispatch is per-step, never per-cell, so the GPU
!  no-polymorphism constraint is untouched; 2D and 3D models reuse
!  this skeleton.
!
!  Loop body (legacy old/legacy_runner.F order):
!    output -> pre_step (save state + ghost exchange) -> estimate_dt
!    -> advance time -> N_RK_STAGES stage() calls -> post_step
!    (mixing/statistics/blow-up check).
!  Time advances between estimate_dt and the stages: legacy
!  ESTIMATE_DT increments TIME internally, so the stages see t + dt.
!  Like legacy, the final state at t >= t_end is NOT written — output
!  runs at the loop top only.
!
!  This module has NO output dependency (libcore_output links
!  libcore_engine): the loop-top output call site is the abstract
!  type_engine_monitor, implemented by the driver around its output
!  manager.
!
!  HISTORY :
!    07/09/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_stepper_engine_mod
   use core_constants_mod, only: SP
   use core_log_io_mod, only: type_log_writer
   use core_simulation_time_mod, only: type_simulation_control
   implicit none

   private
   public :: type_stepper_model, type_engine_monitor, type_stepper_engine
   public :: N_RK_STAGES

   integer, parameter :: N_RK_STAGES = 3

   ! ── Abstract model: the physics side of the loop ────────────────
   type, abstract :: type_stepper_model
   contains
      procedure(i_pre_step), deferred :: pre_step
      procedure(i_estimate_dt), deferred :: estimate_dt
      procedure(i_stage), deferred :: stage
      procedure(i_post_step), deferred :: post_step
   end type type_stepper_model

   ! ── Abstract monitor: loop-top output/diagnostics call site ─────
   type, abstract :: type_engine_monitor
   contains
      procedure(i_monitor_step), deferred :: step
   end type type_engine_monitor

   abstract interface
      ! Save step-start state and refresh ghosts; runs once per step
      ! before dt negotiation.
      subroutine i_pre_step(this)
         import :: type_stepper_model
         class(type_stepper_model), intent(inout) :: this
      end subroutine i_pre_step

      ! Negotiate the step size (global reduction owned by the model).
      subroutine i_estimate_dt(this, dt)
         import :: type_stepper_model, SP
         class(type_stepper_model), intent(inout) :: this
         real(SP), intent(out) :: dt
      end subroutine i_estimate_dt

      ! One RK stage.  time is the already-advanced step target t + dt
      ! (legacy TIME semantics inside the stage loop).
      subroutine i_stage(this, istage, dt, time)
         import :: type_stepper_model, SP
         class(type_stepper_model), intent(inout) :: this
         integer, intent(in) :: istage
         real(SP), intent(in) :: dt, time
      end subroutine i_stage

      ! End-of-step work (statistics, stability check).  blowup=.true.
      ! aborts the run from the engine.
      subroutine i_post_step(this, time, blowup)
         import :: type_stepper_model, SP
         class(type_stepper_model), intent(inout) :: this
         real(SP), intent(in) :: time
         logical, intent(out) :: blowup
      end subroutine i_post_step

      ! Output/diagnostics at the loop top, before the state advances.
      subroutine i_monitor_step(this, t, dt)
         import :: type_engine_monitor, SP
         class(type_engine_monitor), intent(inout) :: this
         real(SP), intent(in) :: t, dt
      end subroutine i_monitor_step
   end interface

   type :: type_stepper_engine
      type(type_simulation_control) :: clock
      real(SP) :: screen_interval = 0.0_SP
   contains
      procedure :: init => engine_init
      procedure :: run => engine_run
   end type type_stepper_engine

contains

   subroutine engine_init(this, t_start, t_end, screen_interval)
      class(type_stepper_engine), intent(inout) :: this
      real(SP), intent(in) :: t_start, t_end
      real(SP), intent(in), optional :: screen_interval

      this%clock%t_start = t_start
      this%clock%t_end = t_end
      this%clock%current_time = t_start
      this%clock%step = 0
      if (present(screen_interval)) this%screen_interval = screen_interval
   end subroutine engine_init

   subroutine engine_run(this, model, monitor, log)
      class(type_stepper_engine), intent(inout) :: this
      class(type_stepper_model), intent(inout) :: model
      class(type_engine_monitor), intent(inout) :: monitor
      type(type_log_writer), intent(inout) :: log

      real(SP) :: dt, t_screen
      integer :: istage
      logical :: blowup
      character(160) :: line

      dt = 0.0_SP
      t_screen = this%clock%current_time

      write (line, "(a,es12.5,a,es12.5)") "stepper engine: t = ", &
         this%clock%current_time, " -> ", this%clock%t_end
      call log%info(trim(line))

      do while (.not. this%clock%is_finished())

         ! legacy loop head: output first, so the IC is frame 1
         call monitor%step(this%clock%current_time, dt)

         call model%pre_step()
         call model%estimate_dt(dt)
         call this%clock%advance(dt)

         do istage = 1, N_RK_STAGES
            call model%stage(istage, dt, this%clock%current_time)
         end do

         call model%post_step(this%clock%current_time, blowup)
         if (blowup) then
            write (line, "(a,es12.5)") "blow-up detected at t = ", &
               this%clock%current_time
            call log%exit_on_error(trim(line))
         end if

         write (line, "(a,i0,a,es12.5,a,es12.5)") "step ", this%clock%step, &
            "  t = ", this%clock%current_time, "  dt = ", dt
         call log%debug(trim(line))

         if (this%screen_interval > 0.0_SP .and. &
             this%clock%current_time >= t_screen) then
            call log%info(trim(line))
            t_screen = t_screen + this%screen_interval
         end if

      end do

   end subroutine engine_run

end module core_stepper_engine_mod
