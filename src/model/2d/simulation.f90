!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Simulation parameters YAML reader
!
!  YAML block: simulation:
!    title: "..."
!    total_time: <time>            required
!    t_start: <time>              optional, default 0
!    output_interval: <time>      required
!    screen_interval: <time>      optional, default total_time
!    plot_intv_station: <time>    station output interval (s), default 1.0  (pending deprecation)
!    station_output_buffer: <int> station buffer size,         default 1000 (pending deprecation)
!    time_stepping:
!      fixed_dt: <bool>           optional, default false
!      dt: <time>                 required if fixed_dt: true
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_simulation_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_SIMULATION_PLOT_INTV_STATION, &
                                          DEF_SIMULATION_SCREEN_INTERVAL, &
                                          DEF_SIMULATION_STATION_OUTPUT_BUFFER, &
                                          DEF_SIMULATION_T_START

   implicit none

   private
   public :: type_model_simulation

   type, extends(type_model_base) :: type_model_simulation

      character(:), allocatable :: title
      real(SP) :: total_time = 0.0_SP
      real(SP) :: t_start = 0.0_SP
      real(SP) :: plot_intv = 0.0_SP
      real(SP) :: screen_interval = 0.0_SP
      real(SP) :: plot_intv_station    = 1.0_SP
      integer  :: station_output_buffer = 1000
      logical  :: fixed_dt = .false.
      real(SP) :: dt_fixed = 0.0_SP

   contains
      procedure :: read_input => simulation_read_input
   end type type_model_simulation

contains

   subroutine simulation_read_input(this, env)
      class(type_model_simulation), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: ts_yaml
      logical :: no_ts, no_title, no_tstart, no_screen

      sub_env = get_sub_env(env, 'simulation')
      this%is_activated = .true.

      call sub_env%yaml%read('title', silent=no_title, val=this%title)
      call sub_env%yaml%read_positive('total_time', val=this%total_time)
      call sub_env%yaml%read('t_start', silent=no_tstart, val=this%t_start, default=DEF_SIMULATION_T_START)
      call sub_env%yaml%read_positive('output_interval', val=this%plot_intv)
      call sub_env%yaml%read('screen_interval', silent=no_screen, &
                              val=this%screen_interval, default=DEF_SIMULATION_SCREEN_INTERVAL)
      call sub_env%yaml%read('plot_intv_station',     silent=no_ts, &
                              val=this%plot_intv_station,    default=DEF_SIMULATION_PLOT_INTV_STATION)
      call sub_env%yaml%read('station_output_buffer', silent=no_ts, &
                              val=this%station_output_buffer, default=DEF_SIMULATION_STATION_OUTPUT_BUFFER)

      ! Time stepping sub-block (optional)
      ts_yaml = sub_env%yaml%cast_dictionary('time_stepping', no_ts)
      if (.not. no_ts) then
         call ts_yaml%read('fixed_dt', val=this%fixed_dt, default='NO')
         if (this%fixed_dt) then
            call ts_yaml%read_positive('dt', val=this%dt_fixed)
         end if
      end if

   end subroutine simulation_read_input

end module model_simulation_mod
