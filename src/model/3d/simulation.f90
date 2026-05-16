!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D simulation parameters YAML reader
!
!  YAML block: simulation:
!    total_time: <real>          required
!    sim_steps: <int>            default 0
!    plot_start: <real>          default 0.0
!    plot_intv: <real>           required
!    screen_intv: <real>         default 1.0
!    cfl: <real>                 default 0.5
!    time_stepping:
!      dt_ini: <real>            initial dt, default 0.1
!      dt_min: <real>            minimum dt, default 1e-6
!      dt_max: <real>            maximum dt, default 1.0
!    stations:
!      count: <int>              NSTAT, default 0
!      file: <path>              STATIONS_FILE (required when count > 0)
!      interval: <real>          Plot_Intv_Stat, default 1.0
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_simulation_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_simulation

   type, extends(type_model_base) :: type_model_3d_simulation

      real(SP) :: total_time   = 0.0_SP
      integer  :: sim_steps    = 0
      real(SP) :: plot_start   = 0.0_SP
      real(SP) :: plot_intv    = 0.0_SP
      real(SP) :: screen_intv  = 1.0_SP
      real(SP) :: cfl          = 0.5_SP

      real(SP) :: dt_ini = 0.1_SP
      real(SP) :: dt_min = 1.0e-6_SP
      real(SP) :: dt_max = 1.0_SP

      integer  :: nstat            = 0
      character(:), allocatable :: stations_file
      real(SP) :: plot_intv_stat   = 1.0_SP

   contains
      procedure :: read_input => simulation_3d_read_input
   end type type_model_3d_simulation

contains

   subroutine simulation_3d_read_input(this, env)
      class(type_model_3d_simulation), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: ts_yaml, stat_yaml
      logical :: no_ts, no_stat, no_key

      sub_env = get_sub_env(env, 'simulation')
      this%is_activated = .true.

      call sub_env%yaml%read_positive('total_time', val=this%total_time)
      call sub_env%yaml%read('sim_steps',   silent=no_key, val=this%sim_steps,   default='0')
      call sub_env%yaml%read('plot_start',  silent=no_key, val=this%plot_start,  default='0.0')
      call sub_env%yaml%read_positive('plot_intv', val=this%plot_intv)
      call sub_env%yaml%read('screen_intv', silent=no_key, val=this%screen_intv, default='1.0')
      call sub_env%yaml%read('cfl',         silent=no_key, val=this%cfl,         default='0.5')

      ts_yaml = sub_env%yaml%cast_dictionary('time_stepping', no_ts)
      if (.not. no_ts) then
         call ts_yaml%read('dt_ini', silent=no_key, val=this%dt_ini, default='0.1')
         call ts_yaml%read('dt_min', silent=no_key, val=this%dt_min, default='1.0e-6')
         call ts_yaml%read('dt_max', silent=no_key, val=this%dt_max, default='1.0')
      end if

      stat_yaml = sub_env%yaml%cast_dictionary('stations', no_stat)
      if (.not. no_stat) then
         call stat_yaml%read('count',    silent=no_key, val=this%nstat,          default='0')
         call stat_yaml%read('interval', silent=no_key, val=this%plot_intv_stat, default='1.0')
         if (this%nstat > 0) then
            call stat_yaml%read('file', val=this%stations_file)
         end if
      end if

   end subroutine simulation_3d_read_input

end module model_3d_simulation_mod
