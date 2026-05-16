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

   implicit none

   private
   public :: type_model_numerics

   type, extends(type_model_base) :: type_model_numerics

      character(:), allocatable :: Time_Scheme
      character(:), allocatable :: construction   ! YAML key: CONSTRUCTION → CONSTR
      character(:), allocatable :: high_order     ! YAML key: HIGH_ORDER

      real(SP) :: CFL         = 0.5_SP
      real(SP) :: FroudeCap   = 3.0_SP
      real(SP) :: MinDepth    = 0.1_SP
      real(SP) :: MinDepthFrc = 0.1_SP

      logical  :: OUT_Time   = .false.
      real(SP) :: ArrTimeMin = 0.001_SP

   contains
      procedure :: read_input => numerics_read_input
   end type type_model_numerics

contains

   subroutine numerics_read_input(this, env)
      class(type_model_numerics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_num, no_key

      ! Set string defaults before possible early return so io.F always gets valid values
      this%Time_Scheme  = 'Runge_Kutta'
      this%construction = 'HLLC'
      this%high_order   = 'FOURTH'

      sub_env = get_sub_env(env, 'numerics', is_empty=no_num)
      this%is_activated = .not. no_num
      if (.not. this%is_activated) return

      call sub_env%yaml%read('Time_Scheme',  val=this%Time_Scheme,  default='Runge_Kutta')
      call sub_env%yaml%read('CONSTRUCTION', val=this%construction, default='HLLC')
      call sub_env%yaml%read('HIGH_ORDER',   val=this%high_order,   default='FOURTH')

      call sub_env%yaml%read('CFL',         silent=no_key, val=this%CFL,         default='0.5')
      call sub_env%yaml%read('FroudeCap',   silent=no_key, val=this%FroudeCap,   default='3.0')
      call sub_env%yaml%read('MinDepth',    silent=no_key, val=this%MinDepth,    default='0.1')
      call sub_env%yaml%read('MinDepthFrc', silent=no_key, val=this%MinDepthFrc, default='0.1')

      call sub_env%yaml%read('OUT_Time',   val=this%OUT_Time,   default='NO')
      call sub_env%yaml%read('ArrTimeMinH', silent=no_key, val=this%ArrTimeMin, default='0.001')

   end subroutine numerics_read_input

end module model_numerics_mod
