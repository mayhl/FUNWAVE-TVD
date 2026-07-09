!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D physics parameters YAML reader
!
!  YAML block: physics:
!    barotropic: <bool>         default YES
!    non_hydro: <bool>          default NO
!    high_order: <string>       default 'SECOND'
!    time_order: <string>       default 'THIRD'
!    convection: <string>       default 'WENO'
!    adv_hllc: <bool>           default NO
!    tramp: <real>              default 0.0
!    periodic_x: <bool>         default NO
!    periodic_y: <bool>         default NO
!    external_forcing: <bool>   default NO
!    froude_cap: <real>         FROUDECAP (only under FROUDE_CAP preprocessor flag)
!    wave_average:
!      active: <bool>           WAVE_AVERAGE_ON, default NO
!      t_start: <real>          Wave_Ave_Start, default 0.0
!      t_end: <real>            Wave_Ave_End, default 999999.0
!      height_id: <int>         WaveheightID, default 1
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_physics_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_physics

   type, extends(type_model_base) :: type_model_3d_physics

      logical  :: barotropic       = .true.
      logical  :: non_hydro        = .false.
      character(:), allocatable :: high_order
      character(:), allocatable :: time_order
      character(:), allocatable :: convection
      logical  :: adv_hllc         = .false.
      real(SP) :: tramp            = 0.0_SP
      logical  :: periodic_x       = .false.
      logical  :: periodic_y       = .false.
      logical  :: external_forcing = .false.
      real(SP) :: froude_cap       = 0.0_SP

      logical  :: wave_average_on  = .false.
      real(SP) :: wave_ave_start   = 0.0_SP
      real(SP) :: wave_ave_end     = 999999.0_SP
      integer  :: waveheight_id    = 1

   contains
      procedure :: read_input => physics_3d_read_input
   end type type_model_3d_physics

contains

   subroutine physics_3d_read_input(this, env)
      class(type_model_3d_physics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: wa_yaml
      logical :: no_wa, no_key

      sub_env = get_sub_env(env, "physics")
      this%is_activated = .true.

      call sub_env%yaml%read("barotropic",       val=this%barotropic,       default="YES")
      call sub_env%yaml%read("non_hydro",        val=this%non_hydro,        default="NO")
      call sub_env%yaml%read("high_order",       val=this%high_order,       default="SECOND")
      call sub_env%yaml%read("time_order",       val=this%time_order,       default="THIRD")
      call sub_env%yaml%read("convection",       val=this%convection,       default="WENO")
      call sub_env%yaml%read("adv_hllc",         val=this%adv_hllc,         default="NO")
      call sub_env%yaml%read("tramp",            silent=no_key, val=this%tramp,     default="0.0")
      call sub_env%yaml%read("periodic_x",       val=this%periodic_x,       default="NO")
      call sub_env%yaml%read("periodic_y",       val=this%periodic_y,       default="NO")
      call sub_env%yaml%read("external_forcing", val=this%external_forcing, default="NO")
# if defined (FROUDE_CAP)
      call sub_env%yaml%read("froude_cap", silent=no_key, val=this%froude_cap, default="0.0")
# endif

      wa_yaml = sub_env%yaml%cast_dictionary("wave_average", no_wa)
      if (.not. no_wa) then
         call wa_yaml%read("active",    val=this%wave_average_on, default="NO")
         call wa_yaml%read("t_start",   val=this%wave_ave_start,  default="0.0")
         call wa_yaml%read("t_end",     val=this%wave_ave_end,    default="999999.0")
         call wa_yaml%read("height_id", val=this%waveheight_id,   default="1")
      end if

   end subroutine physics_3d_read_input

end module model_3d_physics_mod
