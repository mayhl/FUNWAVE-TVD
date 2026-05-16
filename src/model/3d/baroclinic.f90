!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D baroclinic / tidal parameters YAML reader
!
!  YAML block: baroclinic:         (optional; guards #if BAROCLINIC in READ_INPUT)
!    ini_sali_input: <string>      'CONST' or 'DATA', default 'CONST'
!    ini_sali: <real>              initial salinity (when CONST), default 35.0
!    ini_sali_file: <path>         initial salinity file (when DATA)
!    ini_temp_input: <string>      'CONST' or 'DATA', default 'CONST'
!    ini_temp: <real>              initial temperature (when CONST), default 0.0
!    ini_temp_file: <path>         initial temperature file (when DATA)
!    tid_low_pass: <bool>          TID_LOW_PASS, default NO
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_baroclinic_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_baroclinic

   type, extends(type_model_base) :: type_model_3d_baroclinic

      character(:), allocatable :: ini_sali_input
      real(SP) :: ini_sali = 35.0_SP
      character(:), allocatable :: ini_sali_file

      character(:), allocatable :: ini_temp_input
      real(SP) :: ini_temp = 0.0_SP
      character(:), allocatable :: ini_temp_file

      logical :: tid_low_pass = .false.

   contains
      procedure :: read_input => baroclinic_3d_read_input
   end type type_model_3d_baroclinic

contains

   subroutine baroclinic_3d_read_input(this, env)
      class(type_model_3d_baroclinic), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key

      sub_env = get_sub_env(env, 'baroclinic', is_empty)
      this%is_activated = .not. is_empty
      if (.not. this%is_activated) return

      call sub_env%yaml%read('ini_sali_input', val=this%ini_sali_input, default='CONST')
      if (this%ini_sali_input(1:4) == 'CONS') then
         call sub_env%yaml%read('ini_sali', silent=no_key, val=this%ini_sali, default='35.0')
      else if (this%ini_sali_input(1:4) == 'DATA') then
         call sub_env%yaml%read('ini_sali_file', val=this%ini_sali_file)
      end if

      call sub_env%yaml%read('ini_temp_input', val=this%ini_temp_input, default='CONST')
      if (this%ini_temp_input(1:4) == 'CONS') then
         call sub_env%yaml%read('ini_temp', silent=no_key, val=this%ini_temp, default='0.0')
      else if (this%ini_temp_input(1:4) == 'DATA') then
         call sub_env%yaml%read('ini_temp_file', val=this%ini_temp_file)
      end if

      call sub_env%yaml%read('tid_low_pass', silent=no_key, val=this%tid_low_pass, default='NO')

   end subroutine baroclinic_3d_read_input

end module model_3d_baroclinic_mod
