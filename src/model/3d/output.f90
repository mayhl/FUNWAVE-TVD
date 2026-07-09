!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D output configuration YAML reader
!
!  YAML block: output:
!    result_folder: <string>    default './output/'
!    field_io_type: <string>    default 'ASCII'
!    variables: [DEP, ETA, U, V, W, P, TKE, EPS, S, MU,
!                BUB, A, F, T, G, SALI, TEMP, RHO]
!                maps each name → OUT_* flag
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_output_mod
   use core_constants_mod, only: SP, type_string
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_output

   type, extends(type_model_base) :: type_model_3d_output

      character(:), allocatable :: result_folder
      character(:), allocatable :: field_io_type

      logical :: out_dep  = .false.
      logical :: out_eta  = .false.
      logical :: out_u    = .false.
      logical :: out_v    = .false.
      logical :: out_w    = .false.
      logical :: out_p    = .false.
      logical :: out_tke  = .false.
      logical :: out_eps  = .false.
      logical :: out_s    = .false.
      logical :: out_mu   = .false.
      logical :: out_bub  = .false.
      logical :: out_a    = .false.
      logical :: out_f    = .false.
      logical :: out_t    = .false.
      logical :: out_g    = .false.
      logical :: out_sali = .false.
      logical :: out_temp = .false.
      logical :: out_rho  = .false.

   contains
      procedure :: read_input => output_3d_read_input
   end type type_model_3d_output

contains

   subroutine output_3d_read_input(this, env)
      class(type_model_3d_output), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_string), allocatable :: var_list(:)
      integer :: iv
      logical :: is_empty, no_vars

      this%result_folder = "./output/"
      this%field_io_type = "ASCII"

      sub_env = get_sub_env(env, "output", is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      call sub_env%yaml%read("result_folder", val=this%result_folder, default="./output/")
      call sub_env%yaml%read("field_io_type", val=this%field_io_type, default="ASCII")

      call sub_env%yaml%read_string_array("variables", silent=no_vars, val=var_list)
      if (.not. no_vars) then
         do iv = 1, size(var_list)
            select case (trim(var_list(iv)%s))
            case ("DEP");  this%out_dep  = .true.
            case ("ETA");  this%out_eta  = .true.
            case ("U");    this%out_u    = .true.
            case ("V");    this%out_v    = .true.
            case ("W");    this%out_w    = .true.
            case ("P");    this%out_p    = .true.
            case ("TKE");  this%out_tke  = .true.
            case ("EPS");  this%out_eps  = .true.
            case ("S");    this%out_s    = .true.
            case ("MU");   this%out_mu   = .true.
            case ("BUB");  this%out_bub  = .true.
            case ("A");    this%out_a    = .true.
            case ("F");    this%out_f    = .true.
            case ("T");    this%out_t    = .true.
            case ("G");    this%out_g    = .true.
            case ("SALI"); this%out_sali = .true.
            case ("TEMP"); this%out_temp = .true.
            case ("RHO");  this%out_rho  = .true.
            end select
         end do
      end if

   end subroutine output_3d_read_input

end module model_3d_output_mod
