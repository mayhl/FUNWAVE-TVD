!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D hot start parameters YAML reader
!
!  YAML block: hot_start:         (optional; omit for cold start)
!    eta_file: <path>             Eta_HotStart_File, required
!    u_file: <path>               U_HotStart_File
!    v_file: <path>               V_HotStart_File
!    w_file: <path>               W_HotStart_File
!    p_file: <path>               P_HotStart_File
!    --- under BAROCLINIC ---
!    sali_file: <path>            Sali_HotStart_File
!    temp_file: <path>            Temp_HotStart_File
!    --- when viscous_flow is active ---
!    rho_file: <path>             Rho_HotStart_File
!    tke_file: <path>             TKE_HotStart_File
!    eps_file: <path>             EPS_HotStart_File
!    --- under AIR_PRESSURE ---
!    pressure_file: <path>        Pressure_HotStart_File
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_hotstart_mod
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_hotstart

   type, extends(type_model_base) :: type_model_3d_hotstart

      character(:), allocatable :: eta_file
      character(:), allocatable :: u_file
      character(:), allocatable :: v_file
      character(:), allocatable :: w_file
      character(:), allocatable :: p_file
# if defined (BAROCLINIC)
      character(:), allocatable :: sali_file
      character(:), allocatable :: temp_file
# endif
      character(:), allocatable :: rho_file
      character(:), allocatable :: tke_file
      character(:), allocatable :: eps_file
# if defined (AIR_PRESSURE)
      character(:), allocatable :: pressure_file
# endif

   contains
      procedure :: read_input => hotstart_3d_read_input
   end type type_model_3d_hotstart

contains

   subroutine hotstart_3d_read_input(this, env)
      class(type_model_3d_hotstart), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key

      sub_env = get_sub_env(env, "hot_start", is_empty)
      this%is_activated = .not. is_empty
      if (.not. this%is_activated) return

      call sub_env%yaml%read("eta_file", val=this%eta_file)
      call sub_env%yaml%read("u_file",   silent=no_key, val=this%u_file,   default="")
      call sub_env%yaml%read("v_file",   silent=no_key, val=this%v_file,   default="")
      call sub_env%yaml%read("w_file",   silent=no_key, val=this%w_file,   default="")
      call sub_env%yaml%read("p_file",   silent=no_key, val=this%p_file,   default="")
# if defined (BAROCLINIC)
      call sub_env%yaml%read("sali_file", silent=no_key, val=this%sali_file, default="")
      call sub_env%yaml%read("temp_file", silent=no_key, val=this%temp_file, default="")
# endif
      call sub_env%yaml%read("rho_file", silent=no_key, val=this%rho_file, default="")
      call sub_env%yaml%read("tke_file", silent=no_key, val=this%tke_file, default="")
      call sub_env%yaml%read("eps_file", silent=no_key, val=this%eps_file, default="")
# if defined (AIR_PRESSURE)
      call sub_env%yaml%read("pressure_file", silent=no_key, val=this%pressure_file, default="")
# endif

   end subroutine hotstart_3d_read_input

end module model_3d_hotstart_mod
