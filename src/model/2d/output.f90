!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output configuration YAML reader
!
!  YAML block: output:
!    result_folder:   <string>   output directory,             default './output/'
!    field_io_type:   <string>   parallel field I/O format,    default 'ASCII'
!    number_stations: <int>      station count,                default 0
!    stations_file:   <string>   station coordinates file      (required if number_stations > 0)
!    plot_intv_station: <real>   station output interval (s),  default 1.0
!    station_output_buffer: <int> station buffer size,         default 1000
!    output_res:      <int>      field sub-sampling factor,    default 1
!    EtaBlowVal:      <real>     blow-up threshold (m),        default 10.0
!    depth_out:       <bool>     output bathymetry (static),   default NO
!    T_INTV_mean:     <real>     wave-averaging interval (s),  default 999999.0 (disabled)
!    STEADY_TIME:     <real>     time to start averaging (s),  default 999999.0 (disabled)
!                                (time-varying once sediment is active)
!    variables: [U, V, ETA, Hmax, Hmin, Umax, MFmax, VORmax,
!                MASK, MASK9, Umean, Vmean, ETAmean, WaveHeight,
!                SXL, SXR, SYL, SYR, SourceX, SourceY,
!                FrcX, FrcY, BrkdisX, BrkdisY, P, Q,
!                Fx, Fy, Gx, Gy, AGE, ROLLER, UNDERTOW,
!                NU, TMP, Radiation, ETAscreen]
!                                temporary flat list; maps each name → OUT_* flag.
!                                Will be replaced by per-channel variable lists.
!    channels: (list of channel dicts — stub, not yet parsed)
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_output_mod
   use core_constants_mod, only: SP, type_string
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_OUTPUT_DEPTH_OUT, DEF_OUTPUT_ETABLOWVAL, &
                                        DEF_OUTPUT_FIELD_IO_TYPE, &
                                        DEF_OUTPUT_NUMBER_STATIONS, DEF_OUTPUT_OUTPUT_RES, &
                                        DEF_OUTPUT_RESULT_FOLDER, DEF_OUTPUT_STEADY_TIME, &
                                        DEF_OUTPUT_T_INTV_MEAN

   implicit none

   private
   public :: type_channel_config, type_model_output

   character(len=10), parameter :: GEOM_TYPES(3) = &
                                   [character(len=10) :: "field", "station", "transect"]
   character(len=8), parameter :: STAT_TYPES(4) = &
                                  [character(len=8) :: "min", "max", "mean", "rms"]
   character(len=8), parameter :: FORMAT_TYPES(1) = &
                                  [character(len=8) :: "ascii"]

   type :: type_channel_config
      character(:), allocatable :: id
      character(:), allocatable :: geom_type
      character(:), allocatable :: format
      character(:), allocatable :: variables(:)
      character(:), allocatable :: statistics(:)
      logical :: snapshot = .true.
      real(SP) :: t_start = 0.0_SP
      real(SP) :: interval = 0.0_SP
      integer :: buffer_size = 1000
      character(:), allocatable :: coords_file
      real(SP) :: start_coord(2) = 0.0_SP
      real(SP) :: end_coord(2) = 0.0_SP
      integer :: n_points = 0
   end type type_channel_config

   type, extends(type_model_base) :: type_model_output
      type(type_channel_config), allocatable :: channels(:)
      integer :: n_channels = 0

      ! Bridge fields for legacy io.F use
      character(:), allocatable :: result_folder
      character(:), allocatable :: field_io_type
      character(:), allocatable :: stations_file
      integer  :: number_stations = 0
      integer  :: output_res = 1
      real(SP) :: EtaBlowVal = 10.0_SP

      ! Depth output — static (no time component) unless sediment is active
      logical :: depth_out = .false.

      ! Per-variable output flags; derived from variables: list
      logical :: OUT_U = .false.
      logical :: OUT_V = .false.
      logical :: OUT_ETA = .false.
      logical :: OUT_EtaScreen = .false.
      logical :: OUT_Hmax = .false.
      logical :: OUT_Hmin = .false.
      logical :: OUT_Umax = .false.
      logical :: OUT_MFmax = .false.
      logical :: OUT_VORmax = .false.
      logical :: OUT_MASK = .false.
      logical :: OUT_MASK9 = .false.
      logical :: OUT_Umean = .false.
      logical :: OUT_Vmean = .false.
      logical :: OUT_ETAmean = .false.
      logical :: OUT_WaveHeight = .false.
      logical :: OUT_SXL = .false.
      logical :: OUT_SXR = .false.
      logical :: OUT_SYL = .false.
      logical :: OUT_SYR = .false.
      logical :: OUT_SourceX = .false.
      logical :: OUT_SourceY = .false.
      logical :: OUT_FrcX = .false.
      logical :: OUT_FrcY = .false.
      logical :: OUT_BrkdisX = .false.
      logical :: OUT_BrkdisY = .false.
      logical :: OUT_P = .false.
      logical :: OUT_Q = .false.
      logical :: OUT_Fx = .false.
      logical :: OUT_Fy = .false.
      logical :: OUT_Gx = .false.
      logical :: OUT_Gy = .false.
      logical :: OUT_AGE = .false.
      logical :: OUT_ROLLER = .false.
      logical :: OUT_UNDERTOW = .false.
      logical :: OUT_NU = .false.
      logical :: OUT_TMP = .false.
      logical :: OUT_Radiation = .false.

      ! Wave-averaged output window; will map to a mean-stats channel interval/t_start
      ! once the output block YAML is fully implemented.
      real(SP) :: T_INTV_mean = 999999.0_SP
      real(SP) :: STEADY_TIME = 999999.0_SP
   contains
      procedure :: read_input => output_read_input
   end type type_model_output

contains

   subroutine output_read_input(this, env)
      class(type_model_output), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_string), allocatable :: var_list(:)
      integer :: iv
      logical :: is_empty, no_key, no_vars

      ! Initialize string fields before possible early return so io.F always gets valid values
      this%result_folder = "./output/"
      this%field_io_type = "ASCII"
      this%stations_file = ""

      sub_env = get_sub_env(env, "output", is_empty)
      this%is_activated = .not. is_empty
      this%n_channels = 0
      if (allocated(this%channels)) deallocate (this%channels)
      if (is_empty) return

      call sub_env%yaml%read("result_folder", val=this%result_folder, default=DEF_OUTPUT_RESULT_FOLDER)
      call sub_env%yaml%read("field_io_type", val=this%field_io_type, default=DEF_OUTPUT_FIELD_IO_TYPE)
      call sub_env%yaml%read("number_stations", val=this%number_stations, default=DEF_OUTPUT_NUMBER_STATIONS)
      if (this%number_stations > 0) then
         call sub_env%yaml%read("stations_file", val=this%stations_file, default="")
      end if
      call sub_env%yaml%read("output_res", val=this%output_res, default=DEF_OUTPUT_OUTPUT_RES)
      call sub_env%yaml%read("EtaBlowVal", silent=no_key, val=this%EtaBlowVal, default=DEF_OUTPUT_ETABLOWVAL)
      call sub_env%yaml%read("depth_out", val=this%depth_out, default=DEF_OUTPUT_DEPTH_OUT)
      call sub_env%yaml%read("T_INTV_mean", silent=no_key, val=this%T_INTV_mean, default=DEF_OUTPUT_T_INTV_MEAN)
      call sub_env%yaml%read("STEADY_TIME", silent=no_key, val=this%STEADY_TIME, default=DEF_OUTPUT_STEADY_TIME)

      call sub_env%yaml%read_string_array("variables", silent=no_vars, val=var_list)
      if (.not. no_vars) then
         do iv = 1, size(var_list)
            select case (trim(var_list(iv)%s))
            case ("U"); this%OUT_U = .true.
            case ("V"); this%OUT_V = .true.
            case ("ETA"); this%OUT_ETA = .true.
            case ("ETAscreen"); this%OUT_EtaScreen = .true.
            case ("Hmax"); this%OUT_Hmax = .true.
            case ("Hmin"); this%OUT_Hmin = .true.
            case ("Umax"); this%OUT_Umax = .true.
            case ("MFmax"); this%OUT_MFmax = .true.
            case ("VORmax"); this%OUT_VORmax = .true.
            case ("MASK"); this%OUT_MASK = .true.
            case ("MASK9"); this%OUT_MASK9 = .true.
            case ("Umean"); this%OUT_Umean = .true.
            case ("Vmean"); this%OUT_Vmean = .true.
            case ("ETAmean"); this%OUT_ETAmean = .true.
            case ("WaveHeight"); this%OUT_WaveHeight = .true.
            case ("SXL"); this%OUT_SXL = .true.
            case ("SXR"); this%OUT_SXR = .true.
            case ("SYL"); this%OUT_SYL = .true.
            case ("SYR"); this%OUT_SYR = .true.
            case ("SourceX"); this%OUT_SourceX = .true.
            case ("SourceY"); this%OUT_SourceY = .true.
            case ("FrcX"); this%OUT_FrcX = .true.
            case ("FrcY"); this%OUT_FrcY = .true.
            case ("BrkdisX"); this%OUT_BrkdisX = .true.
            case ("BrkdisY"); this%OUT_BrkdisY = .true.
            case ("P"); this%OUT_P = .true.
            case ("Q"); this%OUT_Q = .true.
            case ("Fx"); this%OUT_Fx = .true.
            case ("Fy"); this%OUT_Fy = .true.
            case ("Gx"); this%OUT_Gx = .true.
            case ("Gy"); this%OUT_Gy = .true.
            case ("AGE"); this%OUT_AGE = .true.
            case ("ROLLER"); this%OUT_ROLLER = .true.
            case ("UNDERTOW"); this%OUT_UNDERTOW = .true.
            case ("NU"); this%OUT_NU = .true.
            case ("TMP"); this%OUT_TMP = .true.
            case ("Radiation"); this%OUT_Radiation = .true.
            end select
         end do
      end if

   end subroutine output_read_input

end module model_output_mod
