!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output configuration YAML reader
!
!  YAML block: output:
!    interval:        <real>     field output cadence (s),     REQUIRED
!                                (nee simulation.output_interval)
!    result_folder:   <string>   output directory,             default './output/'
!    field_io_type:   <string>   parallel field I/O format,    default 'ASCII'
!    output_res:      <int>      field sub-sampling factor,    default 1
!    blowup_threshold: <real>    blow-up |eta| threshold (m),  default derived
!                                100*max|Depth| (nee EtaBlowVal)
!    depth_out:       <bool>     output bathymetry (static),   default NO
!    stations:                   presence = station time series
!      file:     <string>        one "i j" pair per line;      REQUIRED
!                                station count = line count
!      interval: <real>          station cadence (s),          default 1.0
!      buffer:   <int>           station buffer size,          default 1000
!    means:                      presence = wave-averaged output window
!      interval:    <real>       averaging window (s),         REQUIRED
!      steady_time: <real>       time to start averaging (s),  default 0
!    vessel:                     presence = resistance time series
!      interval: <real>          series cadence (s), 0 = every step; REQUIRED
!                                (nee OUT_VESSEL + PLOT_INTV_VESSEL)
!    arrival_time:               presence = first-arrival map
!      min_height: <real>        arrival threshold (m),        default 0.001
!    variables: [U, V, ETA, Hmax, Hmin, Umax, MFmax, VORmax,
!                MASK, MASK9, Umean, Vmean, ETAmean, WaveHeight,
!                SXL, SXR, SYL, SYR, SourceX, SourceY,
!                FrcX, FrcY, BrkdisX, BrkdisY, P, Q,
!                Fx, Fy, Gx, Gy, AGE, ROLLER, UNDERTOW,
!                NU, TMP, Radiation, ETAscreen,
!                Pstorm, Ustorm, Vstorm,     # meteo fields (nee OUT_METEO)
!                Pves, VesUp, VesVp]         # vessel fields (nee OUT_VESSEL)
!                                temporary flat list; maps each name → OUT_* flag.
!                                Unknown names are rejected loudly.
!                                Will be replaced by per-channel variable lists.
!    channels: (list of channel dicts — stub, not yet parsed)
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_output_mod
   use core_constants_mod, only: SP, ZERO, SMALL, type_string, MPI_SP
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use core_grid_mod, only: type_grid_2d
   use model_base_mod, only: type_model_base
   use mpi_f08

   use core_yaml_file_mod, only: type_yaml_reader

   use model_config_defaults_mod, only: DEF_OUTPUT_ARRIVAL_TIME_MIN_HEIGHT, &
                                        DEF_OUTPUT_DEPTH_OUT, &
                                        DEF_OUTPUT_FIELD_IO_TYPE, &
                                        DEF_OUTPUT_MEANS_STEADY_TIME, &
                                        DEF_OUTPUT_OUTPUT_RES, &
                                        DEF_OUTPUT_RESULT_FOLDER, &
                                        DEF_OUTPUT_STATIONS_BUFFER, &
                                        DEF_OUTPUT_STATIONS_INTERVAL

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

      ! Field output cadence (nee simulation.output_interval / legacy PLOT_INTV)
      real(SP) :: interval = 0.0_SP

      ! Bridge fields for legacy io.F use
      character(:), allocatable :: result_folder
      character(:), allocatable :: field_io_type
      integer  :: output_res = 1

      ! Checkpoint (hot-start) write dir; empty => none.  Presence => write the
      ! checkpoint set (core.bin now, later per-module bins) at run end.
      character(:), allocatable :: checkpoint
      logical  :: write_checkpoint = .false.
      ! Blow-up threshold.  Legacy DERIVES this as 100*max|Depth| in
      ! INITIALIZATION (init.F:850) and overwrites whatever the input file said,
      ! so legacy's own EtaBlowVal key is dead.  resolve_blowup() reproduces the
      ! derived value whenever the YAML key is absent; an explicit key overrides
      ! it (a deliberate improvement -- legacy cannot be overridden at all).
      ! A fixed threshold cannot work: a deep-draft hull legitimately imprints
      ! eta = -draft, which a flat 10 m limit reads as a blow-up on step 1.
      real(SP) :: blowup_threshold = 10.0_SP
      logical  :: has_blow_val = .false.

      ! Depth output — static (no time component) unless sediment is active
      logical :: depth_out = .false.

      ! Station time series (nee number_stations/stations_file + the
      ! simulation-section cadence pair); count derived from the file
      logical :: stations_on = .false.
      character(:), allocatable :: stations_file
      real(SP) :: stations_interval = 1.0_SP
      integer  :: stations_buffer = 1000

      ! Vessel resistance time series (nee OUT_VESSEL + PLOT_INTV_VESSEL);
      ! interval 0 maps to SMALL = legacy every-step default
      logical  :: vessel_series_on = .false.
      real(SP) :: vessel_interval = SMALL

      ! First-arrival map (nee numerics OUT_Time/ArrTimeMin): block presence
      ! enables the time-of-first-exceedance accumulator; no cadence
      logical  :: out_arr_time = .false.
      real(SP) :: arr_time_min_h = 0.001_SP

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
      ! Meteo/vessel field dumps (nee OUT_METEO/OUT_VESSEL bools); the field
      ! channel builder cross-checks applicability against the active models
      logical :: OUT_Pstorm = .false.
      logical :: OUT_Ustorm = .false.
      logical :: OUT_Vstorm = .false.
      logical :: OUT_Pves = .false.
      logical :: OUT_VesUp = .false.
      logical :: OUT_VesVp = .false.

      ! Wave-averaged output window (nee T_INTV_mean/STEADY_TIME); 999999
      ! component defaults = averaging disabled when the means: block is absent
      real(SP) :: T_INTV_mean = 999999.0_SP
      real(SP) :: STEADY_TIME = 999999.0_SP
   contains
      procedure :: read_input => output_read_input
      procedure :: resolve_blowup => output_resolve_blowup
   end type type_model_output

contains

   subroutine output_read_input(this, env)
      class(type_model_output), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: blk_yaml
      type(type_string), allocatable :: var_list(:)
      type(type_path) :: sta_path
      integer :: iv
      logical :: is_empty, no_key, no_vars, no_blk

      ! Initialize string fields before possible early exit so io.F always gets valid values
      this%result_folder = "./output/"
      this%field_io_type = "ASCII"
      this%stations_file = ""

      sub_env = get_sub_env(env, "output", is_empty)
      this%is_activated = .not. is_empty
      this%n_channels = 0
      if (allocated(this%channels)) deallocate (this%channels)
      if (is_empty) call env%log%exit_on_error( &
         "output: section is required -- at minimum set interval:")

      call sub_env%yaml%read_positive("interval", val=this%interval)
      call sub_env%yaml%read("result_folder", val=this%result_folder, default=DEF_OUTPUT_RESULT_FOLDER)
      call sub_env%yaml%read("checkpoint", silent=no_key, val=this%checkpoint, default="")
      this%write_checkpoint = .not. no_key
      call sub_env%yaml%read("field_io_type", val=this%field_io_type, default=DEF_OUTPUT_FIELD_IO_TYPE)
      call sub_env%yaml%read("output_res", val=this%output_res, default=DEF_OUTPUT_OUTPUT_RES)
      ! NOTE: no `default=` here on purpose -- yaml%read only assigns `silent`
      ! when `default` is ABSENT, so asking for both hands back an unwritten
      ! flag.  Absent key -> blowup_threshold is WIPED (val intent(out)); safety
      ! comes from has_blow_val gating resolve_blowup(), which overwrites it with
      ! the legacy-derived 100*max|Depth| -- NOT from value preservation
      call sub_env%yaml%read("blowup_threshold", silent=no_key, val=this%blowup_threshold)
      this%has_blow_val = .not. no_key
      call sub_env%yaml%read("depth_out", val=this%depth_out, default=DEF_OUTPUT_DEPTH_OUT)

      ! stations: block presence enables the station time series; the station
      ! count is the file's line count (derive, don't duplicate)
      blk_yaml = sub_env%yaml%cast_dictionary("stations", no_blk)
      if (.not. no_blk) then
         this%stations_on = .true.
         call blk_yaml%read_input_path("file", silent=no_key, val=sta_path)
         if (no_key) call env%log%exit_on_error("output: stations: file is required")
         this%stations_file = sta_path%root
         call blk_yaml%read("interval", silent=no_key, val=this%stations_interval, &
                            default=DEF_OUTPUT_STATIONS_INTERVAL)
         call blk_yaml%read("buffer", silent=no_key, val=this%stations_buffer, &
                            default=DEF_OUTPUT_STATIONS_BUFFER)
      end if

      ! means: block presence enables the wave-averaged window
      blk_yaml = sub_env%yaml%cast_dictionary("means", no_blk)
      if (.not. no_blk) then
         call blk_yaml%read_positive("interval", val=this%T_INTV_mean)
         call blk_yaml%read("steady_time", silent=no_key, val=this%STEADY_TIME, &
                            default=DEF_OUTPUT_MEANS_STEADY_TIME)
      end if

      ! vessel: block presence enables the resistance time series
      blk_yaml = sub_env%yaml%cast_dictionary("vessel", no_blk)
      if (.not. no_blk) then
         this%vessel_series_on = .true.
         call blk_yaml%read("interval", val=this%vessel_interval)
         ! legacy "PLOT_INTV_VESSEL not specified, use SMALL" -- 0 keeps the
         ! every-step behaviour without a magic literal in the config
         if (this%vessel_interval <= ZERO) this%vessel_interval = SMALL
      end if

      ! arrival_time: block presence enables the first-arrival map
      blk_yaml = sub_env%yaml%cast_dictionary("arrival_time", no_blk)
      if (.not. no_blk) then
         this%out_arr_time = .true.
         call blk_yaml%read("min_height", silent=no_key, val=this%arr_time_min_h, &
                            default=DEF_OUTPUT_ARRIVAL_TIME_MIN_HEIGHT)
      end if

      ! Retired key spellings: loud rejection beats silent acceptance
      call reject_moved_key(sub_env, "EtaBlowVal", "blowup_threshold")
      call reject_moved_key(sub_env, "T_INTV_mean", "means: interval")
      call reject_moved_key(sub_env, "STEADY_TIME", "means: steady_time")
      call reject_moved_key(sub_env, "number_stations", "stations: (count = file line count)")
      call reject_moved_key(sub_env, "stations_file", "stations: file")

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
            case ("Pstorm"); this%OUT_Pstorm = .true.
            case ("Ustorm"); this%OUT_Ustorm = .true.
            case ("Vstorm"); this%OUT_Vstorm = .true.
            case ("Pves"); this%OUT_Pves = .true.
            case ("VesUp"); this%OUT_VesUp = .true.
            case ("VesVp"); this%OUT_VesVp = .true.
            case default
               call env%log%exit_on_error( &
                  "output: variables: unknown name '"//trim(var_list(iv)%s)//"'")
            end select
         end do
      end if

   end subroutine output_read_input

   subroutine reject_moved_key(sub_env, old_key, new_home)
      type(type_env), intent(inout) :: sub_env
      character(*), intent(in) :: old_key, new_home

      character(:), allocatable :: tmp
      logical :: no_key

      call sub_env%yaml%read(old_key, silent=no_key, val=tmp)
      if (.not. no_key) call sub_env%log%exit_on_error( &
         "output: "//old_key//" moved -- set "//new_home)

   end subroutine reject_moved_key

   ! Legacy INITIALIZATION (init.F:850):
   !     EtaBlowVal = 100 * MAXVAL(abs(Depth(Ibeg:Iend, Jbeg:Jend)))
   ! reduced with MPI_MAX across ranks.  Scaling the threshold to the water
   ! depth is what makes it work for a deep-draft hull, whose draft legitimately
   ! drives |eta| far past any fixed limit.  Called once the bathymetry is up.
   subroutine output_resolve_blowup(this, grid, depth)
      class(type_model_output), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: depth(:, :)

      real(SP) :: local_max, global_max
      integer :: ierr

      ! an explicit key wins; legacy has no such escape hatch
      if (this%has_blow_val) return

      associate (lp => grid%lp)
         local_max = maxval(abs(depth(lp%ib:lp%ie, lp%jb:lp%je)))
      end associate

      if (grid%nx_proc*grid%ny_proc > 1) then
         call MPI_Allreduce(local_max, global_max, 1, MPI_SP, MPI_MAX, &
                            grid%cart_comm, ierr)
         local_max = global_max
      end if

      this%blowup_threshold = 100.0_SP*local_max

   end subroutine output_resolve_blowup

end module model_output_mod
