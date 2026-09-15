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
!    format:          <string>   parallel field I/O format,    default 'binary'
!                                ascii | binary | netcdf | pnetcdf
!                                (nee field_io_type)
!    layout:          <string>   netcdf file topology,         default 'chunked'
!                                single (one output.nc, streams as groups) |
!                                per_stream (data.nc + diagnostics.nc) |
!                                chunked (field files split in time,
!                                <id>_<t0>-<t1>.nc, size-derived window)
!    max_file_size:   <real>     chunk roll-over size (GB),    default 50.0
!                                also the single/per_stream predicted-size
!                                warning threshold
!    blowup_threshold: <real>    blow-up |eta| threshold (m),  default derived
!                                100*max|Depth| (nee EtaBlowVal)
!    depth_out:       <bool>     output bathymetry (static),   default NO
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
!    geometries:                 named point sets shared by channels
!      - name: <string>          referenced by channels.geometry,  REQUIRED
!        type: station|transect                                    REQUIRED
!        x/y: [<real>, ...]      station query coords (m), equal length
!        file: <path>            OR one "x y" pair (m) per line
!        start/end: [x, y]       transect endpoints (m)
!        n_points: <int>         transect sample count (>= 2),
!                                default = sampled at min(dx, dy)
!    channels:                   point output streams (registry-name vars)
!      - name: <string>          file-name stem <name>_<var>.dat,  REQUIRED
!        geometry: <string>      geometries: entry name; OR inline
!                                type:/x:/y:/start:/end:/n_points: keys
!                                (joins the geometry list under the
!                                channel's name)
!        variables: [eta, ...]   field-registry names,             REQUIRED
!        interval: <real>        flush cadence (s),                REQUIRED
!        statistics: [max, ...]  presence => windowed channel (min/max/
!                                mean/rms over each interval, no snapshots);
!                                absence => instantaneous snapshot channel
!        t_start: <real>         default: simulation t_start
!        format: ascii|netcdf    default follows the deck format (netcdf/
!                                pnetcdf => netcdf group in diagnostics.nc,
!                                else per-variable <name>_<var>.dat)
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

   implicit none

   private
   public :: type_output_geometry, type_channel_config, type_model_output
   public :: VEC_DERIVED, PROD_DERIVED, PROD_SRC, PROD_STAT, PROD_SCALE

   character(len=10), parameter :: GEOM_TYPES(3) = &
                                   [character(len=10) :: "station", "transect", "field"]
   character(len=12), parameter :: STAT_TYPES(11) = &
                                   [character(len=12) :: "min", "max", "mean", "rms", "std", &
                                                          "max_time", "first_time", "last_time", "duration", &
                                                          "duration_max", "count"]
   ! the event class needs a threshold:
   character(len=12), parameter :: EVENT_STATS(5) = &
                                   [character(len=12) :: "first_time", "last_time", "duration", &
                                                          "duration_max", "count"]
   ! statistics: presets, expanded in place
   character(len=12), parameter :: PRESET_NAMES(3) = &
                                   [character(len=12) :: "envelope", "arrival", "inundation"]

   ! Vector-derived instantaneous variables (registry vectors: velocity =
   ! [u, v]): the builder registers per-step scratch fields under these
   ! names.  dir is circular — statistics on it are rejected at read.
   character(len=32), parameter :: VEC_DERIVED(2) = &
                                   [character(len=32) :: "velocity.mag", "velocity.dir"]

   ! Product-derived catalogue: out = scale * stat(source), evaluated at
   ! flush over the window.  hsig = 4.004 std(eta) — the Rayleigh H_1/3
   ! constant (Longuet-Higgins); 4.0 would be the spectral Hm0 convention
   ! (registry doc records the choice).  Sources ride hidden accumulators
   ! when not requested themselves.
   character(len=8), parameter :: PROD_DERIVED(1) = [character(len=8) :: "hsig"]
   character(len=8), parameter :: PROD_SRC(1) = [character(len=8) :: "eta"]
   character(len=8), parameter :: PROD_STAT(1) = [character(len=8) :: "std"]
   real(SP), parameter :: PROD_SCALE(1) = [4.004_SP]

   ! Named point set: station coords verbatim, transect expanded to its
   ! n_points samples at read time (channels only see resolved coords)
   type :: type_output_geometry
      character(:), allocatable :: name
      character(:), allocatable :: geom_type   ! 'station' or 'transect'
      real(SP), allocatable :: x(:), y(:)      ! global query coords (m)
   end type type_output_geometry

   type :: type_channel_config
      character(:), allocatable :: name
      integer :: geom_idx = 0
      character(32), allocatable :: variables(:)
      ! parallel to variables: hidden = accumulate only (derived source
      ! auto-added, never written)
      logical, allocatable :: hidden(:)
      ! product-derived requests (catalogue names, e.g. hsig)
      character(8), allocatable :: derived(:)
      integer :: n_derived = 0
      character(12), allocatable :: statistics(:)
      integer :: n_stats = 0
      ! threshold: {above | below | magnitude: v | [v, ...]} for the event
      ! statistics; direction +1 above / -1 below; filters (s, 0 = off)
      real(SP), allocatable :: thresholds(:)
      integer :: thr_dir = 0
      real(SP) :: gap = 0.0_SP
      real(SP) :: min_duration = 0.0_SP
      ! accumulate: window (default) | running | total
      character(8) :: accum_mode = "window"
      ! statistics presence derives the channel kind: windowed channels
      ! never write snapshots (uniform time meaning per file)
      logical :: snapshot = .true.
      real(SP) :: interval = 0.0_SP
      real(SP) :: t_start = 0.0_SP
      logical :: has_t_start = .false.
      ! t_start: spinup -- resolved against simulation.spinup where the
      ! channel is built (main), since output: is read without it in scope
      logical :: t_start_spinup = .false.
      real(SP) :: t_end = 0.0_SP
      logical :: has_t_end = .false.
      ! '' inherits the deck default: netcdf when the deck format is
      ! netcdf/pnetcdf, ascii otherwise
      character(8) :: format = ''
      ! single-precision save (binary/netcdf field bytes; ascii unchanged)
      logical :: single_prec = .false.
   end type type_channel_config

   type, extends(type_model_base) :: type_model_output
      type(type_output_geometry), allocatable :: geometries(:)
      type(type_channel_config), allocatable :: channels(:)
      integer :: n_channels = 0

      ! Field output cadence (nee simulation.output_interval / legacy PLOT_INTV)
      real(SP) :: interval = 0.0_SP

      ! Bridge fields for legacy io.F use
      character(:), allocatable :: result_folder
      character(:), allocatable :: format   ! ascii/binary/netcdf/pnetcdf (nee field_io_type)

      ! NetCDF file topology (single/per_stream/chunked) + the chunk
      ! roll-over size, doubling as the predicted-size warning cap (GB)
      character(:), allocatable :: layout
      real(SP) :: max_file_size = 50.0_SP

      ! Finest grid spacing, min(dx, dy) -- set by main BEFORE read_input
      ! (the deferred read_input(env) interface cannot carry it); the
      ! default transect sample step
      real(SP) :: min_spacing = 0.0_SP

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
      ! gate-field mirrors (breaking_active / nu_capped / froude_scale)
      logical :: OUT_BRK_ACTIVE = .false.
      logical :: OUT_NU_CAPPED = .false.
      logical :: OUT_FROUDE_SCALE = .false.
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

      ! Wave-averaged window params: CONFIG-ORPHANED since the means:
      ! retirement (channels own averaged output).  The means module
      ! stays for its physics hooks (roller/meteo etamean) but never
      ! activates at these defaults -- a physics-owned mean is the
      ! punch-listed replacement.  (nee T_INTV_mean/STEADY_TIME); 999999
      ! component defaults = averaging disabled when the means: block is absent
      real(SP) :: T_INTV_mean = 999999.0_SP
      real(SP) :: STEADY_TIME = 999999.0_SP
   contains
      procedure :: read_input => output_read_input
      procedure :: need => output_need
      procedure :: resolve_blowup => output_resolve_blowup
   end type type_model_output

contains

   subroutine output_read_input(this, env)
      class(type_model_output), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: blk_yaml
      integer :: iv
      logical :: is_empty, no_key, no_blk

      ! Initialize string fields before possible early exit so io.F always gets valid values
      this%result_folder = "./output/"
      this%format = "binary"

      sub_env = get_sub_env(env, "output", is_empty)
      this%is_activated = .not. is_empty
      this%n_channels = 0
      if (allocated(this%channels)) deallocate (this%channels)
      if (allocated(this%geometries)) deallocate (this%geometries)
      if (is_empty) call env%log%exit_on_error( &
         "output: section is required -- at minimum one channels: entry")
      call sub_env%yaml%read("result_folder", val=this%result_folder, default="./output/")
      call sub_env%yaml%read("checkpoint", silent=no_key, val=this%checkpoint, default="")
      this%write_checkpoint = .not. no_key
      call sub_env%yaml%read("format", val=this%format, default="binary")
      if (this%format /= "ascii" .and. this%format /= "binary" .and. &
          this%format /= "netcdf" .and. this%format /= "pnetcdf") &
         call env%log%exit_on_error("output: unknown format '"//this%format// &
                                    "' -- valid: ascii binary netcdf pnetcdf")
      call sub_env%yaml%read("layout", val=this%layout, default="chunked")
      if (this%layout /= "single" .and. this%layout /= "per_stream" .and. &
          this%layout /= "chunked") &
         call env%log%exit_on_error("output: unknown layout '"//this%layout// &
                                    "' -- valid: single per_stream chunked")
      call sub_env%yaml%read_positive("max_file_size", val=this%max_file_size, &
                                      default="50.0")
      ! no `default=` on purpose -- the fallback is not a constant: when the
      ! key is absent, resolve_blowup() computes the legacy-derived
      ! 100*max|Depth|, gated by has_blow_val
      call sub_env%yaml%read("blowup_threshold", silent=no_key, val=this%blowup_threshold)
      this%has_blow_val = .not. no_key
      call sub_env%yaml%read("depth_out", val=this%depth_out, default="NO")

      ! stations: retired -- channels: supersedes it (x/y coords, not i j
      ! indices; one <name>_<var>.dat per variable instead of sta_NNNN)
      blk_yaml = sub_env%yaml%cast_dictionary("stations", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "output: stations: retired -- use channels: with a station geometry"// &
         " (x/y in metres; legacy i j maps to x = (i-1)*dx, y = (j-1)*dy)")

      ! means: retired -- a windowed field channel supersedes it
      blk_yaml = sub_env%yaml%cast_dictionary("means", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "output: means retired -- use a field channel: channels: [{name: means,"// &
         " geometry: field, variables: [eta, u, v, hsig], statistics: [mean],"// &
         " t_start: <steady_time>, interval: <interval>}]")

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
                            default="0.001")
      end if

      ! geometries: + channels: point output streams
      call read_geometries(this, sub_env, this%min_spacing)
      call read_channels(this, sub_env, this%min_spacing)

      ! Retired key spellings: loud rejection beats silent acceptance
      call reject_moved_key(sub_env, "interval", &
                            "it on your field channel (channels: - {name: fields,"// &
                            " geometry: field, variables: [...], interval: ...})")
      call reject_string_list_key(sub_env, "variables", &
                                  "registry names on a channel (ETA -> eta, Hmax -> h_max,"// &
                                  " Umean -> a statistics: [mean] channel, WaveHeight -> hsig)")
      call reject_moved_key(sub_env, "field_io_type", &
                            "format: (lowercase ascii | binary | netcdf | pnetcdf)")
      call reject_moved_key(sub_env, "EtaBlowVal", "blowup_threshold")
      call reject_moved_key(sub_env, "T_INTV_mean", "means: interval")
      call reject_moved_key(sub_env, "STEADY_TIME", "means: steady_time")
      call reject_moved_key(sub_env, "number_stations", "channels: (count = x/y list length)")
      call reject_moved_key(sub_env, "stations_file", "channels: with x/y coordinate lists")
      call reject_moved_key(sub_env, "output_res", &
                            "nothing -- the stride was never consumed; subsample downstream")

      ! demand flags: channels drive the accumulator/copy machinery the
      ! flags list used to gate (running envelopes, mask copies, meteo
      ! staging).  Derived from the registry names the channels reference.
      do iv = 1, this%n_channels
         call derive_demand_flags(this, this%channels(iv))
         ! event statistics sample wet cells only: the mask mirror
         if (size(this%channels(iv)%thresholds) > 0) call this%need("mask")
      end do

   end subroutine output_read_input

   ! Channel variables -> demand flags (nee the variables: list)
   subroutine derive_demand_flags(this, cfg)
      type(type_model_output), intent(inout) :: this
      type(type_channel_config), intent(in) :: cfg

      integer :: iv

      do iv = 1, size(cfg%variables)
         call this%need(cfg%variables(iv))
      end do

   end subroutine derive_demand_flags

   ! Registry name -> the demand flag that keeps its array maintained.
   ! Raised by every channel variable and by any consumer that reads a
   ! maintained array without writing it (the meteo crest mask on h_max),
   ! the way a hidden hsig source accumulates unrequested; writing stays
   ! gated on the channel request.  Only names whose machinery is gated
   ! need mapping; plain fields (eta/u/v/p_flux/...) are always registered.
   subroutine output_need(this, name)
      class(type_model_output), intent(inout) :: this
      character(len=*), intent(in) :: name

      select case (trim(name))
      case ("h_max"); this%OUT_Hmax = .true.
      case ("h_min"); this%OUT_Hmin = .true.
      case ("u_max"); this%OUT_Umax = .true.
      case ("mf_max"); this%OUT_MFmax = .true.
      case ("vort_max"); this%OUT_VORmax = .true.
      case ("breaking_active"); this%OUT_BRK_ACTIVE = .true.
      case ("nu_capped"); this%OUT_NU_CAPPED = .true.
      case ("froude_scale"); this%OUT_FROUDE_SCALE = .true.
      case ("mask"); this%OUT_MASK = .true.
      case ("mask9"); this%OUT_MASK9 = .true.
      case ("nu_break"); this%OUT_NU = .true.
      case ("age_break"); this%OUT_AGE = .true.
      case ("roller_flux"); this%OUT_ROLLER = .true.
      case ("undertow_u", "undertow_v"); this%OUT_UNDERTOW = .true.
      case ("meteo_pressure"); this%OUT_Pstorm = .true.
      case ("meteo_wind_u"); this%OUT_Ustorm = .true.
      case ("meteo_wind_v"); this%OUT_Vstorm = .true.
      case ("vessel_pressure"); this%OUT_Pves = .true.
      case ("vessel_up"); this%OUT_VesUp = .true.
      case ("vessel_vp"); this%OUT_VesVp = .true.
      case ("arr_time"); this%out_arr_time = .true.
      end select

   end subroutine output_need

   subroutine read_geometries(this, sub_env, min_spacing)
      type(type_model_output), intent(inout) :: this
      type(type_env), intent(inout) :: sub_env
      real(SP), intent(in) :: min_spacing

      type(type_yaml_reader), allocatable :: entries(:)
      integer :: k, kk
      logical :: no_blk, no_key

      call sub_env%yaml%cast_dictionary_list("geometries", no_blk, entries)
      if (no_blk) then
         allocate (this%geometries(0))
         return
      end if

      allocate (this%geometries(size(entries)))
      do k = 1, size(entries)
         associate (g => this%geometries(k))
            call entries(k)%read("name", silent=no_key, val=g%name)
            if (no_key .or. len(g%name) == 0) call sub_env%log%exit_on_error( &
               "output: geometries: every entry needs a name:")
            call parse_geometry(entries(k), "geometries: '"//g%name//"'", &
                                sub_env, g, min_spacing)
         end associate
      end do

      do k = 2, size(this%geometries)
         do kk = 1, k - 1
            if (this%geometries(k)%name == this%geometries(kk)%name) &
               call sub_env%log%exit_on_error("output: geometries: duplicate name '"// &
                                              this%geometries(k)%name//"'")
         end do
      end do

      ! 'field' is the reserved whole-domain reference (geometry: field)
      do k = 1, size(this%geometries)
         if (this%geometries(k)%name == "field" .and. &
             this%geometries(k)%geom_type /= "field") &
            call sub_env%log%exit_on_error("output: geometries: the name 'field'"// &
                                           " is reserved for the whole-domain geometry")
      end do

   end subroutine read_geometries

   ! Shared by named geometries: entries and channel-inline geometry
   ! (ctx prefixes error messages with the owning entry)
   ! Grow this%geometries by one entry.
   !
   ! Explicit temporary + move_alloc, NOT the self-referential array
   ! constructor (this%geometries = [this%geometries, g]).  That form on an
   ! allocatable derived-type array with allocatable components miscompiles
   ! widely: ifort 2021.4 -O3 and 2021.7 both die here at output init with
   ! "forrtl: severe (122): invalid attempt to assign into a pointer that is
   ! not associated", and crayftn 16/18 ICE on the file outright (llvm-gen-
   ! util.c:1358 "Pointer argument type mismatch").  cce/21 and newer ifx
   ! tolerate it, which is why this only ever showed up off the main build.
   subroutine append_geometry(this, g)
      class(type_model_output), intent(inout) :: this
      type(type_output_geometry), intent(in) :: g

      type(type_output_geometry), allocatable :: grown(:)
      integer :: n

      n = 0
      if (allocated(this%geometries)) n = size(this%geometries)
      allocate (grown(n + 1))
      if (n > 0) grown(1:n) = this%geometries
      grown(n + 1) = g
      call move_alloc(grown, this%geometries)

   end subroutine append_geometry

   subroutine parse_geometry(entry, ctx, sub_env, g, min_spacing)
      type(type_yaml_reader), intent(inout) :: entry
      character(*), intent(in) :: ctx
      type(type_env), intent(inout) :: sub_env
      type(type_output_geometry), intent(inout) :: g
      real(SP), intent(in) :: min_spacing

      real(SP), allocatable :: p0(:), p1(:)
      character(:), allocatable :: gtype
      type(type_path) :: xy_path
      real(SP) :: frac
      integer :: np, i
      logical :: no_key, no_file

      call entry%read_enum("type", GEOM_TYPES, val=gtype)
      g%geom_type = gtype

      select case (gtype)
      case ("station")
         ! coords come from inline x:/y: lists XOR a coordinate file
         ! (one "x y" pair per line, metres)
         call entry%read_input_path("file", silent=no_file, val=xy_path)
         call entry%read_real_array("x", silent=no_key, val=g%x)
         if (.not. no_file) then
            if (.not. no_key) call sub_env%log%exit_on_error("output: "//ctx// &
                                                             ": give file: or x:/y:, not both")
            call read_station_coords(sub_env, ctx, xy_path%root, g)
         else
            if (no_key) call sub_env%log%exit_on_error("output: "//ctx// &
                                                       ": station needs x:/y: lists or a file:")
            call entry%read_real_array("y", silent=no_key, val=g%y)
            if (no_key) call sub_env%log%exit_on_error("output: "//ctx// &
                                                       ": station needs y:")
            if (size(g%x) /= size(g%y) .or. size(g%x) == 0) &
               call sub_env%log%exit_on_error("output: "//ctx// &
                                              ": x: and y: must be equal-length and non-empty")
         end if

      case ("transect")
         call entry%read_real_array("start", silent=no_key, val=p0)
         if (no_key .or. size(p0) /= 2) call sub_env%log%exit_on_error( &
            "output: "//ctx//": transect needs start: [x, y]")
         call entry%read_real_array("end", silent=no_key, val=p1)
         if (no_key .or. size(p1) /= 2) call sub_env%log%exit_on_error( &
            "output: "//ctx//": transect needs end: [x, y]")
         call entry%read("n_points", silent=no_key, val=np)
         if (no_key) then
            ! sample at the finest grid spacing by default
            np = max(2, nint(hypot(p1(1) - p0(1), p1(2) - p0(2))/min_spacing) + 1)
         else if (np < 2) then
            call sub_env%log%exit_on_error( &
               "output: "//ctx//": transect n_points: must be >= 2")
         end if

         allocate (g%x(np), g%y(np))
         do i = 1, np
            frac = real(i - 1, SP)/real(np - 1, SP)
            g%x(i) = p0(1) + frac*(p1(1) - p0(1))
            g%y(i) = p0(2) + frac*(p1(2) - p0(2))
         end do

      case ("field")
         ! whole-domain geometry: every interior cell, no coordinates
         if (entry%has_key("x") .or. entry%has_key("y") .or. &
             entry%has_key("file") .or. entry%has_key("start") .or. &
             entry%has_key("end") .or. entry%has_key("n_points")) &
            call sub_env%log%exit_on_error("output: "//ctx// &
                                           ": field geometry takes no coordinate keys")
         allocate (g%x(0), g%y(0))
      end select

   end subroutine parse_geometry

   ! Station coordinate file: one "x y" pair per line (metres); a parse
   ! failure before EOF is a malformed line, not a short count.
   subroutine read_station_coords(sub_env, ctx, fname, g)
      type(type_env), intent(inout) :: sub_env
      character(*), intent(in) :: ctx, fname
      type(type_output_geometry), intent(inout) :: g

      character(12) :: line_str
      real(SP) :: xv, yv
      logical :: file_exist
      integer :: funit, ios, n, i

      inquire (file=fname, exist=file_exist)
      if (.not. file_exist) call sub_env%log%exit_on_error( &
         "output: "//ctx//": file cannot be found: "//fname)

      open (newunit=funit, file=fname, status="old", action="read")
      n = 0
      do
         read (funit, *, iostat=ios) xv, yv
         if (ios /= 0) exit
         n = n + 1
      end do
      if (ios > 0) then
         write (line_str, '(I0)') n + 1
         call sub_env%log%exit_on_error("output: "//ctx// &
                                        ": cannot parse an 'x y' pair on line "// &
                                        trim(line_str)//" of "//fname)
      end if
      if (n == 0) call sub_env%log%exit_on_error("output: "//ctx// &
                                                 ": "//fname//" contains no points")

      allocate (g%x(n), g%y(n))
      rewind (funit)
      do i = 1, n
         read (funit, *) g%x(i), g%y(i)
      end do
      close (funit)

   end subroutine read_station_coords

   subroutine read_channels(this, sub_env, min_spacing)
      type(type_model_output), intent(inout) :: this
      type(type_env), intent(inout) :: sub_env
      real(SP), intent(in) :: min_spacing

      type(type_yaml_reader), allocatable :: entries(:)
      type(type_string), allocatable :: names(:)
      character(:), allocatable :: gname, valid, fmt
      integer :: k, kk, iv, g
      logical :: no_blk, no_key, no_stats

      call sub_env%yaml%cast_dictionary_list("channels", no_blk, entries)
      if (no_blk) then
         allocate (this%channels(0))
         return
      end if

      allocate (this%channels(size(entries)))
      this%n_channels = size(entries)
      do k = 1, size(entries)
         associate (cfg => this%channels(k))
            call entries(k)%read("name", silent=no_key, val=cfg%name)
            if (no_key .or. len(cfg%name) == 0) call sub_env%log%exit_on_error( &
               "output: channels: every entry needs a name:")

            ! geometry: reference XOR inline geometry keys; an inline
            ! geometry joins the list under the channel's own name
            call entries(k)%read("geometry", silent=no_key, val=gname)
            if (.not. no_key .and. entries(k)%has_key("type")) &
               call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                              "': give geometry: OR an inline type:, not both")
            if (no_key) then
               if (.not. entries(k)%has_key("type")) &
                  call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                 "': needs geometry: <name> or an inline type:")
               do g = 1, size(this%geometries)
                  if (this%geometries(g)%name == cfg%name) &
                     call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                    "': inline geometry collides with the"// &
                                                    " geometries: entry of the same name")
               end do
               block
                  type(type_output_geometry) :: g_inline
                  g_inline%name = cfg%name
                  call parse_geometry(entries(k), "channels: '"//cfg%name//"'", &
                                      sub_env, g_inline, min_spacing)
                  call append_geometry(this, g_inline)
               end block
               cfg%geom_idx = size(this%geometries)
            else
               do g = 1, size(this%geometries)
                  if (this%geometries(g)%name == gname) cfg%geom_idx = g
               end do
               if (cfg%geom_idx == 0 .and. gname == "field") then
                  ! reserved name: implicit whole-domain geometry
                  block
                     type(type_output_geometry) :: g_field
                     g_field%name = "field"
                     g_field%geom_type = "field"
                     allocate (g_field%x(0), g_field%y(0))
                     call append_geometry(this, g_field)
                  end block
                  cfg%geom_idx = size(this%geometries)
               end if
               if (cfg%geom_idx == 0) then
                  valid = ""
                  do g = 1, size(this%geometries)
                     valid = valid//" "//this%geometries(g)%name
                  end do
                  call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                 "': unknown geometry '"//gname// &
                                                 "' -- defined:"//valid//" field")
               end if
            end if

            call entries(k)%read_string_array("variables", silent=no_key, val=names)
            if (no_key .or. size(names) == 0) call sub_env%log%exit_on_error( &
               "output: channels: '"//cfg%name//"': variables: is required")
            call split_channel_variables(sub_env, cfg, names)

            call entries(k)%read_positive("interval", val=cfg%interval)
            ! t_start accepts a real OR the sentinel `spinup`.  YAML scalars
            ! are text until interpreted, so probe as a string first: a
            ! numeric value simply falls through to the real read below.
            block
               character(:), allocatable :: tstr
               logical :: no_ts_str
               call entries(k)%read_string("t_start", silent=no_ts_str, val=tstr)
               if (.not. no_ts_str) cfg%t_start_spinup = trim(adjustl(tstr)) == "spinup"
            end block
            if (cfg%t_start_spinup) then
               cfg%has_t_start = .true.
            else
               call entries(k)%read("t_start", silent=no_key, val=cfg%t_start)
               cfg%has_t_start = .not. no_key
            end if
            call entries(k)%read("t_end", silent=no_key, val=cfg%t_end)
            cfg%has_t_end = .not. no_key

            ! optional format: absence inherits the deck-format default.
            ! Field-geometry channels take the full set; points stay
            ! ascii/netcdf (groups in diagnostics.nc)
            call entries(k)%read("format", silent=no_key, val=fmt)
            if (.not. no_key) then
               if (this%geometries(cfg%geom_idx)%geom_type == "field") then
                  if (fmt /= "ascii" .and. fmt /= "binary" .and. &
                      fmt /= "netcdf" .and. fmt /= "pnetcdf") &
                     call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                    "': unknown field-channel format '"//fmt// &
                                                    "' -- valid: ascii binary netcdf pnetcdf")
               else if (fmt /= "ascii" .and. fmt /= "netcdf") then
                  call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                 "': unknown format '"//fmt// &
                                                 "' -- valid: ascii netcdf")
               end if
               cfg%format = fmt
            end if

            ! optional precision: single halves binary/netcdf field bytes
            ! (double = the model working precision, default)
            call entries(k)%read_enum("precision", &
                                      [character(6) :: "single", "double"], &
                                      silent=no_key, val=fmt)
            if (.not. no_key) cfg%single_prec = fmt == "single"

            ! statistics presence derives the channel kind (windowed vs
            ! snapshot); validated against the accumulator's stat set
            call entries(k)%read_string_array("statistics", silent=no_stats, val=names)
            if (no_stats) then
               allocate (cfg%statistics(0))
            else
               if (size(names) == 0) call sub_env%log%exit_on_error( &
                  "output: channels: '"//cfg%name//"': statistics: must not be empty")
               call expand_statistics(sub_env, cfg, names)
               cfg%snapshot = .false.
               ! direction is circular: the mean of angles is meaningless --
               ! take the direction OF the mean components instead
               do iv = 1, size(cfg%variables)
                  if (cfg%variables(iv) == "velocity.dir") &
                     call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                    "': statistics on velocity.dir are"// &
                                                    " circular -- derive the direction of the"// &
                                                    " mean components instead")
               end do
            end if
            ! a derived-only channel is windowed even without statistics:
            ! nothing visible remains to snapshot
            if (cfg%n_derived > 0 .and. .not. any(.not. cfg%hidden)) &
               cfg%snapshot = .false.

            call read_threshold(sub_env, entries(k), cfg)
            call entries(k)%read_enum("accumulate", &
                                      [character(7) :: "window", "running", "total"], &
                                      silent=no_key, val=fmt)
            if (.not. no_key) cfg%accum_mode = fmt
            if (cfg%accum_mode /= "window" .and. cfg%n_stats == 0) &
               call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                              "': accumulate: needs statistics:")
         end associate
      end do

      do k = 2, size(this%channels)
         do kk = 1, k - 1
            if (this%channels(k)%name == this%channels(kk)%name) &
               call sub_env%log%exit_on_error("output: channels: duplicate name '"// &
                                              this%channels(k)%name//"'")
         end do
      end do

   end subroutine read_channels

   ! Split the deck variables list: plain and vector-derived names stay
   ! (the builder validates them against the field registry / scratch
   ! set); product-derived names (the catalogue: hsig) move to
   ! cfg%derived, with each source auto-added HIDDEN when not already
   ! requested — accumulated for the product, never written itself.
   ! statistics: names plus presets (envelope = max min max_time, arrival =
   ! first_time, inundation = first_time duration duration_max count),
   ! expanded in order without repeats
   subroutine expand_statistics(sub_env, cfg, names)
      type(type_env), intent(inout) :: sub_env
      type(type_channel_config), intent(inout) :: cfg
      type(type_string), intent(in) :: names(:)

      character(12) :: buf(size(names)*4)
      character(12), allocatable :: expanded(:)
      integer :: iv, n, j

      n = 0
      do iv = 1, size(names)
         select case (trim(names(iv)%s))
         case ("envelope")
            expanded = [character(12) :: "max", "min", "max_time"]
         case ("arrival")
            expanded = [character(12) :: "first_time"]
         case ("inundation")
            expanded = [character(12) :: "first_time", "duration", "duration_max", "count"]
         case default
            if (.not. any(STAT_TYPES == trim(names(iv)%s))) &
               call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                              "': unknown statistic '"//trim(names(iv)%s)// &
                                              "' -- valid: min max mean rms std max_time"// &
                                              " first_time last_time duration duration_max"// &
                                              " count, presets envelope arrival inundation")
            expanded = [character(12) :: trim(names(iv)%s)]
         end select
         do j = 1, size(expanded)
            if (any(buf(1:n) == expanded(j))) cycle
            n = n + 1
            buf(n) = expanded(j)
         end do
      end do
      allocate (cfg%statistics(n))
      cfg%statistics = buf(1:n)
      cfg%n_stats = n
   end subroutine expand_statistics

   ! threshold: {above: v | below: v | magnitude: v}, v a real or a list;
   ! magnitude is above on a .mag speed variable.  gap: and min_duration:
   ! ride beside it.  Required by the event statistics, pointless without
   subroutine read_threshold(sub_env, entry, cfg)
      type(type_env), intent(inout) :: sub_env
      type(type_yaml_reader), intent(inout) :: entry
      type(type_channel_config), intent(inout) :: cfg

      type(type_yaml_reader) :: blk
      character(:), allocatable :: key
      logical :: no_blk, no_key, has_events
      real(SP) :: v
      integer :: iv, j, n_keys

      has_events = .false.
      do iv = 1, cfg%n_stats
         if (any(EVENT_STATS == cfg%statistics(iv))) has_events = .true.
      end do

      blk = entry%cast_dictionary("threshold", no_blk)
      if (no_blk) then
         if (has_events) call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                        "': first_time/last_time/duration/"// &
                                                        "duration_max/count need threshold:"// &
                                                        " {above | below | magnitude: <value>}")
         allocate (cfg%thresholds(0))
         return
      end if
      if (.not. has_events) call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                           "': threshold: needs an event statistic"// &
                                                           " (first_time last_time duration"// &
                                                           " duration_max count)")

      n_keys = 0
      if (blk%has_key("above")) then
         n_keys = n_keys + 1; key = "above"; cfg%thr_dir = 1
      end if
      if (blk%has_key("below")) then
         n_keys = n_keys + 1; key = "below"; cfg%thr_dir = -1
      end if
      if (blk%has_key("magnitude")) then
         n_keys = n_keys + 1; key = "magnitude"; cfg%thr_dir = 1
         do iv = 1, size(cfg%variables)
            if (index(cfg%variables(iv), ".mag") == 0) &
               call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                              "': threshold: magnitude: applies to a .mag"// &
                                              " speed variable (velocity.mag), not '"// &
                                              trim(cfg%variables(iv))//"'")
         end do
      end if
      if (n_keys /= 1) call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                      "': threshold: takes exactly one of"// &
                                                      " above: below: magnitude:")

      if (blk%is_list(key)) then
         call blk%read_real_array(key, val=cfg%thresholds)
      else
         call blk%read(key, val=v)
         cfg%thresholds = [v]
      end if
      if (size(cfg%thresholds) == 0) call sub_env%log%exit_on_error( &
         "output: channels: '"//cfg%name//"': threshold: "//key//": must not be empty")
      do iv = 2, size(cfg%thresholds)
         do j = 1, iv - 1
            if (cfg%thresholds(iv) == cfg%thresholds(j)) call sub_env%log%exit_on_error( &
               "output: channels: '"//cfg%name//"': threshold: "//key//": repeated value")
         end do
      end do

      call entry%read_nonnegative("gap", silent=no_key, val=cfg%gap, default="0.0")
      call entry%read_nonnegative("min_duration", silent=no_key, val=cfg%min_duration, &
                                  default="0.0")
   end subroutine read_threshold

   subroutine split_channel_variables(sub_env, cfg, names)
      type(type_env), intent(inout) :: sub_env
      type(type_channel_config), intent(inout) :: cfg
      type(type_string), intent(in) :: names(:)

      character(32) :: vars(size(names) + size(PROD_DERIVED))
      logical :: hid(size(names) + size(PROD_DERIVED))
      character(8) :: der(size(PROD_DERIVED))
      integer :: iv, k, ip, nv, nd

      nv = 0
      nd = 0
      do iv = 1, size(names)
         if (len_trim(names(iv)%s) > 32) &
            call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                           "': variable name too long: "//trim(names(iv)%s))
         ip = 0
         do k = 1, size(PROD_DERIVED)
            if (trim(names(iv)%s) == trim(PROD_DERIVED(k))) ip = k
         end do
         if (ip > 0) then
            if (any(der(1:nd) == PROD_DERIVED(ip))) &
               call sub_env%log%exit_on_error("output: channels: '"//cfg%name// &
                                              "': duplicate variable "//trim(PROD_DERIVED(ip)))
            nd = nd + 1
            der(nd) = PROD_DERIVED(ip)
         else
            nv = nv + 1
            vars(nv) = trim(names(iv)%s)
            hid(nv) = .false.
         end if
      end do
      do k = 1, nd
         do ip = 1, size(PROD_DERIVED)
            if (der(k) == PROD_DERIVED(ip)) exit
         end do
         if (.not. any(vars(1:nv) == PROD_SRC(ip))) then
            nv = nv + 1
            vars(nv) = PROD_SRC(ip)
            hid(nv) = .true.
         end if
      end do

      allocate (cfg%variables(nv), cfg%hidden(nv), cfg%derived(nd))
      cfg%variables = vars(1:nv)
      cfg%hidden = hid(1:nv)
      cfg%derived = der(1:nd)
      cfg%n_derived = nd

   end subroutine split_channel_variables

   subroutine reject_moved_key(sub_env, old_key, new_home)
      type(type_env), intent(inout) :: sub_env
      character(*), intent(in) :: old_key, new_home

      character(:), allocatable :: tmp
      logical :: no_key

      call sub_env%yaml%read(old_key, silent=no_key, val=tmp)
      if (.not. no_key) call sub_env%log%exit_on_error( &
         "output: "//old_key//" moved -- set "//new_home)

   end subroutine reject_moved_key

   ! list-valued retired key (reject_moved_key reads scalars only)
   subroutine reject_string_list_key(sub_env, old_key, new_home)
      type(type_env), intent(inout) :: sub_env
      character(*), intent(in) :: old_key, new_home

      type(type_string), allocatable :: tmp(:)
      logical :: no_key

      call sub_env%yaml%read_string_array(old_key, silent=no_key, val=tmp)
      if (.not. no_key) call sub_env%log%exit_on_error( &
         "output: "//old_key//" retired -- list "//new_home)

   end subroutine reject_string_list_key

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
