!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Station time series — port of legacy STATIONS (old/io.F, the
!  CARTESIAN variant).  Nearest-cell sampling (no interpolation) at
!  global grid indices read from stations_file, one sta_NNNN file
!  per station (1-based, 4-digit), written by the owning rank only.
!
!  Legacy quirks kept:
!    - buffered rows flush only when the buffer FILLS or TIME hits
!      TOTAL_TIME; the sample arriving on a flush call is DROPPED and
!      the end-of-run flush pads the tail with (TOTAL_TIME, 0, 0, 0)
!      rows (legacy prefill) — always exactly buffer_size rows out
!    - dry cells (mask < 1) record zeros
!    - station coordinates are GLOBAL interior indices read as reals
!      and truncated
!    - cadence is the PLOT_COUNT_STATION dt-accumulator, firing at
!      t = 0 and NOT gated by t_start/PLOT_START_TIME
!
!  HISTORY :
!    07/10/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_stations_mod

   use core_constants_mod, only: SP, N_GHOST
   use core_grid_mod, only: type_grid_2d
   use core_env_mod, only: type_env
   use core_time_utils_mod, only: type_timing_control

   use model_fields_2d_mod, only: type_fields_2d

   implicit none

   private
   public :: type_model_stations

   type :: type_model_stations

      integer :: n_stations = 0
      integer :: buffer_size = 0
      real(SP) :: total_time = 0.0_SP

      type(type_timing_control) :: trigger

      integer, allocatable :: ista(:), jsta(:)  ! local ghost-inclusive
      logical, allocatable :: own(:)
      integer, allocatable :: unit(:)
      integer, allocatable :: buffer_count(:)
      real(SP), allocatable :: buffer(:, :, :)   ! (buffer_size, n, 4)
      logical :: closed = .false.

      type(type_fields_2d), pointer :: fields => null()

   contains
      procedure :: init_compute => stations_init_compute
      procedure :: update => stations_update
      procedure :: finish => stations_finish
      procedure :: free => stations_free
   end type type_model_stations

contains

   subroutine stations_init_compute(this, grid, env, fields, enabled, &
                                    stations_file, result_folder, &
                                    plot_intv_station, buffer_size, total_time)
      class(type_model_stations), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_env), intent(inout) :: env
      type(type_fields_2d), intent(inout), target :: fields
      logical, intent(in) :: enabled
      integer, intent(in) :: buffer_size
      character(*), intent(in) :: stations_file, result_folder
      real(SP), intent(in) :: plot_intv_station, total_time

      character(:), allocatable :: folder
      character(4) :: snum
      character(12) :: line_str
      real(SP) :: dum1, dum2
      logical :: file_exist
      integer :: i, ios, funit, n_stations

      this%n_stations = 0
      if (.not. enabled) return

      this%buffer_size = buffer_size
      this%total_time = total_time
      this%fields => fields

      this%trigger%t_start = 0.0_SP
      this%trigger%interval = plot_intv_station
      this%trigger%last_triggered = -1.0_SP

      inquire (file=trim(stations_file), exist=file_exist)
      if (.not. file_exist) then
         call env%log%exit_on_error( &
            "output: stations: file cannot be found: "//trim(stations_file))
      end if

      ! station count = line count (nee number_stations); a parse failure
      ! before EOF is a malformed line, not a short count.  The returns keep
      ! non-io ranks out of the later reads while the io rank aborts.
      open (newunit=funit, file=trim(stations_file), status="old", action="read")
      n_stations = 0
      do
         read (funit, *, iostat=ios) dum1, dum2
         if (ios /= 0) exit
         n_stations = n_stations + 1
      end do
      if (ios > 0) then
         write (line_str, '(I0)') n_stations + 1
         call env%log%exit_on_error( &
            "output: stations: cannot parse an 'i j' pair on line "// &
            trim(line_str)//" of "//trim(stations_file))
         return
      end if
      rewind (funit)
      if (n_stations == 0) then
         call env%log%exit_on_error( &
            "output: stations: "//trim(stations_file)//" contains no stations")
         return
      end if
      this%n_stations = n_stations

      folder = trim(result_folder)
      if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"

      allocate (this%ista(n_stations), this%jsta(n_stations), &
                this%own(n_stations), this%unit(n_stations), &
                this%buffer_count(n_stations))
      allocate (this%buffer(buffer_size, n_stations, 4))
      this%buffer_count = 0
      ! legacy leaves the buffer uninitialised until first fill; the
      ! flush prefill values are what an end-of-run pad row contains
      this%buffer(:, :, 1) = total_time
      this%buffer(:, :, 2:4) = 0.0_SP

      ! Global interior indices -> local ghost-inclusive; a rank owns a
      ! station when it falls inside its interior range (legacy ykchoi
      ! iista/jjsta form; grid%ibegin is that 1-based global start).
      do i = 1, n_stations
         read (funit, *) dum1, dum2
         this%ista(i) = N_GHOST + int(dum1) - (grid%ibegin - 1)
         this%jsta(i) = N_GHOST + int(dum2) - (grid%jbegin - 1)
         this%own(i) = this%ista(i) >= grid%lp%ib .and. this%ista(i) <= grid%lp%ie &
                       .and. this%jsta(i) >= grid%lp%jb .and. this%jsta(i) <= grid%lp%je
         if (this%own(i)) then
            write (snum, '(I4.4)') i
            open (newunit=this%unit(i), file=folder//"sta_"//snum, &
                  status="replace", action="write")
         end if
      end do
      close (funit)

   end subroutine stations_init_compute

   ! Loop-top sampling at the PLOT_COUNT_STATION cadence.
   subroutine stations_update(this, t, dt)
      class(type_model_stations), intent(inout) :: this
      real(SP), intent(in) :: t, dt

      real(SP) :: eta_sta, u_sta, v_sta
      integer :: i, j, k

      if (this%n_stations <= 0 .or. this%closed) return
      if (.not. this%trigger%should_trigger(t, dt)) return

      associate (f => this%fields)
         do i = 1, this%n_stations
            if (.not. this%own(i)) cycle

            if (f%mask(this%ista(i), this%jsta(i)) < 1) then
               eta_sta = 0.0_SP
               u_sta = 0.0_SP
               v_sta = 0.0_SP
            else
               eta_sta = f%eta(this%ista(i), this%jsta(i))
               u_sta = f%u(this%ista(i), this%jsta(i))
               v_sta = f%v(this%ista(i), this%jsta(i))
            end if

            if (this%buffer_count(i) < this%buffer_size &
                .and. t < this%total_time) then
               this%buffer_count(i) = this%buffer_count(i) + 1
               this%buffer(this%buffer_count(i), i, 1) = t
               this%buffer(this%buffer_count(i), i, 2) = eta_sta
               this%buffer(this%buffer_count(i), i, 3) = u_sta
               this%buffer(this%buffer_count(i), i, 4) = v_sta
            else
               ! legacy flush: all buffer_size rows, current sample dropped
               do j = 1, this%buffer_size
                  write (this%unit(i), '(E21.10E4, 3E16.5E4)') &
                     (this%buffer(j, i, k), k=1, 4)
               end do
               this%buffer_count(i) = 0
               this%buffer(:, i, 1) = this%total_time
               this%buffer(:, i, 2:4) = 0.0_SP
            end if
         end do
      end associate

   end subroutine stations_update

   ! Legacy post-loop STATIONS call (main.F after the time loop):
   ! flush the residual buffer — the slots beyond buffer_count still
   ! hold the (TOTAL_TIME, 0, 0, 0) prefill, giving legacy's padded
   ! tail — and close the files.
   subroutine stations_finish(this)
      class(type_model_stations), intent(inout) :: this
      integer :: i, j, k

      if (this%n_stations <= 0 .or. this%closed) return
      do i = 1, this%n_stations
         if (.not. this%own(i)) cycle
         do j = 1, this%buffer_size
            write (this%unit(i), '(E21.10E4, 3E16.5E4)') &
               (this%buffer(j, i, k), k=1, 4)
         end do
         close (this%unit(i))
      end do
      this%closed = .true.

   end subroutine stations_finish

   subroutine stations_free(this)
      class(type_model_stations), intent(inout) :: this
      integer :: i

      if (this%n_stations > 0 .and. .not. this%closed) then
         do i = 1, this%n_stations
            if (this%own(i)) close (this%unit(i))
         end do
         this%closed = .true.
      end if
      if (allocated(this%ista)) deallocate (this%ista, this%jsta, this%own, &
                                            this%unit, this%buffer_count, &
                                            this%buffer)
      this%fields => null()
      this%n_stations = 0

   end subroutine stations_free

end module model_stations_mod
