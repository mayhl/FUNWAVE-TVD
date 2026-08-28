!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Time-series file reader with linear-in-time interpolation.
!
!  One correct implementation of the "timestamped records -> 2-slot bracket
!  -> blend in time" pattern that the forcing modules (tide, meteo, wind,
!  precipitation, vessel) each re-implemented.  A consumer opens the file
!  with its field count, then samples at the model time every step; the
!  reader advances its own bracket lazily.
!
!  Semantics (the fixes the copy-paste versions miss):
!    1. Advance-until-caught-up: reads records until the high bracket
!       reaches the query time, sliding the LOW bracket on every read so
!       [t1, t2] stays the tightest pair around the query (a slide-once
!       advance leaves t1 stale after a multi-record jump).
!    2. Freeze-on-EOF: past the last record the bracket freezes at it and
!       eof is raised once; the consumer decides whether to warn.
!    3. Clamped weight: the blend fraction is clamped to [0, 1], so a query
!       outside the bracket (after EOF, or when record spacing < dt) holds
!       the nearest record instead of extrapolating.
!
module core_time_series_mod
   use core_constants_mod, only: SP

   implicit none
   private

   public :: type_time_series
   public :: interp_weight

   type :: type_time_series
      integer :: unit = -1
      integer :: nfield = 0
      real(SP) :: t1 = 0.0_SP, t2 = 0.0_SP  ! bracket times, t1 <= t2
      real(SP), allocatable :: f1(:), f2(:)  ! bracket field values (nfield)
      logical :: eof = .false.
   contains
      procedure :: open => ts_open
      procedure :: attach => ts_attach
      procedure :: sample => ts_sample
      procedure :: rate => ts_rate
      procedure :: close => ts_close
   end type type_time_series

contains

   ! Open the file, optionally skip one header line, and seed both bracket
   ! slots from the first record ("t f(1) .. f(nfield)" per line).
   subroutine ts_open(this, fname, nfield, has_header)
      class(type_time_series), intent(inout) :: this
      character(*), intent(in) :: fname
      integer, intent(in) :: nfield
      logical, intent(in) :: has_header

      character(len=1) :: dummy

      this%nfield = nfield
      this%eof = .false.
      allocate (this%f1(nfield), this%f2(nfield))

      open (newunit=this%unit, file=fname, status='old', action='read')
      if (has_header) read (this%unit, '(A)') dummy
      read (this%unit, *) this%t2, this%f2
      this%t1 = this%t2
      this%f1 = this%f2
   end subroutine ts_open

   ! Adopt an already-open unit positioned at its second record, seeding both
   ! bracket slots from the caller-supplied first record (t0, f0).  For files
   ! whose series follows a preamble the caller reads itself, e.g. a vessel
   ! track after the hull and propeller lines.
   subroutine ts_attach(this, unit, t0, f0)
      class(type_time_series), intent(inout) :: this
      integer, intent(in) :: unit
      real(SP), intent(in) :: t0, f0(:)

      this%unit = unit
      this%nfield = size(f0)
      this%eof = .false.
      allocate (this%f1(this%nfield), this%f2(this%nfield))
      this%t2 = t0; this%f2 = f0
      this%t1 = t0; this%f1 = f0
   end subroutine ts_attach

   ! Interpolate the record fields at query time tq into out(1:nfield),
   ! advancing the bracket as needed (see the header semantics).
   subroutine ts_sample(this, tq, out)
      class(type_time_series), intent(inout) :: this
      real(SP), intent(in) :: tq
      real(SP), intent(out) :: out(:)

      real(SP) :: tt, ff(this%nfield), frac
      integer :: ios

      do while (this%t2 < tq .and. .not. this%eof)
         read (this%unit, *, iostat=ios) tt, ff
         if (ios /= 0) then
            this%eof = .true.
            exit
         end if
         this%t1 = this%t2; this%f1 = this%f2   ! slide low <- high
         this%t2 = tt; this%f2 = ff             ! new high record
      end do

      if (this%t2 > this%t1) then
         frac = interp_weight(tq, this%t1, this%t2)
      else
         frac = 1.0_SP                          ! collapsed bracket -> latest
      end if
      out = this%f1*(1.0_SP - frac) + this%f2*frac
   end subroutine ts_sample

   ! Clamped linear blend fraction of xq within [x1, x2], in [0, 1] so a
   ! query outside the bracket holds the nearest endpoint (no
   ! extrapolation).  The shared interpolation contract for the time
   ! bracket here and the alongshore boundary-spectrum blend; a collapsed
   ! bracket (x2 <= x1) returns 0 (the low endpoint).
   pure function interp_weight(xq, x1, x2) result(frac)
      real(SP), intent(in) :: xq, x1, x2
      real(SP) :: frac

      if (x2 > x1) then
         frac = max(0.0_SP, min(1.0_SP, (xq - x1)/(x2 - x1)))
      else
         frac = 0.0_SP
      end if
   end function interp_weight

   ! Current-segment slope d(field)/dt = (f2 - f1)/(t2 - t1), zero on a
   ! collapsed bracket (single record, before the first, or EOF-frozen).
   ! Reflects the bracket left by the most recent sample; call after sample.
   subroutine ts_rate(this, out)
      class(type_time_series), intent(in) :: this
      real(SP), intent(out) :: out(:)

      if (this%t2 > this%t1) then
         out = (this%f2 - this%f1)/(this%t2 - this%t1)
      else
         out = 0.0_SP
      end if
   end subroutine ts_rate

   subroutine ts_close(this)
      class(type_time_series), intent(inout) :: this
      if (this%unit >= 0) then
         close (this%unit)
         this%unit = -1
      end if
      if (allocated(this%f1)) deallocate (this%f1, this%f2)
   end subroutine ts_close

end module core_time_series_mod
