
module stations_mod
   use param, only: SP
   use, intrinsic :: ieee_arithmetic
   implicit none
   private
   public :: stations_type, stations_init, stations_flush, stations_append

# if defined (FOAM)
   integer, parameter :: SUBSIZE = 7
   character(*), parameter :: ASCII_FMT = '(E21.10E4, 6E16.5E4)'
# else
   integer, parameter :: SUBSIZE = 4
   character(*), parameter :: ASCII_FMT = '(E21.10E4, 3E16.5E4)'
# endif

   ! Define the derived type (class)
   type :: stations_type
      character(len=80) :: dir_path
      integer, dimension(:), allocatable :: i, j, k
      real(SP), dimension(:, :, :), allocatable:: buffer
      integer :: n, l, buffer_size
      logical :: is_binary
   contains
      procedure append => stations_append
      procedure flush => stations_flush
      procedure open_file => stations_open_file
   end type stations_type

   ! Interface block to act as the constructor
   interface stations_type
      module procedure stations_init
   end interface stations_type

contains

   ! Constructor function: returns an instance of stations_type
   function stations_init(dir_path, sta_path, total_nsta, buffer_size, is_binary, &
                          Ibeg, Iend, iista, Jbeg, Jend, jjsta, Nghost) result(this)
      type(stations_type) :: this
      character(len=80), intent(in) :: dir_path
      character(len=80), intent(in) :: sta_path
      integer, intent(in) :: total_nsta
      integer, intent(in) :: buffer_size
      logical, intent(in) :: is_binary
      integer, intent(in) :: Ibeg, Iend, iista, Jbeg, Jend, jjsta, Nghost
      integer :: dum1, dum2, nsta_loc, i, j, unit
      integer, dimension(:), allocatable :: ista, jsta
      logical, dimension(:), allocatable :: flag

      this%dir_path = dir_path
      this%is_binary = is_binary
      this%buffer_size = buffer_size

      open (100, file=trim(sta_path))

      allocate (ista(total_nsta))
      allocate (jsta(total_nsta))
      allocate (flag(total_nsta))

      this%n = 0
      do i = 1, total_nsta
         read (100, *) dum1, dum2
# if defined (CARTESIAN) || defined (SPHERICAL_IJ_STATION)
# if defined (PARALLEL)
         ista(i) = Nghost + dum1 - (iista - 1)
         jsta(i) = Nghost + dum2 - (jjsta - 1)
# else
         ista(i) = Nghost + dum1
         jsta(i) = Nghost + dum2
# endif
# else
# if defined (PARALLEL)
         ista(i) = Nghost + 1 + nint((dum2 - Lon_West)/Dphi) &
                   - (iista - 1)
         jsta(i) = Nghost + 1 + nint((dum1 - Lat_South)/Dtheta) &
                   - (jjsta - 1)
# else
         ista(i) = Nghost + 1 + nint((dum2 - Lon_West)/Dphi)
         jsta(i) = Nghost + 1 + nint((dum1 - Lat_South)/Dtheta)
# endif
# endif
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            flag(i) = .true.
            this%n = this%n + 1
         else
            flag(i) = .false.
         end if
      end do

      if (this%n > 0) then

         allocate (this%i(this%n))
         allocate (this%j(this%n))
         allocate (this%k(this%n))

         allocate (this%buffer(SUBSIZE, buffer_size, this%n))
         j = 0
         do i = 1, total_nsta

            if (flag(i)) then
               j = j + 1
               this%i(j) = ista(i)
               this%j(j) = jsta(i)
               this%k(j) = i
               this%l = 0
            end if

            ! Clearing old file data if it exists
            call this%open_file(i, unit, is_new=.true.)
            close (unit)
         end do

      end if

      deallocate (ista, jsta, flag)
      close (100)

   end function stations_init

# if defined (FOAM)
   subroutine stations_append(this, time, mask, eta, u, v, &
                              eddy, eta_foam, eta_foam_max)
      class(stations_type), intent(in) :: this
      real(SP), intent(in) :: time
      integer, dimension(:, :), allocatable, intent(in) :: mask
      real(SP), dimension(:, :), allocatable, intent(in) :: eta, u, v
      real(SP), dimension(:, :), allocatable, intent(in) :: eddy, eta_foam, eta_foam_max
      integer :: i

   end subroutine stations_append
# else
   subroutine stations_append(this, time, mask, eta, u, v)
      class(stations_type), intent(inout) :: this
      real(SP), intent(in) :: time
      integer, dimension(:, :), allocatable, intent(in) :: mask
      real(SP), dimension(:, :), allocatable, intent(in) :: eta, u, v
      integer :: i, j, k
      real :: nan_val

      if (this%n < 1) return

      nan_val = ieee_value(nan_val, ieee_quiet_nan)
      this%l = this%l + 1

      do k = 1, this%n
         this%buffer(1, this%l, k) = time
         i = this%i(k)
         j = this%j(k)

         if (mask(j, i) < 1) then
            do i = 2, SUBSIZE
               this%buffer(i, this%l, k) = nan_val
            end do
         else
            this%buffer(2, this%l, k) = eta(j, i)
            this%buffer(3, this%l, k) = u(j, i)
            this%buffer(4, this%l, k) = v(j, i)
         end if
      end do

      if (this%l >= this%buffer_size) then
         call this%flush()
         this%l = 1
      end if

   end subroutine stations_append
# endif

   subroutine stations_open_file(this, k, unit, is_new)

      class(stations_type), intent(inout) :: this
      integer, intent(in) :: k
      integer, intent(out) :: unit
      logical, optional, intent(in) :: is_new
      character(:), allocatable :: fpath
      character(10):: index, position
      logical :: is_new_local

      if (present(is_new)) then
         is_new_local = is_new
      else
         is_new_local = .false.
      end if

      unit = 100 + this%k(k)
      if (this%is_binary) then
         write (index(1:5), '(I5.5)') this%k(k)
         fpath = trim(this%dir_path)//"sta_"//trim(index)

         if (is_new_local) then
            open (unit, file=fpath, access='stream', form='unformatted', status='replace')
         else
            open (unit, file=fpath, access='stream', form='unformatted', status='old', position='append')
         end if
      else
         fpath = trim(this%dir_path)//"sta_"//trim(index)
         write (index(1:4), '(I4.4)') this%k(k)

         if (is_new_local) then
            open (unit, file=fpath, form='formatted', status='replace')
         else
            open (unit, file=fpath, form='formatted', status='old', position='append')
         end if
      end if

      deallocate (fpath)

   end subroutine stations_open_file

   subroutine stations_flush(this)
      class(stations_type), intent(inout) :: this
      integer :: unit, i, k, l

      do k = 1, this%n

         call this%open_file(k, unit)

         if (this%is_binary) then
            write (unit) this%buffer(1:SUBSIZE, 1:this%l, k)
         else
            write (unit, ASCII_FMT) this%buffer(1:SUBSIZE, 1:this%l, k)
         end if

         close (unit)
      end do

      this%l = 0

   end subroutine stations_flush

end module stations_mod

