module core_grid_mod
   use core_constants_mod, only: SP
   use core_yaml_file_mod, only: type_yaml_reader
   use core_grid_interface_mod, only: abstract_grid
   implicit none

   ! Concrete 2D Grid implementation
   type, extends(abstract_grid), public :: type_grid_2d
      integer :: M, N
      ! Domain decomposition
      integer :: nx_proc = 1, ny_proc = 1
      ! Indices for local compute domain
      integer :: ibegin, istop, jbegin, jstop
      ! Indices for domain including ghost cells
      integer :: ig_begin, ig_stop, jg_begin, jg_stop
      integer :: local_nx, local_ny
   contains
      procedure, public :: get_indices => get_indices_2d
      procedure, public :: decompose
   end type type_grid_2d

contains

   subroutine decompose(this, nprocs)
      class(type_grid_2d), intent(inout) :: this
      integer, intent(in) :: nprocs
      call compute_optimal_grid_size(nprocs, this%M, this%N, this%nx_proc, this%ny_proc)
   end subroutine decompose

   subroutine compute_optimal_grid_size(nproc, nx, ny, px, py)
      integer, intent(in) :: nx, ny, nproc
      integer, intent(out) :: px, py
      integer, allocatable :: factors(:)
      integer :: nfactors, i, min_i
      real(SP) :: ratio, nx_loc, ny_loc, min_ratio

      call get_factors(nproc, factors, nfactors)
      min_ratio = 9e10_sp
      do i = 1, nfactors
         nx_loc = (1.0_sp*nx)/(1.0_sp*factors(i))
         ny_loc = (1.0_sp*ny)/((1.0_sp*nproc)/(1.0_sp*factors(i)))
         if (nx_loc > ny_loc) then
            ratio = nx_loc/ny_loc
         else
            ratio = ny_loc/nx_loc
         end if
         if (ratio < min_ratio) then
            min_i = i
            min_ratio = ratio
         end if
      end do
      px = factors(min_i)
      py = nproc/factors(min_i)
   end subroutine compute_optimal_grid_size

   subroutine get_factors(n, factors, nfactors)
      integer, intent(in) :: n
      integer, allocatable, intent(out) :: factors(:)
      integer, intent(out) :: nfactors
      integer :: i, limit, count
      integer, allocatable :: temp(:)
      if (n <= 0) then
         nfactors = 0
         allocate (factors(0))
         return
      end if
      limit = int(sqrt(real(n)))
      allocate (temp(2*limit))
      count = 0
      do i = 1, limit
         if (mod(n, i) == 0) then
            count = count + 1
            temp(count) = i
            if (i /= n/i) then
               count = count + 1
               temp(count) = n/i
            end if
         end if
      end do
      nfactors = count
      allocate (factors(nfactors))
      factors = temp(1:nfactors)
      deallocate (temp)
   end subroutine get_factors

   subroutine get_indices_2d(this, indices)
      class(type_grid_2d), intent(inout) :: this
      integer, allocatable, intent(out) :: indices(:, :)
      integer :: i, j, k

      allocate (indices(2, this%M*this%N))
      k = 1
      do j = 1, this%N
         do i = 1, this%M
            indices(1, k) = i
            indices(2, k) = j
            k = k + 1
         end do
      end do
      this%n_points = k - 1
   end subroutine get_indices_2d

end module core_grid_mod
