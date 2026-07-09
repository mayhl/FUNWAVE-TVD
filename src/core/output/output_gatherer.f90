!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output gatherer: MPI_Gatherv wrapper for assembling
!  distributed output data on the IO rank.
!
!  Two modes (one per instance, chosen by which init is called):
!   - Points (station/transect): gathers n_local interpolated scalar
!     values from each rank into a global n_global array on IO rank.
!     init_points sets up recv_counts/displs by exchanging n_local and
!     optionally gathers global point ids so the caller can restore
!     point order (gathered data arrives in rank order).
!
!   - Field (2D subdomain): init_field exchanges per-rank interior
!     extents; gather_field assembles the global (M, N) interior array
!     on the IO rank from flattened subdomain blocks.
!
!  recv_counts/recv_displs are allocated on ALL ranks so MPI calls
!  are always valid (non-root values are ignored by MPI_Gatherv).
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_output_gatherer_mod
   use core_constants_mod, only: SP, MPI_SP
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use mpi_f08
   implicit none

   private
   public :: type_output_gatherer

   type :: type_output_gatherer
      integer :: n_global = 0
      integer :: n_local = 0
      integer :: io_rank = 0
      ! Field mode: global interior dims + per-rank interior extents.
      ! Extent arrays hold valid data on the IO rank only.
      integer :: M = 0, N = 0
      integer, allocatable :: ib_all(:), ie_all(:)
      integer, allocatable :: jb_all(:), je_all(:)
      ! Points mode: global point ids in rank-gather order (IO rank only).
      ! gathered(k) is the value at point point_ids(k).
      integer, allocatable :: point_ids(:)
      ! MPI_Gatherv params — allocated on ALL ranks; non-root values ignored by MPI
      integer, allocatable :: recv_counts(:)
      integer, allocatable :: recv_displs(:)
   contains
      procedure :: init_points => gatherer_init_points
      procedure :: init_field => gatherer_init_field
      procedure :: gather_vals => gatherer_gather_vals
      procedure :: gather_field => gatherer_gather_field
      procedure :: finalize => gatherer_finalize
   end type type_output_gatherer

contains

   ! Set up Gatherv parameters for n_local points on this rank.
   ! Call after interp%init so n_local is known.
   ! Pass local_ids (interp%point_id; size n_local, possibly 0) to have
   ! the IO rank record the global id of each gathered slot in point_ids.
   subroutine gatherer_init_points(this, n_global, n_local, comm, local_ids)
      class(type_output_gatherer), intent(inout) :: this
      integer, intent(in)    :: n_global, n_local
      type(type_comm), intent(inout) :: comm
      integer, intent(in), optional :: local_ids(:)

      integer :: k, ierr
      integer, allocatable :: all_counts(:)

      this%n_global = n_global
      this%n_local = n_local
      this%io_rank = comm%get_io_rank()

      ! Exchange n_local from every rank so IO rank knows recv layout.
      ! All ranks allocate all_counts; non-IO values are unused.
      allocate (all_counts(comm%size), source=0)
      call MPI_Gather(n_local, 1, MPI_INTEGER, &
                      all_counts, 1, MPI_INTEGER, &
                      this%io_rank, comm%id, ierr)

      ! Build recv_counts and recv_displs on ALL ranks (MPI_Gatherv
      ! ignores them on non-root, but they must be allocated to be safe).
      allocate (this%recv_counts(comm%size), source=0)
      allocate (this%recv_displs(comm%size), source=0)
      if (comm%is_io_node()) then
         this%recv_counts = all_counts
         this%recv_displs(1) = 0
         do k = 2, comm%size
            this%recv_displs(k) = this%recv_displs(k - 1) + this%recv_counts(k - 1)
         end do
      end if

      deallocate (all_counts)

      if (present(local_ids)) then
         allocate (this%point_ids(merge(n_global, 1, comm%is_io_node())), source=0)
         call MPI_Gatherv(local_ids, n_local, MPI_INTEGER, &
                          this%point_ids, this%recv_counts, this%recv_displs, MPI_INTEGER, &
                          this%io_rank, comm%id, ierr)
      end if
   end subroutine gatherer_init_points

   ! Set up Gatherv parameters for full-field output: exchange per-rank
   ! interior extents so the IO rank can place each flattened subdomain
   ! block into the global (M, N) interior array.
   subroutine gatherer_init_field(this, grid, comm)
      class(type_output_gatherer), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid
      type(type_comm), intent(inout) :: comm

      integer :: k, ierr
      integer :: exts(4)
      integer, allocatable :: all_exts(:, :)

      this%M = grid%M
      this%N = grid%N
      this%n_global = grid%M*grid%N
      this%n_local = grid%local_nx*grid%local_ny
      this%io_rank = comm%get_io_rank()

      exts = [grid%ibegin, grid%istop, grid%jbegin, grid%jstop]
      allocate (all_exts(4, comm%size), source=0)
      call MPI_Gather(exts, 4, MPI_INTEGER, &
                      all_exts, 4, MPI_INTEGER, &
                      this%io_rank, comm%id, ierr)

      allocate (this%recv_counts(comm%size), source=0)
      allocate (this%recv_displs(comm%size), source=0)
      allocate (this%ib_all(comm%size), source=0)
      allocate (this%ie_all(comm%size), source=0)
      allocate (this%jb_all(comm%size), source=0)
      allocate (this%je_all(comm%size), source=0)
      if (comm%is_io_node()) then
         this%ib_all = all_exts(1, :)
         this%ie_all = all_exts(2, :)
         this%jb_all = all_exts(3, :)
         this%je_all = all_exts(4, :)
         this%recv_counts = (this%ie_all - this%ib_all + 1) &
                            *(this%je_all - this%jb_all + 1)
         this%recv_displs(1) = 0
         do k = 2, comm%size
            this%recv_displs(k) = this%recv_displs(k - 1) + this%recv_counts(k - 1)
         end do
      end if

      deallocate (all_exts)
   end subroutine gatherer_init_field

   ! Gather local_vals (size n_local) from all ranks into global_out
   ! (size n_global) on IO rank. Non-IO ranks may pass a size-1 dummy global_out.
   ! Point ordering in global_out follows rank order, not point_id order;
   ! re-sorting by point_ids is the caller's responsibility.
   subroutine gatherer_gather_vals(this, local_vals, global_out, comm)
      class(type_output_gatherer), intent(in)    :: this
      real(SP), intent(in)    :: local_vals(:)
      real(SP), intent(inout) :: global_out(:)
      type(type_comm), intent(inout) :: comm

      integer :: ierr

      call MPI_Gatherv(local_vals, this%n_local, MPI_SP, &
                       global_out, this%recv_counts, this%recv_displs, MPI_SP, &
                       this%io_rank, comm%id, ierr)
   end subroutine gatherer_gather_vals

   ! Assemble the global interior field on the IO rank.
   ! local_vals is this rank's interior slice, shape (local_nx, local_ny);
   ! global_out must be (M, N) on the IO rank (a (1,1) dummy elsewhere).
   subroutine gatherer_gather_field(this, local_vals, global_out, comm)
      class(type_output_gatherer), intent(in)    :: this
      real(SP), intent(in)    :: local_vals(:, :)
      real(SP), intent(inout) :: global_out(:, :)
      type(type_comm), intent(inout) :: comm

      integer :: k, nx, ny, off, ierr
      real(SP), allocatable :: sendbuf(:), recvbuf(:)

      sendbuf = reshape(local_vals, [this%n_local])
      allocate (recvbuf(merge(this%n_global, 1, comm%is_io_node())))

      call MPI_Gatherv(sendbuf, this%n_local, MPI_SP, &
                       recvbuf, this%recv_counts, this%recv_displs, MPI_SP, &
                       this%io_rank, comm%id, ierr)

      if (comm%is_io_node()) then
         do k = 1, size(this%recv_counts)
            nx = this%ie_all(k) - this%ib_all(k) + 1
            ny = this%je_all(k) - this%jb_all(k) + 1
            off = this%recv_displs(k)
            global_out(this%ib_all(k):this%ie_all(k), &
                       this%jb_all(k):this%je_all(k)) = &
               reshape(recvbuf(off + 1:off + nx*ny), [nx, ny])
         end do
      end if
   end subroutine gatherer_gather_field

   subroutine gatherer_finalize(this)
      class(type_output_gatherer), intent(inout) :: this
      if (allocated(this%recv_counts)) deallocate (this%recv_counts)
      if (allocated(this%recv_displs)) deallocate (this%recv_displs)
      if (allocated(this%ib_all)) deallocate (this%ib_all)
      if (allocated(this%ie_all)) deallocate (this%ie_all)
      if (allocated(this%jb_all)) deallocate (this%jb_all)
      if (allocated(this%je_all)) deallocate (this%je_all)
      if (allocated(this%point_ids)) deallocate (this%point_ids)
      this%n_global = 0
      this%n_local = 0
      this%M = 0
      this%N = 0
   end subroutine gatherer_finalize

end module core_output_gatherer_mod
