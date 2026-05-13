!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output gatherer: MPI_Gatherv wrapper for assembling
!  distributed output data on the IO rank.
!
!  Two modes:
!   - Points (station/transect): gathers n_local interpolated scalar
!     values from each rank into a global n_global array on IO rank.
!     init_points sets up recv_counts/displs by exchanging n_local.
!
!   - Field (2D subdomain): TODO — row-by-row MPI_Gatherv or
!     MPI_Type_subarray for assembling (M, N) from subdomains.
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
   use core_comm_mod,      only: type_comm
   use mpi_f08
   implicit none(external)

   private
   public :: type_output_gatherer

   type, public :: type_output_gatherer
      integer :: n_global = 0
      integer :: n_local  = 0
      integer :: io_rank  = 0
      ! MPI_Gatherv params — allocated on ALL ranks; non-root values ignored by MPI
      integer, allocatable :: recv_counts(:)
      integer, allocatable :: recv_displs(:)
   contains
      procedure :: init_points  => gatherer_init_points
      procedure :: gather_vals  => gatherer_gather_vals
      procedure :: finalize     => gatherer_finalize
   end type type_output_gatherer

contains

   ! Set up Gatherv parameters for n_local points on this rank.
   ! Call after interp%init so n_local is known.
   subroutine gatherer_init_points(this, n_global, n_local, comm)
      class(type_output_gatherer), intent(inout) :: this
      integer,         intent(in)    :: n_global, n_local
      type(type_comm), intent(inout) :: comm

      integer :: k, ierr
      integer, allocatable :: all_counts(:)

      this%n_global = n_global
      this%n_local  = n_local
      this%io_rank  = comm%get_io_rank()

      ! Exchange n_local from every rank so IO rank knows recv layout.
      ! All ranks allocate all_counts; non-IO values are unused.
      allocate(all_counts(comm%size), source=0)
      call MPI_Gather(n_local, 1, MPI_INTEGER, &
                      all_counts, 1, MPI_INTEGER, &
                      this%io_rank, comm%id, ierr)

      ! Build recv_counts and recv_displs on ALL ranks (MPI_Gatherv
      ! ignores them on non-root, but they must be allocated to be safe).
      allocate(this%recv_counts(comm%size), source=0)
      allocate(this%recv_displs(comm%size), source=0)
      if (comm%is_io_node()) then
         this%recv_counts = all_counts
         this%recv_displs(1) = 0
         do k = 2, comm%size
            this%recv_displs(k) = this%recv_displs(k-1) + this%recv_counts(k-1)
         end do
      end if

      deallocate(all_counts)
   end subroutine gatherer_init_points

   ! Gather local_vals (size n_local) from all ranks into global_out
   ! (size n_global) on IO rank. Non-IO ranks may pass a size-1 dummy global_out.
   ! Point ordering in global_out follows rank order, not point_id order;
   ! re-sorting by interp%point_id is the caller's responsibility.
   subroutine gatherer_gather_vals(this, local_vals, global_out, comm)
      class(type_output_gatherer), intent(in)    :: this
      real(SP),        intent(in)    :: local_vals(:)
      real(SP),        intent(inout) :: global_out(:)
      type(type_comm), intent(inout) :: comm

      integer :: ierr

      call MPI_Gatherv(local_vals,        this%n_local,    MPI_SP, &
                       global_out,         this%recv_counts, this%recv_displs, MPI_SP, &
                       this%io_rank, comm%id, ierr)
   end subroutine gatherer_gather_vals

   subroutine gatherer_finalize(this)
      class(type_output_gatherer), intent(inout) :: this
      if (allocated(this%recv_counts)) deallocate(this%recv_counts)
      if (allocated(this%recv_displs)) deallocate(this%recv_displs)
      this%n_global = 0
      this%n_local  = 0
   end subroutine gatherer_finalize

end module core_output_gatherer_mod
