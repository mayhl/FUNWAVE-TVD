!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Communicator
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module Communicator

   USE MPI

   type TypeNode

      public
      integer :: comm_id, rank_id
      integer :: proc_x_id, proc_y_id
      ! cross-shore (x-)direction.
      ! back => shore <=> -x > + x
      integer :: back_rank_id, shore_rank_id
      ! alongshore (y-)direction.
      ! right => left <=> -y > + y
      integer :: left_rank_id, right_rank_id

      logical :: is_left_boundry
      logical :: is_right_boundry
      logical :: is_shore_boundry
      logical :: is_back_boundry

   end type Comm

contains

   function constructor(node, comm)

      implicit none
      type(TypeNode) :: node
      integer, intent(in) :: comm
      !integer, diension(2) :: coords
      integer :: ier

      node%comm_id = comm
      call MPI_Comm_rank(comm, node%rank_id)

      !call MPI_CART_CREATE(MPI_COMM_WORLD, ndims, dims, &
      !                      periods, reorder, comm2d, ier)

      ! call MPI_CART_COORDS(comm2d, myid, 2, coords, ier)
      !
      ! node%proc_xi = coords(1)
      ! node%proc_yj = coords(2)
      !
      ! call MPI_Cart_shift(comm2d, 0, 1, node%back_rank_id, node%shore_rank, ier)
      ! call MPI_Cart_shift(comm2d, 1, 1, node%right_rank, node%left_rank_id, ier)

   end function constructor

   subroutine setLogWriterByID(this, rank_id)

!implict none
      type(TypeNode), intent(inout) :: this
      integer, intent(in)  :: rank_id

   end subroutine

   function initComm(size) result(ierror)

      use MPI, only: MPI_INIT, MPI_Comm_size
      implicit none
      integer, intent(in) :: size
      integer :: comm, ierror

      CALL MPI_INIT(ierror)
      CALL MPI_Comm_size(comm, size)

   end function initComm

   function creat2DComm(comm, num_proc_x, num_proc_y) result(node)

      integer, intent(IN):: comm, num_proc_x, num_proc_y

      integer :: ierr
      integer, dimension(2):: coords

      type(Node):: node

   end function create2DComm

end module Communicator

