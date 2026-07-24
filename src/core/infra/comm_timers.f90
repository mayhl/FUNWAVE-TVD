! allow(E001)
module core_comm_timers_mod
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use mpi_f08, only: MPI_Comm, MPI_Reduce, MPI_DOUBLE_PRECISION, &
                      MPI_SUM, MPI_MAX, MPI_Comm_rank, MPI_Comm_size
   implicit none
   private

   ! Comm-phase wall accumulators (overlap-rung sizing).  Always
   ! accumulated — an MPI_Wtime pair per phase call is beneath
   ! measurement — and reported at shutdown only when
   ! FUNWAVE_COMM_TIMERS is set in the environment.  The halo entries
   ! time the message span only (post to wait); the trid entries time
   ! the WHOLE solve, pipeline wait plus elimination compute.
   integer, parameter, public :: CT_HALO_X = 1, CT_HALO_Y = 2, &
                                 CT_HALO_ONE = 3, CT_TRID_X = 4, &
                                 CT_TRID_Y = 5, CT_DT_REDUCE = 6
   integer, parameter :: NCT = 6
   character(len=9), parameter :: ct_name(NCT) = &
                                  [character(len=9) :: "halo_x", "halo_y", "halo_one", "trid_x", &
                                                        "trid_y", "dt_reduce"]

   real(real64), public :: comm_t(NCT) = 0.0_real64
   integer(int64), public :: comm_n(NCT) = 0_int64

   public :: comm_timers_report

contains

   ! ----------------------------------------------------------------
   ! comm_timers_report — avg/max across ranks, printed by rank 0.
   ! Call once, before MPI_Finalize.
   ! ----------------------------------------------------------------
   subroutine comm_timers_report(comm)
      type(MPI_Comm), intent(in) :: comm
      character(len=8) :: env
      real(real64) :: tsum(NCT), tmax(NCT)
      integer :: rank, nranks, i, stat, ierr

      call get_environment_variable("FUNWAVE_COMM_TIMERS", env, status=stat)
      if (stat /= 0) return
      call MPI_Comm_rank(comm, rank, ierr)
      call MPI_Comm_size(comm, nranks, ierr)
      call MPI_Reduce(comm_t, tsum, NCT, MPI_DOUBLE_PRECISION, MPI_SUM, &
                      0, comm, ierr)
      call MPI_Reduce(comm_t, tmax, NCT, MPI_DOUBLE_PRECISION, MPI_MAX, &
                      0, comm, ierr)
      if (rank /= 0) return
      do i = 1, NCT
         if (comm_n(i) > 0_int64 .or. tmax(i) > 0.0_real64) &
            write (*, '(a,a9,a,i0,a,f10.3,a,f10.3)') &
            "COMMTIMER phase=", ct_name(i), " calls=", comm_n(i), &
            " avg_s=", tsum(i)/nranks, " max_s=", tmax(i)
      end do
   end subroutine comm_timers_report

end module core_comm_timers_mod
