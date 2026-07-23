!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Dummy `pnetcdf` module, compiled ONLY when PnetCDF is absent (or
!  USE_PNETCDF=OFF): just enough of the nf90mpi surface for
!  output_channel to compile unchanged.  Every call returns a non-zero
!  status, so pnc_check reports "built without PnetCDF" and stops at
!  the first parallel-write request — builds without the library stay
!  runnable, only field_io_type: PNETCDF dies.
!
!  HISTORY :
!    07/22/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module pnetcdf
   use core_constants_mod, only: SP
   use mpi_f08, only: MPI_OFFSET_KIND
   implicit none

   private
   public :: NF90_64BIT_DATA
   public :: nf90mpi_create, nf90mpi_def_dim, nf90mpi_def_var
   public :: nf90mpi_put_att, nf90mpi_enddef, nf90mpi_put_var_all
   public :: nf90mpi_close, nf90mpi_strerror

   integer, parameter :: NF90_64BIT_DATA = 32

   integer, parameter :: STUB_ERR = -1

   interface nf90mpi_put_var_all
      module procedure put_var_all_1d, put_var_all_2d
   end interface nf90mpi_put_var_all

contains

   integer function nf90mpi_create(comm, path, cmode, info, ncid) result(status)
      integer, intent(in) :: comm, cmode, info
      character(*), intent(in) :: path
      integer, intent(out) :: ncid
      associate (c => comm, p => path, m => cmode, i => info)
      end associate
      ncid = -1
      status = STUB_ERR
   end function nf90mpi_create

   integer function nf90mpi_def_dim(ncid, name, len, dimid) result(status)
      integer, intent(in) :: ncid
      character(*), intent(in) :: name
      integer(kind=MPI_OFFSET_KIND), intent(in) :: len
      integer, intent(out) :: dimid
      associate (i => ncid, n => name, l => len)
      end associate
      dimid = -1
      status = STUB_ERR
   end function nf90mpi_def_dim

   integer function nf90mpi_def_var(ncid, name, xtype, dimids, varid) &
      result(status)
      integer, intent(in) :: ncid, xtype, dimids(:)
      character(*), intent(in) :: name
      integer, intent(out) :: varid
      associate (i => ncid, n => name, x => xtype, d => dimids)
      end associate
      varid = -1
      status = STUB_ERR
   end function nf90mpi_def_var

   integer function nf90mpi_put_att(ncid, varid, name, values) result(status)
      integer, intent(in) :: ncid, varid
      character(*), intent(in) :: name, values
      associate (i => ncid, v => varid, n => name, s => values)
      end associate
      status = STUB_ERR
   end function nf90mpi_put_att

   integer function nf90mpi_enddef(ncid) result(status)
      integer, intent(in) :: ncid
      associate (i => ncid)
      end associate
      status = STUB_ERR
   end function nf90mpi_enddef

   integer function put_var_all_1d(ncid, varid, values, start, count) &
      result(status)
      integer, intent(in) :: ncid, varid
      real(SP), intent(in) :: values(:)
      integer(kind=MPI_OFFSET_KIND), intent(in) :: start(:), count(:)
      associate (i => ncid, v => varid, x => values, s => start, c => count)
      end associate
      status = STUB_ERR
   end function put_var_all_1d

   integer function put_var_all_2d(ncid, varid, values, start, count) &
      result(status)
      integer, intent(in) :: ncid, varid
      real(SP), intent(in) :: values(:, :)
      integer(kind=MPI_OFFSET_KIND), intent(in) :: start(:), count(:)
      associate (i => ncid, v => varid, x => values, s => start, c => count)
      end associate
      status = STUB_ERR
   end function put_var_all_2d

   integer function nf90mpi_close(ncid) result(status)
      integer, intent(in) :: ncid
      associate (i => ncid)
      end associate
      status = STUB_ERR
   end function nf90mpi_close

   function nf90mpi_strerror(status) result(msg)
      integer, intent(in) :: status
      character(:), allocatable :: msg
      associate (s => status)
      end associate
      msg = 'built without PnetCDF (reconfigure with the library)'
   end function nf90mpi_strerror

end module pnetcdf
