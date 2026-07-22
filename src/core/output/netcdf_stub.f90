!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Dummy `netcdf` module, compiled ONLY when netcdf-fortran is absent
!  (or USE_NETCDF=OFF): just enough of the nf90 surface for
!  output_channel to compile unchanged.  Every call returns a non-zero
!  status, so the channel's nc_check reports "built without
!  netcdf-fortran" and stops at the first netcdf request — production
!  builds without the library stay buildable, only format: netcdf dies.
!  Unit-test builds require the real library (the round-trip test
!  exercises it), enforced at configure time.
!
!  HISTORY :
!    07/22/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module netcdf
   use core_constants_mod, only: SP
   implicit none

   private
   public :: NF90_NOERR, NF90_CLOBBER, NF90_NETCDF4, NF90_UNLIMITED
   public :: NF90_DOUBLE, NF90_GLOBAL
   public :: nf90_create, nf90_def_dim, nf90_def_var, nf90_put_att
   public :: nf90_enddef, nf90_put_var, nf90_close, nf90_strerror

   integer, parameter :: NF90_NOERR = 0
   integer, parameter :: NF90_CLOBBER = 0
   integer, parameter :: NF90_NETCDF4 = 4096
   integer, parameter :: NF90_UNLIMITED = 0
   integer, parameter :: NF90_DOUBLE = 6
   integer, parameter :: NF90_GLOBAL = 0

   integer, parameter :: STUB_ERR = -1

   interface nf90_put_var
      module procedure put_var_1d, put_var_2d
   end interface nf90_put_var

contains

   integer function nf90_create(path, cmode, ncid) result(status)
      character(*), intent(in) :: path
      integer, intent(in) :: cmode
      integer, intent(out) :: ncid
      associate (p => path, c => cmode)
      end associate
      ncid = -1
      status = STUB_ERR
   end function nf90_create

   integer function nf90_def_dim(ncid, name, len, dimid) result(status)
      integer, intent(in) :: ncid, len
      character(*), intent(in) :: name
      integer, intent(out) :: dimid
      associate (i => ncid, n => name, l => len)
      end associate
      dimid = -1
      status = STUB_ERR
   end function nf90_def_dim

   integer function nf90_def_var(ncid, name, xtype, dimids, varid) &
      result(status)
      integer, intent(in) :: ncid, xtype, dimids(:)
      character(*), intent(in) :: name
      integer, intent(out) :: varid
      associate (i => ncid, n => name, x => xtype, d => dimids)
      end associate
      varid = -1
      status = STUB_ERR
   end function nf90_def_var

   integer function nf90_put_att(ncid, varid, name, values) result(status)
      integer, intent(in) :: ncid, varid
      character(*), intent(in) :: name, values
      associate (i => ncid, v => varid, n => name, s => values)
      end associate
      status = STUB_ERR
   end function nf90_put_att

   integer function nf90_enddef(ncid) result(status)
      integer, intent(in) :: ncid
      associate (i => ncid)
      end associate
      status = STUB_ERR
   end function nf90_enddef

   integer function put_var_1d(ncid, varid, values, start) result(status)
      integer, intent(in) :: ncid, varid
      real(SP), intent(in) :: values(:)
      integer, intent(in), optional :: start(:)
      associate (i => ncid, v => varid, x => values)
      end associate
      if (present(start)) continue
      status = STUB_ERR
   end function put_var_1d

   integer function put_var_2d(ncid, varid, values, start) result(status)
      integer, intent(in) :: ncid, varid
      real(SP), intent(in) :: values(:, :)
      integer, intent(in), optional :: start(:)
      associate (i => ncid, v => varid, x => values)
      end associate
      if (present(start)) continue
      status = STUB_ERR
   end function put_var_2d

   integer function nf90_close(ncid) result(status)
      integer, intent(in) :: ncid
      associate (i => ncid)
      end associate
      status = STUB_ERR
   end function nf90_close

   function nf90_strerror(status) result(msg)
      integer, intent(in) :: status
      character(:), allocatable :: msg
      associate (s => status)
      end associate
      msg = 'built without netcdf-fortran (reconfigure with the library)'
   end function nf90_strerror

end module netcdf
