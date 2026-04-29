!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Component
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_grid_mod
   use mpi_f08

   use core_comm_mod, only: type_comm
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env
   use core_log_io_mod, only: type_log_writer
   use core_path_mod, only: type_path
   use core_yaml_file_mod, only: type_yaml_reader
   use model_interface_mod, only: type_model_interface

   implicit none(external)

   private

   public type_model_grid
   character(len=5), dimension(3) ::  gtypes
   character(len=5), dimension(1) ::  ftypes
   data gtypes/'file', 'flat', 'slope'/
   data ftypes/'ascii'/

   type, extends(type_model_interface) :: type_model_grid

      integer :: nx, ny
      integer :: nx_proc, ny_proc
      character(:), allocatable :: gtype, ftype
      type(type_path) :: fpath
      real(SP) :: depth_flat, slope_x0, slope_m
      real(SP) :: x0, y0, dx, dy
      logical :: apply_correction

   contains
      procedure :: read_input => read_input
   end type type_model_grid

contains

   subroutine finalize(this)

      class(type_model_grid), intent(inout) :: this

      if (allocated(this%gtype)) deallocate (this%gtype)
      !if (allocated(this%fpath)) deallocate (this%fpath)
      if (allocated(this%ftype)) deallocate (this%ftype)

   end subroutine finalize

   subroutine read_input(this, env)

      class(type_model_grid), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      integer :: err
      integer(MPI_ADDRESS_KIND) ::address

      logical :: is_px_empty, is_py_empty, is_p_empty

      character(:), allocatable :: msg, submsg

      this%yaml = env%yaml
      this%log => env%log
      this%is_activated = .true.

      call env%yaml%cast_dictionary('grid', this%yaml)
      call env%comm%barrier()
      call this%yaml%read('type', gtypes, val=this%gtype, default='file')
      !this%gtype = 'slope'
      call this%yaml%comm%barrier()
      select case (this%gtype)

      case ('file')
         call this%yaml%read('file type', ftypes, val=this%ftype, default='ascii')
         call this%yaml%read('file path', val=this%fpath)
         call this%yaml%read('slope correction', val=this%apply_correction, default='NO')

      case ('flat')
         call this%yaml%read_positive('flat depth', val=this%depth_flat)

      case ('slope')
         call this%yaml%read_positive('flat depth', val=this%depth_flat)
         call this%yaml%read('slope x0', val=this%slope_x0)
         call this%yaml%read('slope m', val=this%slope_m)

      end select

      call this%yaml%comm%barrier()

      ! TODO:
      ! ! Stretched & spherical grid
      !
      call this%yaml%read_positive('nx', val=this%nx)
      call this%yaml%read_positive('ny', val=this%ny)

      call this%yaml%read_positive('x_proc', val=this%nx_proc, is_empty=is_px_empty)
      call this%yaml%read_positive('y_proc', val=this%ny_proc, is_empty=is_py_empty)

      call this%yaml%comm%barrier()

      is_p_empty = is_py_empty .and. is_px_empty
      if (is_px_empty .neqv. is_py_empty) then
         if (is_px_empty) then
            submsg = "grid/ny_proc"
         else
            submsg = "grid/nx_proc"
         end if
         msg = "both grid/nx_proc and grid/ny_proc required, only "//submsg//" found."
         call env%log%exit_on_error(msg)
      end if

      !call env%comm%barrier()
      !call env%comm%create_2d(this%nx_proc, this%ny_proc, this%nx, this%ny, is_p_empty)

      ! TODO: Add spherical compiler flags => dx float vs array + Coriolis?
      !       Generalization to curve-linear coordinates via array dx, dy?
      !       File only for spherical/curve-linear mode?
      !
      call this%yaml%read_real('dx', val=this%dx)
      call this%yaml%read_real('dy', val=this%dy)
      call this%yaml%read_real('x0', val=this%x0, default='0.0')
      call this%yaml%read_real('y0', val=this%y0, default='0.0')

      call env%comm%barrier()
   end subroutine read_input

end module model_grid_mod

