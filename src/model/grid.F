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

   use Constants, only: sp
   use comm_mod, only: type_comm
   use yaml_file_mod, only: type_yaml_reader
   use log_io_mod, only: type_log_writer
   use model_interface_mod, only: type_model_interface

   character(len=5), dimension(3) ::  gtypes
   character(len=5), dimension(1) ::  ftypes
   data gtypes/'file', 'flat', 'slope'/
   data ftypes/'ascii'/

   type, extends(type_model_interface) :: type_model_grid

      integer :: nx, ny
      integer :: nx_proc, ny_proc
      character(:), allocatable :: gtype, fpath, ftype
      real(sp) :: depth_flat, slope_x0, slope_m
      real(sp) :: x0, y0, dx, dy

   contains
      procedure :: validate_yaml => model_validate_yaml
   end type type_model_grid

contains

   subroutine finalize(this)

      class(type_model_grid), intent(inout) :: this

      if (allocated(this%gtype)) deallocate (this%gtype)
      if (allocated(this%fpath)) deallocate (this%fpath)
      if (allocated(this%ftype)) deallocate (this%ftype)

   end subroutine finalize

   subroutine model_validate_yaml(this, comm, yaml, log)

      class(type_model_grid), intent(inout) :: this
      class(type_comm), intent(inout) :: comm
      type(type_yaml_reader), intent(inout), target :: yaml
      type(type_log_writer), intent(inout), target :: log

      logical :: is_px_empty, is_py_empty

      character(:), allocatable :: msg, submsg

      call yaml%cast_dictionary('grid', this%yaml)
      this%log => log

      call this%yaml%read_enum('type', gtypes, val=this%gtype, default_val='file')

      select case (this%gtype)

      case ('file')
         call this%yaml%read_enum('file type', ftypes, val=this%fpath, default_val='ascii')
         call this%yaml%read_input_path('file path', val=this%fpath)

      case ('flat')
         call this%yaml%read_positive_real('flat depth', val=this%depth_flat)

      case ('slope')
         call this%yaml%read_positive_real('flat depth', val=this%depth_flat)
         call this%yaml%read_real('slope x0', val=this%slope_x0)
         call this%yaml%read_real('slope m', val=this%slope_m)

      end select

      ! TODO: Add nx, ny detection
      call this%yaml%read_positive_integer('nx', val=this%nx)
      call this%yaml%read_positive_integer('ny', val=this%ny)

      call this%yaml%read_positive_integer('nx_proc', val=this%nx_proc, is_empty=is_px_empty)
      call this%yaml%read_positive_integer('ny_proc', val=this%ny_proc, is_empty=is_py_empty)

      if (is_px_empty .neqv. is_py_empty) then
         if (is_px_empty) then
            submsg = "grid/ny_proc"
         else
            submsg = "grid/nx_proc"
         end if
         msg = "both grid/nx_proc and grid/ny_proc required, only "//submsg//" found."
         call log%exit_on_error(msg)
      end if

      call comm%barrier()
      call comm%create_2d(this%nx_proc, this%ny_proc, this%nx, this%ny, is_px_empty)

      ! TODO: Add spherical compiler flags => dx float vs array + Coriolis?
      !       Generalization to curve-linear coordinates via array dx, dy?
      !       File only for spherical/curve-linear mode?
      !
      call this%yaml%read_real('dx', val=this%dx)
      call this%yaml%read_real('dy', val=this%dy)
      call this%yaml%read_real('x0', val=this%x0, default_val='0.0')
      call this%yaml%read_real('y0', val=this%y0, default_val='0.0')

   end subroutine model_validate_yaml

end module model_grid_mod

