!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  NOTE: Superseded by geometry.f90 (type_model_geometry).
!  Retained for legacy io.F compatibility during transition.
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Fixed env storage UB; updated to use model_base_mod
!
!-------------------------------------------------

module model_grid_mod
   use mpi_f08
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   implicit none(external)

   private

   public type_model_grid
   character(len=5), dimension(3) ::  gtypes
   character(len=5), dimension(1) ::  ftypes
   data gtypes/'file', 'flat', 'slope'/
   data ftypes/'ascii'/

   type, extends(type_model_base) :: type_model_grid

      integer :: nx, ny
      integer :: nx_proc, ny_proc
      character(:), allocatable :: gtype, ftype
      type(type_path) :: fpath
      real(SP) :: depth_flat, slope_x0, slope_m
      real(SP) :: x0, y0, dx, dy
      logical :: apply_correction

   contains
      procedure :: read_input => grid_read_input
   end type type_model_grid

contains

   subroutine grid_read_input(this, env)
      class(type_model_grid), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_px_empty, is_py_empty, is_p_empty
      character(:), allocatable :: msg, submsg

      sub_env = get_sub_env(env, 'grid')
      this%is_activated = .true.

      call sub_env%comm%barrier()
      call sub_env%yaml%read_enum('type', gtypes, val=this%gtype, default='file')
      call sub_env%comm%barrier()

      select case (this%gtype)
      case ('file')
         call sub_env%yaml%read_enum('file type', ftypes, val=this%ftype, default='ascii')
         call sub_env%yaml%read_input_path('file path', val=this%fpath)
         call sub_env%yaml%read('slope correction', val=this%apply_correction, default='NO')
      case ('flat')
         call sub_env%yaml%read_positive('flat depth', val=this%depth_flat)
      case ('slope')
         call sub_env%yaml%read_positive('flat depth', val=this%depth_flat)
         call sub_env%yaml%read('slope x0', val=this%slope_x0)
         call sub_env%yaml%read('slope m', val=this%slope_m)
      end select

      call sub_env%comm%barrier()

      call sub_env%yaml%read_positive('nx', val=this%nx)
      call sub_env%yaml%read_positive('ny', val=this%ny)

      call sub_env%yaml%read_positive('x_proc', val=this%nx_proc, silent=is_px_empty)
      call sub_env%yaml%read_positive('y_proc', val=this%ny_proc, silent=is_py_empty)

      call sub_env%comm%barrier()

      is_p_empty = is_py_empty .and. is_px_empty
      if (is_px_empty .neqv. is_py_empty) then
         if (is_px_empty) then
            submsg = "grid/ny_proc"
         else
            submsg = "grid/nx_proc"
         end if
         msg = "both grid/nx_proc and grid/ny_proc required, only "//submsg//" found."
         call sub_env%log%exit_on_error(msg)
      end if

      call sub_env%yaml%read_real('dx', val=this%dx)
      call sub_env%yaml%read_real('dy', val=this%dy)
      call sub_env%yaml%read_real('x0', val=this%x0, default='0.0')
      call sub_env%yaml%read_real('y0', val=this%y0, default='0.0')

      call sub_env%comm%barrier()
   end subroutine grid_read_input

end module model_grid_mod
