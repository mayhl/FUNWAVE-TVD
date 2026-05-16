!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D boundary condition parameters YAML reader
!
!  YAML block: boundary_conditions:
!    bc_x0: <int>              default 0
!    bc_xn: <int>              default 0
!    bc_y0: <int>              default 0
!    bc_yn: <int>              default 0
!    bc_z0: <int>              default 0
!    bc_zn: <int>              default 0
!    boundary_type: <string>   BOUNDARY, default 'NONE'
!    boundary_file: <path>     BoundaryFile (required for tidal types)
!
!  When boundary_type starts with 'TID_FLX_LR' or 'TID_ELE_LR' the
!  boundary file is opened and read inside read_input; Z_pct_West/East
!  size is derived from geometry/grid_size(3) read directly from the YAML.
!  Allocatable arrays are moved to MODULE GLOBAL via move_alloc in READ_INPUT.
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_bc_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_bc

   type, extends(type_model_base) :: type_model_3d_bc

      integer  :: bc_x0 = 0
      integer  :: bc_xn = 0
      integer  :: bc_y0 = 0
      integer  :: bc_yn = 0
      integer  :: bc_z0 = 0
      integer  :: bc_zn = 0
      character(:), allocatable :: boundary_type
      character(:), allocatable :: boundary_file

      integer  :: num_time_data = 0
      real(SP), allocatable :: time_data(:)
      real(SP), allocatable :: data_u_l(:)
      real(SP), allocatable :: data_u_r(:)
      real(SP), allocatable :: data_eta_l(:)
      real(SP), allocatable :: data_eta_r(:)
      real(SP), allocatable :: data_sal_l(:)
      real(SP), allocatable :: data_sal_r(:)
      real(SP), allocatable :: data_tem_l(:)
      real(SP), allocatable :: data_tem_r(:)
      real(SP), allocatable :: z_pct_west(:)
      real(SP), allocatable :: z_pct_east(:)

   contains
      procedure :: read_input => bc_3d_read_input
   end type type_model_3d_bc

contains

   subroutine bc_3d_read_input(this, env)
      class(type_model_3d_bc), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env, geom_env
      type(type_yaml_reader) :: geom_yaml
      logical :: no_key, geom_empty
      character(80) :: what
      integer :: kglob_local, i
      integer, allocatable :: grid_size(:)

      sub_env = get_sub_env(env, 'boundary_conditions')
      this%is_activated = .true.

      call sub_env%yaml%read('bc_x0', silent=no_key, val=this%bc_x0, default='0')
      call sub_env%yaml%read('bc_xn', silent=no_key, val=this%bc_xn, default='0')
      call sub_env%yaml%read('bc_y0', silent=no_key, val=this%bc_y0, default='0')
      call sub_env%yaml%read('bc_yn', silent=no_key, val=this%bc_yn, default='0')
      call sub_env%yaml%read('bc_z0', silent=no_key, val=this%bc_z0, default='0')
      call sub_env%yaml%read('bc_zn', silent=no_key, val=this%bc_zn, default='0')
      call sub_env%yaml%read('boundary_type', val=this%boundary_type, default='NONE')

      if (index(this%boundary_type, 'TID_FLX_LR') == 1) then
         this%bc_x0 = 3
         this%bc_xn = 3
      else if (index(this%boundary_type, 'TID_ELE_LR') == 1) then
         this%bc_x0 = 1
         this%bc_xn = 1
      end if

      if (index(this%boundary_type, 'TID_FLX_LR') == 1 .or. &
          index(this%boundary_type, 'TID_ELE_LR') == 1) then
         call sub_env%yaml%read('boundary_file', val=this%boundary_file)

         ! Read nz (Kglob) directly from the geometry block of the same YAML
         geom_env = get_sub_env(env, 'geometry', geom_empty)
         if (.not. geom_empty) then
            call geom_env%yaml%read('grid_size', val=grid_size)
            kglob_local = grid_size(3)
         else
            kglob_local = 0
         end if

         open(2, file=trim(this%boundary_file))
            read(2, *) what
            read(2, *) this%num_time_data
            allocate(this%time_data(this%num_time_data))
            allocate(this%data_u_l(this%num_time_data))
            allocate(this%data_u_r(this%num_time_data))
            allocate(this%data_eta_l(this%num_time_data))
            allocate(this%data_eta_r(this%num_time_data))
            allocate(this%data_sal_l(this%num_time_data))
            allocate(this%data_sal_r(this%num_time_data))
            allocate(this%data_tem_l(this%num_time_data))
            allocate(this%data_tem_r(this%num_time_data))
            if (kglob_local > 0) then
               allocate(this%z_pct_west(kglob_local))
               allocate(this%z_pct_east(kglob_local))
               read(2, *) what
               read(2, *) what
               read(2, *) (this%z_pct_west(i), i = 1, kglob_local)
               read(2, *) what
               read(2, *) (this%z_pct_east(i), i = 1, kglob_local)
            end if
            read(2, *) what
            do i = 1, this%num_time_data
               read(2, *, end=111) this%time_data(i)
               read(2, *) what
               read(2, *, end=111) this%data_eta_l(i), this%data_u_l(i), &
                                   this%data_sal_l(i), this%data_tem_l(i)
               read(2, *) what
               read(2, *, end=111) this%data_eta_r(i), this%data_u_r(i), &
                                   this%data_sal_r(i), this%data_tem_r(i)
            end do
111         continue
         close(2)
      end if

   end subroutine bc_3d_read_input

end module model_3d_bc_mod
