!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Environment Context
!
!  PURPOSE:
!   - wrap infrastructure components (Comm, Log, YAML)
!   - manage lifecycle of the simulation environment
!
!  HISTORY:
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!--------------------------------------------------

module core_env_mod

   use core_comm_mod, only: type_comm, new_comm
   use core_log_io_mod, only: type_log_writer, new_log_writer
   use core_yaml_file_mod, only: type_yaml_reader

   implicit none(external)

   private
   public :: type_env, new_env

   type type_env
      type(type_comm)        :: comm
      type(type_log_writer)  :: log
      type(type_yaml_reader) :: yaml
   contains
      procedure :: finalize => env_finalize
   end type type_env

   interface new_env
      module procedure env_initialize
   end interface new_env

contains

   function env_initialize(label, yaml_path, log_path) result(this)
      character(*), intent(in) :: label
      character(*), intent(in) :: yaml_path
      character(*), intent(in), optional :: log_path
      type(type_env) :: this

      character(:), allocatable :: log_fpath, yaml_fpath

      ! 1. Initialize Communicator
      this%comm = new_comm(io_rank_id=0)

      ! 2. Initialize Logger
      if (present(log_path)) then
         log_fpath = log_path
         this%log = new_log_writer(label, this%comm%is_io_node(), path=log_fpath, &
                                   std_err_threshold=0, std_out_threshold=0, logfile_threshold=100)
      else
         this%log = new_log_writer(label, this%comm%is_io_node())
      end if

      ! 3. Initialize YAML Reader
      yaml_fpath = yaml_path
      call this%yaml%init(yaml_fpath, this%comm)

   end function env_initialize

   subroutine env_finalize(this)
      class(type_env), intent(inout) :: this

      ! Finalize infrastructure
      call this%log%finalize()
      call this%comm%finalize()

   end subroutine env_finalize

end module core_env_mod
