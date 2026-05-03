module core_config_mod
   use core_yaml_file_mod, only: type_yaml_reader
   use core_constants_mod, only: SP, type_string
   use core_comm_mod, only: type_comm
   use fortran_yaml_c, only: type_list, type_list_item, type_dictionary, type_node
   implicit none

   type, public :: type_simulation_config
      real(SP) :: duration = 0.0_SP
      real(SP) :: start_time = 0.0_SP
      character(:), allocatable :: time_scheme
   contains
      procedure :: init_from_yaml
   end type type_simulation_config

   type, public :: type_output_channel
      character(:), allocatable :: id
      character(:), allocatable :: geometry
      real(SP) :: interval = 1.0_SP
      integer :: buffer_size = 0
      type(type_string), allocatable :: options(:)
   end type type_output_channel

   type, public :: type_config
      type(type_simulation_config) :: simulation
      type(type_output_channel), allocatable :: output_channels(:)
   contains
      procedure :: load_from_yaml
   end type type_config

contains

   subroutine init_from_yaml(this, reader)
      class(type_simulation_config), intent(inout) :: this
      type(type_yaml_reader), intent(inout) :: reader
      type(type_yaml_reader) :: sim_reader

      logical:: test
      !print *, reader%comm%rank_id, reader%comm%is_io_node()

      !sim_reader = reader%cast_dictionary("simulation", test)

      !print *, sim_reader%comm%rank_id
      !if (.not. test) then
      if (reader%is_dictionary("simulation")) then
         sim_reader = reader%cast_dictionary("simulation")
         print *, "init_from_yaml true", sim_reader%comm%rank_id
         call sim_reader%read_real("duration", val=this%duration)
         call sim_reader%read("start_time", val=this%start_time, dim='time')
         call sim_reader%read("time_scheme", val=this%time_scheme)
      end if
   end subroutine init_from_yaml

   subroutine load_from_yaml(this, file_path, comm)
      class(type_config), intent(inout) :: this
      character(len=*), intent(in) :: file_path
      character(:), allocatable :: path_copy
      type(type_comm), target, intent(inout) :: comm
      type(type_yaml_reader) :: reader, chan_reader
      class(type_node), pointer :: node
      class(type_list), pointer :: list_node
      type(type_list_item), pointer :: item
      class(type_dictionary), pointer :: dict
      integer :: i, n_channels

      print *, comm%rank_id, comm%is_io_node()
      print *, "-------------------------"
      path_copy = file_path
      call reader%init(path_copy, comm)
      call this%simulation%init_from_yaml(reader)

      node => reader%root%get("output")
      if (associated(node)) then
         select type (node)
         class is (type_list)
            list_node => node
            n_channels = list_node%size()
            allocate (this%output_channels(n_channels))

            i = 1
            item => list_node%first
            do while (associated(item))
               select type (n => item%node)
               class is (type_dictionary)
                  dict => n
                  chan_reader = reader%clone(dict)

                  call chan_reader%read("id", val=this%output_channels(i)%id)
                  call chan_reader%read("geometry", val=this%output_channels(i)%geometry)
                  call chan_reader%read("interval", val=this%output_channels(i)%interval)
                  call chan_reader%read("buffer_size", default="0", val=this%output_channels(i)%buffer_size)
                  call chan_reader%read_string_array("options", val=this%output_channels(i)%options)
               end select
               i = i + 1
               item => item%next
            end do
         end select
      end if
   end subroutine load_from_yaml

end module core_config_mod
