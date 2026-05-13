!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output channel configuration YAML reader
!
!  YAML block: output: (list of channel dicts)
!    - id: <string>
!      geometry: field | station | transect
!      variables: [eta, u, v, ...]
!      snapshot: <bool>             optional, default true
!      statistics: [min, max, mean, rms]  optional
!      t_start: <time>              optional, default 0
!      interval: <time>             required
!      format: ascii                optional, default ascii
!      coords_file: <path>          station and transect only
!      buffer_size: <int>           station and transect only
!      start_coord: [x, y]          transect only
!      end_coord: [x, y]            transect only
!      n_points: <int>              transect only
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_output_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none(external)

   private
   public :: type_channel_config, type_model_output

   character(len=10), parameter :: GEOM_TYPES(3) = &
      [character(len=10) :: 'field', 'station', 'transect']
   character(len=8), parameter :: STAT_TYPES(4) = &
      [character(len=8) :: 'min', 'max', 'mean', 'rms']
   character(len=8), parameter :: FORMAT_TYPES(1) = &
      [character(len=8) :: 'ascii']

   type :: type_channel_config
      character(:), allocatable :: id
      character(:), allocatable :: geom_type
      character(:), allocatable :: format
      character(:), allocatable :: variables(:)
      character(:), allocatable :: statistics(:)
      logical :: snapshot = .true.
      real(SP) :: t_start = 0.0_SP
      real(SP) :: interval = 0.0_SP
      integer :: buffer_size = 1000
      character(:), allocatable :: coords_file
      ! Transect geometry
      real(SP) :: start_coord(2) = 0.0_SP
      real(SP) :: end_coord(2) = 0.0_SP
      integer :: n_points = 0
   end type type_channel_config

   type, extends(type_model_base) :: type_model_output
      type(type_channel_config), allocatable :: channels(:)
      integer :: n_channels = 0
   contains
      procedure :: read_input => output_read_input
   end type type_model_output

contains

   subroutine output_read_input(this, env)
      ! Stub — full implementation after src/core/output/ skeleton is complete
      class(type_model_output), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_yaml_reader) :: dummy
      logical :: is_empty

      dummy = env%yaml%cast_dictionary('output', is_empty)
      this%is_activated = .not. is_empty
      this%n_channels = 0
      if (allocated(this%channels)) deallocate(this%channels)
   end subroutine output_read_input

end module model_output_mod
