!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Coupling configuration YAML reader
!
!  Temporary stub — will be consolidated with tidal forcing into
!  model_bc_mod once tidal boundary conditions are implemented.
!  Sponge layers are handled separately in model_sponge_mod.
!
!  YAML block: coupling:          (# if defined COUPLING only)
!    coupling_file: <path>        required when COUPLING is defined
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_coupling_mod
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   implicit none(external)

   private
   public :: type_model_coupling

   type, extends(type_model_base) :: type_model_coupling
      type(type_path) :: coupling_file
   contains
      procedure :: read_input => coupling_read_input
   end type type_model_coupling

contains

   subroutine coupling_read_input(this, env)
      class(type_model_coupling), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key

      sub_env = get_sub_env(env, 'coupling', is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      call sub_env%yaml%read_input_path('coupling_file', silent=no_key, val=this%coupling_file)

   end subroutine coupling_read_input

end module model_coupling_mod
