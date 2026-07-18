!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Hot start parameters YAML reader
!
!  YAML block: hot_start:      (top-level; omit to disable)
!    checkpoint: <dir>         restart from a binary checkpoint set (holds
!                              core.bin = eta,p,q,mask,time; later per-module
!                              bins).  When present, the eta/u/v/mask/time keys
!                              below are unused (state + time come from the bin).
!    eta_file: <path>          required (ASCII path; when checkpoint absent)
!    u_file: <path>            optional (zero velocity if absent)
!    v_file: <path>            optional (zero velocity if absent)
!    mask_file: <path>         optional (no mask if absent)
!    bed_deformation: <bool>   optional, default false
!    time: <time>              optional, default 0
!    output_start_number: <int> optional, default 1
!
!  is_activated = .true.  when the hot_start: block is present
!  is_activated = .false. when the block is absent (cold start)
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_hot_start_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_hot_start

   type, extends(type_model_base) :: type_model_hot_start

      character(:), allocatable :: checkpoint   ! restart checkpoint dir ("" if unused)
      logical :: use_checkpoint = .false.
      type(type_path) :: eta_file
      type(type_path) :: u_file
      type(type_path) :: v_file
      type(type_path) :: mask_file
      logical :: no_uv_file = .true.
      logical :: no_mask_file = .true.
      logical :: bed_deformation = .false.
      real(SP) :: time = 0.0_SP
      integer :: output_start_number = 1

   contains
      procedure :: read_input => hot_start_read_input
   end type type_model_hot_start

contains

   subroutine hot_start_read_input(this, env)
      class(type_model_hot_start), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_hs, no_chk, no_u, no_v, no_mask

      sub_env = get_sub_env(env, "hot_start", is_empty=no_hs)
      this%is_activated = .not. no_hs
      if (.not. this%is_activated) return

      ! A binary checkpoint set supersedes the ASCII eta/u/v/mask path: state
      ! and time come from core.bin, so those keys are not read when present.
      call sub_env%yaml%read("checkpoint", silent=no_chk, val=this%checkpoint, default="")
      this%use_checkpoint = .not. no_chk

      if (.not. this%use_checkpoint) then
         call sub_env%yaml%read_input_path("eta_file", val=this%eta_file)
         call sub_env%yaml%read_input_path("u_file", silent=no_u, val=this%u_file)
         call sub_env%yaml%read_input_path("v_file", silent=no_v, val=this%v_file)
         call sub_env%yaml%read_input_path("mask_file", silent=no_mask, val=this%mask_file)
         this%no_uv_file = no_u .or. no_v
         this%no_mask_file = no_mask

         call sub_env%yaml%read("bed_deformation", val=this%bed_deformation, default="NO")
         call sub_env%yaml%read_nonnegative("time", val=this%time, default="0.0")
      end if

      call sub_env%yaml%read_positive("output_start_number", val=this%output_start_number, default="1")

   end subroutine hot_start_read_input

end module model_hot_start_mod
