!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Hot start parameters YAML reader
!
!  YAML block: hot_start:      (top-level; omit for a cold start)
!    checkpoint: <dir>         required: restart from a binary checkpoint
!                              set (core.bin = eta,p,q,mask,time; later
!                              per-module bins).  State and time come
!                              from the bins.
!    output_start_number: <int> optional, default 1
!
!  The legacy ASCII restart (ETA_FILE/U_FILE/V_FILE/MASK_FILE,
!  HotStartTime, BED_DEFORMATION) is retired: initial: fields reads the
!  same fields in every format, with time and bed_deformation; the
!  wet/dry mask is derived from eta and depth.
!
!  is_activated = .true.  when the hot_start: block is present
!  is_activated = .false. when the block is absent (cold start)
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!    09/16/2026  checkpoint only; the ASCII path moved to initial: fields
!
!-------------------------------------------------

module model_hot_start_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_hot_start

   type, extends(type_model_base) :: type_model_hot_start

      character(:), allocatable :: checkpoint   ! restart checkpoint dir
      logical :: use_checkpoint = .false.       ! = is_activated; kept for the call sites
      real(SP) :: time = 0.0_SP                 ! restored from core.bin
      integer :: output_start_number = 1

   contains
      procedure :: read_input => hot_start_read_input
   end type type_model_hot_start

contains

   subroutine hot_start_read_input(this, env)
      class(type_model_hot_start), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_hs, no_chk

      sub_env = get_sub_env(env, "hot_start", is_empty=no_hs)
      this%is_activated = .not. no_hs
      if (.not. this%is_activated) return

      call sub_env%yaml%read("checkpoint", silent=no_chk, val=this%checkpoint, default="")
      if (no_chk) call env%log%exit_on_error( &
         "hot_start: checkpoint is required (a start from field files is initial: fields)")
      this%use_checkpoint = .true.

      call sub_env%yaml%read_positive("output_start_number", val=this%output_start_number, default="1")

   end subroutine hot_start_read_input

end module model_hot_start_mod
