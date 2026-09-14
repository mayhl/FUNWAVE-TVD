!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Checkpoint (hot-start) binary I/O — the `core` module.
!
!  A checkpoint is a DIRECTORY of per-module binary files; this module owns
!  core.bin, the step-boundary state plus the simulation time.  Later modules
!  (wavemaker phases, sediment C/bed, k-eps) contribute their own <module>.bin
!  behind the same per-module dispatch, so a bin present => restore, absent =>
!  cold-init (the mode-3 chaining policy, design-hotstart).
!
!  core.bin layout (stream, unformatted):
!    header : version(int) M(int) N(int) time(real)
!    body   : eta p q u v hu hv mask mask9 pflux qflux
!             (each (M,N) real, global interior)
!
!  We save the CONSERVED dispersive flux (p, q) directly, NOT a reconstructed
!  p = H*u — the old hot-start path dropped the dispersion inversion and so
!  discontinued every dispersive run.  We ALSO save every derived field the
!  next step reads before recomputing it: the momentum fields (u, v, hu, hv,
!  mask9) and the interface flux workspace (pflux, qflux = the last stage's
!  fluxes, which the next step's dispersion reads for eta_t).  All of these are
!  produced by kernels that read the PREVIOUS stage's ghosts (a deliberate
!  legacy-parity quirk), so a fresh restart cannot reproduce them from
!  (eta, p, q) alone — it lands ~1e-7 off, not bitwise.  Storing them makes the
!  core restart bitwise-exact; H is the one field rebuilt on restart (a pure
!  local function of eta).
!
!  Write gathers each interior to the IO rank (one global file); read is serial
!  on every rank (read the global file, slice this subdomain) so a checkpoint
!  restarts under ANY rank count, like read_field_ascii.
!
!  HISTORY :
!    07/18/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_checkpoint_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_env_mod, only: type_env
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_output_gatherer_mod, only: type_output_gatherer
   use model_fields_2d_mod, only: type_fields_2d
   use model_sediment_mod, only: type_model_sediment

   implicit none

   private
   public :: write_checkpoint_core, read_checkpoint_core
   public :: write_checkpoint_sediment, read_checkpoint_sediment

   ! Bump when the core.bin layout changes; read rejects an unknown version.
   integer, parameter :: CORE_VERSION = 3
   ! sediment.bin: depth, the suspended/bed accumulators the morphology
   ! cannot rebuild (zb, dchg, ch all derive from these + depth_ini), and the
   ! Morph_interval running window -- the partial sums, the last means the
   ! morphology and the feedback terms read every step, and the avalanche
   ! accumulator.  Without the window a restart integrates zero pickup until
   ! the first window closes (measured 5e-7 in depth, 1e-6 in eta).
   integer, parameter :: SED_VERSION = 2
   integer, parameter :: SED_NFIELD = 18

contains

   ! Gather the live core state interiors to the IO rank and write <dir>core.bin.
   ! pflux/qflux are the stepper's interface flux workspace (registry p_flux/
   ! q_flux).  dir must already exist and carry a trailing separator.
   subroutine write_checkpoint_core(env, comm, grid, f, pflux, qflux, time, dir)
      type(type_env), intent(inout) :: env
      type(type_comm), intent(inout) :: comm
      type(type_grid_2d), intent(in) :: grid
      type(type_fields_2d), intent(in) :: f
      real(SP), intent(in) :: pflux(:, :), qflux(:, :)
      real(SP), intent(in) :: time
      character(*), intent(in) :: dir

      type(type_output_gatherer) :: g
      real(SP), allocatable :: ge(:, :), gp(:, :), gq(:, :), gu(:, :), gv(:, :)
      real(SP), allocatable :: ghu(:, :), ghv(:, :), gm(:, :), gm9(:, :)
      real(SP), allocatable :: gpf(:, :), gqf(:, :)
      integer :: unit

      call g%init_field(grid, comm)
      call alloc_global(comm, grid, ge)
      call alloc_global(comm, grid, gp)
      call alloc_global(comm, grid, gq)
      call alloc_global(comm, grid, gu)
      call alloc_global(comm, grid, gv)
      call alloc_global(comm, grid, ghu)
      call alloc_global(comm, grid, ghv)
      call alloc_global(comm, grid, gm)
      call alloc_global(comm, grid, gm9)
      call alloc_global(comm, grid, gpf)
      call alloc_global(comm, grid, gqf)

      call gather_interior(g, comm, grid, f%eta, ge)
      call gather_interior(g, comm, grid, f%p, gp)
      call gather_interior(g, comm, grid, f%q, gq)
      call gather_interior(g, comm, grid, f%u, gu)
      call gather_interior(g, comm, grid, f%v, gv)
      call gather_interior(g, comm, grid, f%hu, ghu)
      call gather_interior(g, comm, grid, f%hv, ghv)
      call gather_interior(g, comm, grid, real(f%mask, SP), gm)
      call gather_interior(g, comm, grid, real(f%mask9, SP), gm9)
      call gather_interior(g, comm, grid, pflux, gpf)
      call gather_interior(g, comm, grid, qflux, gqf)

      if (comm%is_io_node()) then
         open (newunit=unit, file=trim(dir)//"core.bin", access="stream", &
               form="unformatted", status="replace", action="write")
         write (unit) CORE_VERSION, grid%M, grid%N, time
         write (unit) ge, gp, gq, gu, gv, ghu, ghv, gm, gm9, gpf, gqf
         close (unit)
         call env%log%info("checkpoint: wrote "//trim(dir)//"core.bin")
      end if

      call g%finalize()
   end subroutine write_checkpoint_core

   ! Gather the sediment state interiors to the IO rank and write
   ! <dir>sediment.bin: the evolved depth, the two load accumulators, the
   ! suspended mass, and the Morph_interval window state.
   subroutine write_checkpoint_sediment(env, comm, grid, depth, sed, depth_fx, depth_fy, dir)
      type(type_env), intent(inout) :: env
      type(type_comm), intent(inout) :: comm
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: depth(:, :)
      type(type_model_sediment), intent(in) :: sed
      ! the kernels' face depths: legacy never refreshes them after a bed
      ! change, so they are state the restored bed cannot rebuild
      real(SP), intent(in) :: depth_fx(:, :), depth_fy(:, :)
      character(*), intent(in) :: dir

      type(type_output_gatherer) :: g
      real(SP), allocatable :: gf(:, :, :)
      integer :: unit, k, mloc, nloc

      call g%init_field(grid, comm)
      call alloc_global_set(comm, grid, gf)
      call gather_interior(g, comm, grid, depth, gf(:, :, 1))
      call gather_interior(g, comm, grid, sed%chh, gf(:, :, 2))
      call gather_interior(g, comm, grid, sed%susp_load, gf(:, :, 3))
      call gather_interior(g, comm, grid, sed%bed_load, gf(:, :, 4))
      call gather_interior(g, comm, grid, sed%c_sum, gf(:, :, 5))
      call gather_interior(g, comm, grid, sed%p_sum, gf(:, :, 6))
      call gather_interior(g, comm, grid, sed%d_sum, gf(:, :, 7))
      call gather_interior(g, comm, grid, sed%c_ave, gf(:, :, 8))
      call gather_interior(g, comm, grid, sed%p_ave, gf(:, :, 9))
      call gather_interior(g, comm, grid, sed%d_ave, gf(:, :, 10))
      call gather_interior(g, comm, grid, sed%aval_accum, gf(:, :, 11))
      call gather_interior(g, comm, grid, sed%ch, gf(:, :, 12))
      ! the stage-lagged source rates: the next stage's solve reads the
      ! previous stage's pickup and deposition
      call gather_interior(g, comm, grid, sed%pickup, gf(:, :, 17))
      call gather_interior(g, comm, grid, sed%depo, gf(:, :, 18))
      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      call gather_interior(g, comm, grid, depth_fx(1:mloc, :), gf(:, :, 13))
      call gather_interior(g, comm, grid, depth_fx(2:mloc + 1, :), gf(:, :, 14))
      call gather_interior(g, comm, grid, depth_fy(:, 1:nloc), gf(:, :, 15))
      call gather_interior(g, comm, grid, depth_fy(:, 2:nloc + 1), gf(:, :, 16))

      if (comm%is_io_node()) then
         open (newunit=unit, file=trim(dir)//"sediment.bin", access="stream", &
               form="unformatted", status="replace", action="write")
         write (unit) SED_VERSION, grid%M, grid%N, sed%t_sum
         do k = 1, SED_NFIELD
            write (unit) gf(:, :, k)
         end do
         close (unit)
         call env%log%info("checkpoint: wrote "//trim(dir)//"sediment.bin")
      end if
      call g%finalize()
   end subroutine write_checkpoint_sediment

   ! Read <dir>sediment.bin and slice this subdomain's interior; absent bin
   ! => found = .false. and the module cold-inits (the mode-3 chaining
   ! policy).  Ghosts are the caller's job.
   subroutine read_checkpoint_sediment(env, grid, depth, sed, dir, found)
      type(type_env), intent(inout) :: env
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: depth(:, :)
      type(type_model_sediment), intent(inout) :: sed
      character(*), intent(in) :: dir
      logical, intent(out) :: found

      real(SP), allocatable :: gf(:, :, :)
      character(:), allocatable :: fname
      integer :: unit, ver, mm, nn, k

      fname = trim(dir)//"sediment.bin"
      inquire (file=fname, exist=found)
      if (.not. found) return

      allocate (gf(grid%M, grid%N, SED_NFIELD))
      open (newunit=unit, file=fname, access="stream", &
            form="unformatted", status="old", action="read")
      read (unit) ver, mm, nn
      if (ver /= SED_VERSION) &
         call env%log%exit_on_error("read_checkpoint_sediment: unknown version")
      if (mm /= grid%M .or. nn /= grid%N) &
         call env%log%exit_on_error("read_checkpoint_sediment: grid size mismatch")
      read (unit) sed%t_sum
      do k = 1, SED_NFIELD
         read (unit) gf(:, :, k)
      end do
      close (unit)

      associate (lp => grid%lp, ib => grid%ibegin, ie => grid%istop, &
                 jb => grid%jbegin, je => grid%jstop)
         depth(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 1)
         sed%chh(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 2)
         sed%susp_load(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 3)
         sed%bed_load(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 4)
         sed%c_sum(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 5)
         sed%p_sum(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 6)
         sed%d_sum(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 7)
         sed%c_ave(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 8)
         sed%p_ave(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 9)
         sed%d_ave(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 10)
         sed%aval_accum(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 11)
         sed%ch(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 12)
         sed%pickup(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 17)
         sed%depo(lp%ib:lp%ie, lp%jb:lp%je) = gf(ib:ie, jb:je, 18)
         if (allocated(sed%chk_face)) deallocate (sed%chk_face)
         allocate (sed%chk_face(lp%mloc, lp%nloc, 4), source=0.0_SP)
         do k = 1, 4
            sed%chk_face(lp%ib:lp%ie, lp%jb:lp%je, k) = gf(ib:ie, jb:je, 12 + k)
         end do
      end associate
   end subroutine read_checkpoint_sediment

   ! Read <dir>core.bin on every rank and slice this subdomain's interior into
   ! the live core fields (plus the interface flux workspace pflux/qflux);
   ! return the saved time.  Ghost cells are the caller's job (restart_sync
   ! exchanges them, as read_field_ascii's caller ghost-fills).
   subroutine read_checkpoint_core(env, grid, f, pflux, qflux, time, dir)
      type(type_env), intent(inout) :: env
      type(type_grid_2d), intent(in) :: grid
      type(type_fields_2d), intent(inout) :: f
      real(SP), intent(inout) :: pflux(:, :), qflux(:, :)
      real(SP), intent(out) :: time
      character(*), intent(in) :: dir

      real(SP), allocatable :: ge(:, :), gp(:, :), gq(:, :), gu(:, :), gv(:, :)
      real(SP), allocatable :: ghu(:, :), ghv(:, :), gm(:, :), gm9(:, :)
      real(SP), allocatable :: gpf(:, :), gqf(:, :)
      character(:), allocatable :: fname
      logical :: exists
      integer :: unit, ver, mm, nn

      fname = trim(dir)//"core.bin"
      inquire (file=fname, exist=exists)
      if (.not. exists) call env%log%exit_on_error("read_checkpoint_core: cannot find "//fname)

      open (newunit=unit, file=fname, access="stream", form="unformatted", &
            status="old", action="read")
      read (unit) ver, mm, nn, time
      if (ver /= CORE_VERSION) &
         call env%log%exit_on_error("read_checkpoint_core: unsupported core.bin version")
      if (mm /= grid%M .or. nn /= grid%N) &
         call env%log%exit_on_error("read_checkpoint_core: grid size mismatch vs core.bin")

      allocate (ge(mm, nn), gp(mm, nn), gq(mm, nn), gu(mm, nn), gv(mm, nn))
      allocate (ghu(mm, nn), ghv(mm, nn), gm(mm, nn), gm9(mm, nn))
      allocate (gpf(mm, nn), gqf(mm, nn))
      read (unit) ge, gp, gq, gu, gv, ghu, ghv, gm, gm9, gpf, gqf
      close (unit)

      associate (lp => grid%lp, ib => grid%ibegin, ie => grid%istop, &
                 jb => grid%jbegin, je => grid%jstop)
         f%eta(lp%ib:lp%ie, lp%jb:lp%je) = ge(ib:ie, jb:je)
         f%p(lp%ib:lp%ie, lp%jb:lp%je) = gp(ib:ie, jb:je)
         f%q(lp%ib:lp%ie, lp%jb:lp%je) = gq(ib:ie, jb:je)
         f%u(lp%ib:lp%ie, lp%jb:lp%je) = gu(ib:ie, jb:je)
         f%v(lp%ib:lp%ie, lp%jb:lp%je) = gv(ib:ie, jb:je)
         f%hu(lp%ib:lp%ie, lp%jb:lp%je) = ghu(ib:ie, jb:je)
         f%hv(lp%ib:lp%ie, lp%jb:lp%je) = ghv(ib:ie, jb:je)
         f%mask(lp%ib:lp%ie, lp%jb:lp%je) = nint(gm(ib:ie, jb:je))
         f%mask9(lp%ib:lp%ie, lp%jb:lp%je) = nint(gm9(ib:ie, jb:je))
         pflux(lp%ib:lp%ie, lp%jb:lp%je) = gpf(ib:ie, jb:je)
         qflux(lp%ib:lp%ie, lp%jb:lp%je) = gqf(ib:ie, jb:je)
      end associate
   end subroutine read_checkpoint_core

   ! (M,N) on the IO rank, (1,1) dummy elsewhere — the gather_field contract.
   subroutine alloc_global(comm, grid, glob)
      type(type_comm), intent(inout) :: comm
      type(type_grid_2d), intent(in) :: grid
      real(SP), allocatable, intent(out) :: glob(:, :)
      if (comm%is_io_node()) then
         allocate (glob(grid%M, grid%N))
      else
         allocate (glob(1, 1))
      end if
   end subroutine alloc_global

   subroutine alloc_global_set(comm, grid, glob)
      type(type_comm), intent(inout) :: comm
      type(type_grid_2d), intent(in) :: grid
      real(SP), allocatable, intent(out) :: glob(:, :, :)
      if (comm%is_io_node()) then
         allocate (glob(grid%M, grid%N, SED_NFIELD))
      else
         allocate (glob(1, 1, SED_NFIELD))
      end if
   end subroutine alloc_global_set

   ! Slice the interior (drop the ghost frame) and gather to the IO rank.
   subroutine gather_interior(g, comm, grid, arr, glob)
      type(type_output_gatherer), intent(in) :: g
      type(type_comm), intent(inout) :: comm
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: arr(:, :)
      real(SP), intent(inout) :: glob(:, :)
      associate (ng => N_GHOST, nx => grid%local_nx, ny => grid%local_ny)
         call g%gather_field(arr(ng + 1:ng + nx, ng + 1:ng + ny), glob, comm)
      end associate
   end subroutine gather_interior

end module model_checkpoint_mod
