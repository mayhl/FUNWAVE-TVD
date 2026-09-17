!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Time-averaged statistics — port of legacy MIXING_STUFF /
!  CALCULATE_MEAN / PREVIEW_MEAN (old/mixing.F + old/io.F).
!
!  Accumulates dt-weighted sums of eta/u/v and the cell-centred
!  interface fluxes every step once time >= STEADY_TIME, plus the
!  per-cell zero-up-crossing wave-height counters.  When the window
!  T_sum >= T_INTV_mean closes it freezes the means, updates the
!  wave-height statistics and rolls T_sum over (legacy subtracts the
!  interval, it does not zero).  The legacy PREVIEW_MEAN writer is gone:
!  averaged output is a channel statistic.
!
!  Legacy quirks kept:
!    - the window-closing step updates the mean sums but SKIPS the
!      zero-crossing update (and vice versa) — mixing.F branch shape
!    - Num_Zero_Up / HavgSum / HrmsSum are cumulative since
!      STEADY_TIME (legacy never resets them)
!    - $H_{sig} = 4.004\,\sqrt{\overline{\eta'^2}}$, and the crossing
!      test uses eta relative to the LAST closed window's ETAmean
!    - ETA2sum in the closing step uses the previous ETAmean before
!      the new one is assigned
!
!  Radiation stresses: the depth-integrated momentum
!  flux of the wave-induced velocity fluctuation, closed on the same
!  T_sum window as the means,
!    $$ S_{xx} = \overline{u'^2 H} + \tfrac12 g\,\overline{\eta'^2},\quad
!       S_{xy} = \overline{u'v'H},\quad
!       S_{yy} = \overline{v'^2 H} + \tfrac12 g\,\overline{\eta'^2} $$
!  with $u' = u - \bar u$ and $H = \eta - \bar\eta + h$.  fields%u/v ARE
!  the depth-averaged velocity (ubar/heff), so no separate U_davg array
!  is needed; legacy built one in etauv_solver.F:98 as the face-averaged
!  P_center/max(H, MinDepthFrc), which differs from fields%u only in
!  taking the two x-face fluxes rather than the cell-stored ubar.
!
!  FUTURE: the $-\overline{WW}$ term ($\overline{\tfrac14 W_s^2 H}$) is
!  DROPPED for now.  Legacy DID have it — Wsurf is the O(mu^2) surface
!  vertical velocity, etauv_solver.F:247 sets it from the R1 dispersion
!  accumulator — so this is a gap against legacy, not a legacy quirk.
!  It is isotropic: it shifts Sxx and Syy equally and never touches Sxy
!  or the Sxx-Syy anisotropy that drives longshore forcing, so the
!  wave-driven momentum flux is unaffected by the omission.  Restoring
!  it needs the modern kernel to expose its R1 equivalent.
!
!  None of the set is regression-compared.
!
!  HISTORY :
!    07/10/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_means_mod

   use core_constants_mod, only: SP, N_GHOST, GRAV
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_output_gatherer_mod, only: type_output_gatherer
   use core_output_channel_mod, only: write_field_file

   use model_fields_2d_mod, only: type_fields_2d
   use model_output_mod, only: type_model_output

   implicit none

   private
   public :: type_model_means

   type :: type_model_means

      real(SP) :: t_intv_mean = 0.0_SP
      real(SP) :: steady_time = 0.0_SP

      real(SP) :: t_sum = 0.0_SP

      real(SP), allocatable :: umean(:, :), vmean(:, :), etamean(:, :)
      real(SP), allocatable :: usum(:, :), vsum(:, :), etasum(:, :)
      real(SP), allocatable :: eta2sum(:, :), eta2mean(:, :)
      ! radiation stress: dt-sums of the fluctuation products x total
      ! depth, their window means, and the assembled tensor
      real(SP), allocatable :: uusum(:, :), uvsum(:, :), vvsum(:, :)
      real(SP), allocatable :: uumean(:, :), uvmean(:, :), vvmean(:, :)
      real(SP), allocatable :: sxx(:, :), sxy(:, :), syy(:, :)
      real(SP), allocatable :: p_sum(:, :), q_sum(:, :)
      real(SP), allocatable :: p_mean(:, :), q_mean(:, :)
      real(SP), allocatable :: emax(:, :), emin(:, :)
      integer, allocatable :: num_zero_up(:, :)
      real(SP), allocatable :: havg_sum(:, :), hrms_sum(:, :)
      real(SP), allocatable :: wave_height_rms(:, :), wave_height_ave(:, :)
      real(SP), allocatable :: sig_wave_height(:, :)

      ! Output plumbing (mean files bypass the channel: their cadence
      ! is the T_sum window, not a wall-clock interval)
      type(type_output_gatherer) :: gatherer
      type(type_comm), pointer :: comm => null()
      type(type_grid_2d), pointer :: grid => null()
      character(:), allocatable :: folder, fmt

   contains
      procedure :: init_compute => means_init_compute
      procedure :: update => means_update
      procedure :: free => means_free
   end type type_model_means

contains

   subroutine means_init_compute(this, grid, comm, output)
      class(type_model_means), intent(inout) :: this
      type(type_grid_2d), intent(inout), target :: grid
      type(type_comm), intent(inout), target :: comm
      type(type_model_output), intent(in) :: output

      integer :: mloc, nloc

      this%t_intv_mean = output%T_INTV_mean
      this%steady_time = output%STEADY_TIME

      this%grid => grid
      this%comm => comm
      call this%gatherer%init_field(grid, comm)

      this%folder = trim(output%result_folder)
      if (this%folder(len(this%folder):len(this%folder)) /= "/") &
         this%folder = this%folder//"/"
      select case (output%format(1:1))
      case ("B", "b")
         this%fmt = "binary"
      case default
         this%fmt = "ascii"
      end select

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%umean(mloc, nloc), source=0.0_SP)
      allocate (this%vmean(mloc, nloc), source=0.0_SP)
      allocate (this%etamean(mloc, nloc), source=0.0_SP)
      allocate (this%usum(mloc, nloc), source=0.0_SP)
      allocate (this%vsum(mloc, nloc), source=0.0_SP)
      allocate (this%etasum(mloc, nloc), source=0.0_SP)
      allocate (this%eta2sum(mloc, nloc), source=0.0_SP)
      allocate (this%eta2mean(mloc, nloc), source=0.0_SP)
      allocate (this%uusum(mloc, nloc), source=0.0_SP)
      allocate (this%uvsum(mloc, nloc), source=0.0_SP)
      allocate (this%vvsum(mloc, nloc), source=0.0_SP)
      allocate (this%uumean(mloc, nloc), source=0.0_SP)
      allocate (this%uvmean(mloc, nloc), source=0.0_SP)
      allocate (this%vvmean(mloc, nloc), source=0.0_SP)
      allocate (this%sxx(mloc, nloc), source=0.0_SP)
      allocate (this%sxy(mloc, nloc), source=0.0_SP)
      allocate (this%syy(mloc, nloc), source=0.0_SP)
      allocate (this%p_sum(mloc, nloc), source=0.0_SP)
      allocate (this%q_sum(mloc, nloc), source=0.0_SP)
      allocate (this%p_mean(mloc, nloc), source=0.0_SP)
      allocate (this%q_mean(mloc, nloc), source=0.0_SP)
      allocate (this%emax(mloc, nloc), source=0.0_SP)
      allocate (this%emin(mloc, nloc), source=0.0_SP)
      allocate (this%num_zero_up(mloc, nloc), source=0)
      allocate (this%havg_sum(mloc, nloc), source=0.0_SP)
      allocate (this%hrms_sum(mloc, nloc), source=0.0_SP)
      allocate (this%wave_height_rms(mloc, nloc), source=0.0_SP)
      allocate (this%wave_height_ave(mloc, nloc), source=0.0_SP)
      allocate (this%sig_wave_height(mloc, nloc), source=0.0_SP)

      this%t_sum = 0.0_SP

   end subroutine means_init_compute

   ! ----------------------------------------------------------------
   ! Per-step accumulation (legacy CALCULATE_MEAN).  p_int/q_int are
   ! the interface fluxes from the last RK stage; the cell-centred
   ! P_center/Q_center midpoints are formed inline on the interior
   ! (legacy computes them in etauv_solver.F, ghosts stay zero — the
   ! ghost sums are never written).  min_depth_frc feeds the lagm
   ! division at write time only.
   ! ----------------------------------------------------------------
   subroutine means_update(this, f, p_int, q_int, min_depth_frc, dt, time)
      class(type_model_means), intent(inout) :: this
      type(type_fields_2d), intent(in) :: f
      real(SP), intent(in) :: p_int(:, :), q_int(:, :)
      real(SP), intent(in) :: min_depth_frc, dt, time

      integer :: i, j
      real(SP) :: tmpe, tmp_0

      if (time < this%steady_time) return

      this%t_sum = this%t_sum + dt

      if (this%t_sum >= this%t_intv_mean) then

         ! previous ETAmean by construction (assigned below)
         this%eta2sum = (f%eta - this%etamean)*(f%eta - this%etamean)*dt + this%eta2sum
         ! radiation-stress products ride the SAME previous-window means
         ! as eta2sum (assigned below), so u' and H are referenced to the
         ! last closed window exactly as legacy did
         call accumulate_rad_stress(this, f, dt)
         this%eta2mean = this%eta2sum/this%t_sum

         this%usum = f%u*dt + this%usum
         this%vsum = f%v*dt + this%vsum
         this%etasum = f%eta*dt + this%etasum
         this%umean = this%usum/this%t_sum
         this%vmean = this%vsum/this%t_sum
         this%etamean = this%etasum/this%t_sum

         call accumulate_pq_center(this, p_int, q_int, dt)
         this%p_mean = this%p_sum/this%t_sum
         this%q_mean = this%q_sum/this%t_sum

         this%t_sum = this%t_sum - this%t_intv_mean
         this%usum = 0.0_SP
         this%vsum = 0.0_SP
         this%etasum = 0.0_SP
         this%eta2sum = 0.0_SP
         this%p_sum = 0.0_SP
         this%q_sum = 0.0_SP

         ! assemble the tensor before the sums roll over; the isotropic
         ! pressure term rides eta2mean (already divided above)
         this%uumean = this%uusum/this%t_sum
         this%uvmean = this%uvsum/this%t_sum
         this%vvmean = this%vvsum/this%t_sum
         this%sxx = this%uumean + 0.5_SP*GRAV*this%eta2mean
         this%sxy = this%uvmean
         this%syy = this%vvmean + 0.5_SP*GRAV*this%eta2mean
         this%uusum = 0.0_SP
         this%uvsum = 0.0_SP
         this%vvsum = 0.0_SP

         this%sig_wave_height = 4.004_SP*sqrt(this%eta2mean)

         do j = 1, size(f%eta, 2)
            do i = 1, size(f%eta, 1)
               if (this%num_zero_up(i, j) >= 2) then
                  this%wave_height_ave(i, j) = this%havg_sum(i, j)/this%num_zero_up(i, j)
                  this%wave_height_rms(i, j) = sqrt(this%hrms_sum(i, j)/this%num_zero_up(i, j))
               end if
            end do
         end do

      else

         this%usum = f%u*dt + this%usum
         this%vsum = f%v*dt + this%vsum
         this%etasum = f%eta*dt + this%etasum
         this%eta2sum = (f%eta - this%etamean)*(f%eta - this%etamean)*dt + this%eta2sum
         ! radiation-stress products ride the SAME previous-window means
         ! as eta2sum (assigned below), so u' and H are referenced to the
         ! last closed window exactly as legacy did
         call accumulate_rad_stress(this, f, dt)
         call accumulate_pq_center(this, p_int, q_int, dt)

         ! zero-up-crossing wave tracker (legacy mixing.F 281-299)
         do j = 1, size(f%eta, 2)
            do i = 1, size(f%eta, 1)
               if (f%eta(i, j) > this%emax(i, j)) this%emax(i, j) = f%eta(i, j)
               if (f%eta(i, j) < this%emin(i, j)) this%emin(i, j) = f%eta(i, j)
               tmpe = f%eta(i, j) - this%etamean(i, j)
               tmp_0 = f%eta0(i, j) - this%etamean(i, j)
               if (tmpe > tmp_0 .and. tmpe*tmp_0 <= 0.0_SP) then
                  this%num_zero_up(i, j) = this%num_zero_up(i, j) + 1
                  if (this%num_zero_up(i, j) >= 2) then
                     this%havg_sum(i, j) = this%havg_sum(i, j) &
                                           + this%emax(i, j) - this%emin(i, j)
                     this%hrms_sum(i, j) = this%hrms_sum(i, j) &
                                           + (this%emax(i, j) - this%emin(i, j))**2
                  end if
                  ! reset to find the next wave
                  this%emax(i, j) = -1000.0_SP
                  this%emin(i, j) = 1000.0_SP
               end if
            end do
         end do

      end if

   end subroutine means_update

   ! Radiation-stress dt-sums: fluctuation products weighted by the
   ! instantaneous depth measured from the mean surface.  f%u/f%v are
   ! the depth-averaged velocity (p/H), which is what the momentum flux
   ! wants -- see the module header on legacy's empty U_davg.
   subroutine accumulate_rad_stress(this, f, dt)
      class(type_model_means), intent(inout) :: this
      type(type_fields_2d), intent(in) :: f
      real(SP), intent(in) :: dt

      integer :: i, j
      real(SP) :: up, vp, hh

      do j = 1, size(f%eta, 2)
         do i = 1, size(f%eta, 1)
            up = f%u(i, j) - this%umean(i, j)
            vp = f%v(i, j) - this%vmean(i, j)
            hh = f%eta(i, j) - this%etamean(i, j) + f%depth(i, j)
            this%uusum(i, j) = this%uusum(i, j) + up*up*hh*dt
            this%uvsum(i, j) = this%uvsum(i, j) + up*vp*hh*dt
            this%vvsum(i, j) = this%vvsum(i, j) + vp*vp*hh*dt
         end do
      end do

   end subroutine accumulate_rad_stress

   ! P_center/Q_center dt-sums on the interior (legacy stage-3 values)
   subroutine accumulate_pq_center(this, p_int, q_int, dt)
      class(type_model_means), intent(inout) :: this
      real(SP), intent(in) :: p_int(:, :), q_int(:, :)
      real(SP), intent(in) :: dt

      integer :: i, j

      associate (lp => this%grid%lp)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               this%p_sum(i, j) = this%p_sum(i, j) &
                                  + 0.5_SP*(p_int(i + 1, j) + p_int(i, j))*dt
               this%q_sum(i, j) = this%q_sum(i, j) &
                                  + 0.5_SP*(q_int(i, j + 1) + q_int(i, j))*dt
            end do
         end do
      end associate

   end subroutine accumulate_pq_center

   subroutine means_free(this)
      class(type_model_means), intent(inout) :: this

      call this%gatherer%finalize()
      if (allocated(this%umean)) then
         deallocate (this%umean, this%vmean, this%etamean, this%usum, &
                     this%vsum, this%etasum, this%eta2sum, this%eta2mean, &
                     this%p_sum, this%q_sum, this%p_mean, this%q_mean, &
                     this%emax, this%emin, this%num_zero_up, this%havg_sum, &
                     this%hrms_sum, this%wave_height_rms, &
                     this%wave_height_ave, this%sig_wave_height)
      end if
      this%comm => null()
      this%grid => null()

   end subroutine means_free

end module model_means_mod
