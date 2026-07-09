!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D wavemaker parameters YAML reader
!
!  YAML block: wavemaker:          (optional; omit for no wavemaker)
!    type: <string>                WaveMaker, default 'nothing'
!    --- ABS absorbing-generating ---
!    wave_comp_file: <path>        LinearWaveSerFile
!    west_width: <real>            WaveMaker_West_Width
!    east_width: <real>            WaveMaker_East_Width
!    r_wavemaker: <real>           R_WaveMaker
!    a_wavemaker: <real>           A_WaveMaker
!    --- LEF / INT / FLU regular wave ---
!    amp: <real>                   Amp_Wave
!    per: <real>                   Per_Wave
!    dep: <real>                   Dep_Wave
!    theta: <real>                 Theta_Wave
!    --- INT internal source only ---
!    xsource_west: <real>
!    xsource_east: <real>
!    ysource_suth: <real>
!    ysource_nrth: <real>
!    --- JON spectrum (wavemaker type contains 'JON') ---
!    hm0: <real>
!    tp: <real>
!    freq_min: <real>
!    freq_max: <real>
!    num_freq: <int>
!
!  SPC wavemaker reads from hardcoded file 'spc2d.txt'.
!  JON spectrum computation is done inside read_input.
!  Periodic-Y angle correction stays in READ_INPUT.
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_wavemaker_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_wavemaker

   integer, parameter :: MAX_FREQ = 100
   integer, parameter :: MAX_DIR  = 100

   type, extends(type_model_base) :: type_model_3d_wavemaker

      character(:), allocatable :: wavemaker_type

      character(:), allocatable :: wave_comp_file
      real(SP) :: dep_ser          = 0.0_SP
      real(SP) :: u_flow_left      = 0.0_SP
      real(SP) :: u_flow_right     = 0.0_SP
      integer  :: num_comp_ser     = 0
      real(SP), allocatable :: amp_ser(:)
      real(SP), allocatable :: per_ser(:)
      real(SP), allocatable :: phase_ser(:)
      real(SP), allocatable :: theta_ser(:)
      real(SP), allocatable :: segma_ser(:)
      real(SP), allocatable :: wave_number_ser(:)
      real(SP), allocatable :: stokes_drift_ser(:)
      real(SP) :: west_width   = 0.0_SP
      real(SP) :: east_width   = 0.0_SP
      real(SP) :: r_wavemaker  = 0.0_SP
      real(SP) :: a_wavemaker  = 0.0_SP

      real(SP) :: amp_wave     = 0.0_SP
      real(SP) :: per_wave     = 0.0_SP
      real(SP) :: dep_wave     = 0.0_SP
      real(SP) :: theta_wave   = 0.0_SP
      real(SP) :: xsource_west = 0.0_SP
      real(SP) :: xsource_east = 0.0_SP
      real(SP) :: ysource_suth = 0.0_SP
      real(SP) :: ysource_nrth = 0.0_SP

      integer  :: num_freq     = 0
      integer  :: num_dir      = 0
      real(SP) :: freq(MAX_FREQ)             = 0.0_SP
      real(SP) :: dire(MAX_DIR)              = 0.0_SP
      real(SP) :: wave_spc2d(MAX_DIR, MAX_FREQ) = 0.0_SP
      real(SP) :: random_phs(MAX_DIR, MAX_FREQ) = 0.0_SP

      real(SP) :: hm0          = 0.0_SP
      real(SP) :: tp           = 0.0_SP
      real(SP) :: freq_min     = 0.0_SP
      real(SP) :: freq_max     = 0.0_SP
      real(SP) :: jon_spc(MAX_FREQ) = 0.0_SP
      real(SP) :: ran_phs(MAX_FREQ) = 0.0_SP

   contains
      procedure :: read_input => wavemaker_3d_read_input
   end type type_model_3d_wavemaker

contains

   subroutine wavemaker_3d_read_input(this, env)
      class(type_model_3d_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key
      integer :: i, j
      real(SP) :: segma, celerity, wave_length, wave_number_loc
      real(SP) :: freq_peak_loc, gam, sa, sb, sum_int, a_jon
      real(SP) :: dfreq
      real(SP), parameter :: pi = 3.14159265358979_SP
      real(SP), parameter :: zero = 0.0_SP

      sub_env = get_sub_env(env, "wavemaker", is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) then
         this%wavemaker_type = "nothing"
         return
      end if

      call sub_env%yaml%read("type", val=this%wavemaker_type, default="nothing")

      if (this%wavemaker_type(1:3) == "ABS") then
         call sub_env%yaml%read("wave_comp_file", val=this%wave_comp_file)
         open(1, file=trim(this%wave_comp_file))
            read(1, *)
            read(1, *) this%dep_ser
            read(1, *) this%u_flow_left, this%u_flow_right
            read(1, *) this%num_comp_ser
            allocate(this%amp_ser(this%num_comp_ser))
            allocate(this%per_ser(this%num_comp_ser))
            allocate(this%phase_ser(this%num_comp_ser))
            allocate(this%theta_ser(this%num_comp_ser))
            allocate(this%segma_ser(this%num_comp_ser))
            allocate(this%wave_number_ser(this%num_comp_ser))
            allocate(this%stokes_drift_ser(this%num_comp_ser))
            do i = 1, this%num_comp_ser
               read(1, *) this%amp_ser(i), this%per_ser(i), &
                          this%phase_ser(i), this%theta_ser(i)
               if (this%per_ser(i) == zero) then
                  write(*, *) "input wave frequency is zero, stop"
                  stop
               else
                  this%per_ser(i) = 1.0_SP / this%per_ser(i)
               end if
            end do
         close(1)
         call sub_env%yaml%read("west_width",   silent=no_key, val=this%west_width,  default="0.0")
         call sub_env%yaml%read("east_width",   silent=no_key, val=this%east_width,  default="0.0")
         call sub_env%yaml%read("r_wavemaker",  silent=no_key, val=this%r_wavemaker, default="0.0")
         call sub_env%yaml%read("a_wavemaker",  silent=no_key, val=this%a_wavemaker, default="0.0")
      end if

      if (this%wavemaker_type(1:3) == "LEF" .or. &
          this%wavemaker_type(1:3) == "INT" .or. &
          this%wavemaker_type(1:3) == "FLU") then
         call sub_env%yaml%read("amp",   val=this%amp_wave)
         call sub_env%yaml%read("per",   val=this%per_wave)
         call sub_env%yaml%read("dep",   val=this%dep_wave)
         call sub_env%yaml%read("theta", val=this%theta_wave, default="0.0")
         if (this%wavemaker_type(1:3) == "INT") then
            call sub_env%yaml%read("xsource_west", silent=no_key, val=this%xsource_west, default="0.0")
            call sub_env%yaml%read("xsource_east", silent=no_key, val=this%xsource_east, default="0.0")
            call sub_env%yaml%read("ysource_suth", silent=no_key, val=this%ysource_suth, default="0.0")
            call sub_env%yaml%read("ysource_nrth", silent=no_key, val=this%ysource_nrth, default="0.0")
         end if
      end if

      if (this%wavemaker_type(5:7) == "SPC") then
         open(14, file="spc2d.txt")
            read(14, *) this%num_freq, this%num_dir
            do i = 1, this%num_freq
               read(14, *) this%freq(i)
            end do
            do i = 1, this%num_dir
               read(14, *) this%dire(i)
            end do
            do j = 1, this%num_freq
               do i = 1, this%num_dir
                  read(14, *) this%wave_spc2d(i, j)
               end do
            end do
         close(14)
         do j = 1, this%num_freq
            do i = 1, this%num_dir
               this%random_phs(i, j) = rand(0) * 2.0_SP * pi
            end do
         end do
      end if

      if (this%wavemaker_type(5:7) == "JON") then
         call sub_env%yaml%read("hm0",      val=this%hm0)
         call sub_env%yaml%read("tp",       val=this%tp)
         call sub_env%yaml%read("freq_min", val=this%freq_min)
         call sub_env%yaml%read("freq_max", val=this%freq_max)
         call sub_env%yaml%read("num_freq", val=this%num_freq)

         dfreq     = (this%freq_max - this%freq_min) / this%num_freq
         do i = 1, this%num_freq
            this%freq(i) = this%freq_min + 0.5_SP * dfreq + (i - 1) * dfreq
         end do

         gam       = 3.3_SP
         sa        = 0.07_SP
         sb        = 0.09_SP
         freq_peak_loc = 1.0_SP / this%tp
         do i = 1, this%num_freq
            if (this%freq(i) < freq_peak_loc) then
               this%jon_spc(i) = 9.81_SP**2 / this%freq(i)**5 * &
                  exp(-1.25_SP * (freq_peak_loc / this%freq(i))**4) * &
                  gam**(-0.5_SP * (this%freq(i) / freq_peak_loc - 1.0_SP)**2 / sa**2)
            else
               this%jon_spc(i) = 9.81_SP**2 / this%freq(i)**5 * &
                  exp(-1.25_SP * (freq_peak_loc / this%freq(i))**4) * &
                  gam**(-0.5_SP * (this%freq(i) / freq_peak_loc - 1.0_SP)**2 / sb**2)
            end if
         end do
         sum_int = 0.0_SP
         do i = 1, this%num_freq
            sum_int = sum_int + this%jon_spc(i) * dfreq
         end do
         a_jon = this%hm0**2 / 16.0_SP / sum_int
         do i = 1, this%num_freq
            this%jon_spc(i) = this%jon_spc(i) * a_jon
            this%ran_phs(i) = rand(0) * 2.0_SP * pi
         end do
      end if

   end subroutine wavemaker_3d_read_input

end module model_3d_wavemaker_mod
