

SUBROUTINE READ_INPUT
     USE GLOBAL
     use model_3d_config_mod, only: type_model_3d_config
# if defined (PARALLEL)
     use core_grid_mod, only: compute_optimal_grid_size
# endif
     IMPLICIT NONE

     type(type_model_3d_config) :: config
     character(len=80) :: MKFOLDER, TMP_NAME, FILE_NAME_number
     real(SP) :: Segma_loc, Celerity_loc, Wave_Length_loc, Wave_Number_loc
     real(SP) :: Fk_loc, Fkdif_loc, Theta_Calc_loc, Wnumy_loc
     real(SP) :: tmp_loc, tmp1_loc
     integer  :: Iter_loc, i, j, k

     ! log and unit for time_dt
     OPEN(3,  FILE='LOG.txt')
     OPEN(10000, FILE='time_dt.out', STATUS='UNKNOWN')

     call config%init()

     ! --- Geometry ---
     Mglob = config%geometry%nx
     Nglob = config%geometry%ny
     Kglob = config%geometry%nz
     dx    = config%geometry%dx
     dy    = config%geometry%dy
     Ivgrd = config%geometry%ivgrd
     Grd_R = config%geometry%grd_r
# if defined (PARALLEL)
     PX    = config%geometry%nx_proc
     PY    = config%geometry%ny_proc
     ! Resolve auto-decomposition: a 0 sentinel (nx_proc/ny_proc omitted in YAML)
     ! means pick the PX*PY factor pair of NumP minimising local-tile aspect ratio,
     ! using the same algorithm as the 2D layer.  NumP is seeded in MODULE GLOBAL
     ! by INIT_3D_GLOBAL before READ_INPUT, so the rank count is available here.
     ! This guarantees the legacy cart-topology math never divides by a zero PX/PY.
     if (PX <= 0 .or. PY <= 0) then
       call compute_optimal_grid_size(NumP, Mglob, Nglob, PX, PY)
     elseif (PX*PY /= NumP) then
       if (myid == 0) write(*,*) &
         'ERROR: geometry/decomposition nx_proc*ny_proc =', PX*PY, &
         ' must equal MPI rank count =', NumP
       call MPI_ABORT(MPI_COMM_WORLD, 1, ier)
     endif
# endif
     DEPTH_TYPE = config%geometry%bathy_type
     if (allocated(config%geometry%depth_file)) &
        Depth_File = config%geometry%depth_file
     ANA_BATHY  = config%geometry%ana_bathy
     Ibot       = config%geometry%roughness_type
     Cd0        = config%geometry%cd
     Zob        = config%geometry%zob
     MinDep     = config%geometry%min_depth

     ! --- Simulation ---
     TOTAL_TIME     = config%simulation%total_time
     SIM_STEPS      = config%simulation%sim_steps
     Plot_Start     = config%simulation%plot_start
     Plot_Intv      = config%simulation%plot_intv
     Screen_Intv    = config%simulation%screen_intv
     CFL            = config%simulation%cfl
     dt_ini         = config%simulation%dt_ini
     dt_min         = config%simulation%dt_min
     dt_max         = config%simulation%dt_max
     NSTAT          = config%simulation%nstat
     Plot_Intv_Stat = config%simulation%plot_intv_stat
     if (NSTAT > 0 .and. allocated(config%simulation%stations_file)) &
        STATIONS_FILE = config%simulation%stations_file

     ! --- Turbulence ---
     VISCOUS_FLOW   = config%turbulence%viscous_flow
     IVturb         = config%turbulence%ivturb
     IHturb         = config%turbulence%ihturb
     Visc           = config%turbulence%visc
     Schmidt        = config%turbulence%schmidt
     Cvs            = config%turbulence%cvs
     Chs            = config%turbulence%chs
     VISCOUS_NUMBER = config%turbulence%viscous_number

     ! --- Hot start ---
     HOTSTART = config%hotstart%is_activated
     if (HOTSTART) then
        if (allocated(config%hotstart%eta_file)) &
           Eta_HotStart_File = config%hotstart%eta_file
        if (allocated(config%hotstart%u_file)) &
           U_HotStart_File   = config%hotstart%u_file
        if (allocated(config%hotstart%v_file)) &
           V_HotStart_File   = config%hotstart%v_file
        if (allocated(config%hotstart%w_file)) &
           W_HotStart_File   = config%hotstart%w_file
        if (allocated(config%hotstart%p_file)) &
           P_HotStart_File   = config%hotstart%p_file
# if defined (BAROCLINIC)
        if (allocated(config%hotstart%sali_file)) &
           Sali_HotStart_File = config%hotstart%sali_file
        if (allocated(config%hotstart%temp_file)) &
           Temp_HotStart_File = config%hotstart%temp_file
# endif
        if (VISCOUS_FLOW) then
           if (allocated(config%hotstart%rho_file)) &
              Rho_HotStart_File = config%hotstart%rho_file
           if (allocated(config%hotstart%tke_file)) &
              TKE_HotStart_File = config%hotstart%tke_file
           if (allocated(config%hotstart%eps_file)) &
              EPS_HotStart_File = config%hotstart%eps_file
        end if
# if defined (AIR_PRESSURE)
        if (allocated(config%hotstart%pressure_file)) &
           Pressure_HotStart_File = config%hotstart%pressure_file
# endif
     end if

     ! --- Physics ---
     BAROTROPIC       = config%physics%barotropic
     NON_HYDRO        = config%physics%non_hydro
     if (allocated(config%physics%high_order)) &
        HIGH_ORDER     = config%physics%high_order
     if (allocated(config%physics%time_order)) &
        TIME_ORDER     = config%physics%time_order
     if (allocated(config%physics%convection)) &
        CONVECTION     = config%physics%convection
     ADV_HLLC         = config%physics%adv_hllc
     TRamp            = config%physics%tramp
     PERIODIC_X       = config%physics%periodic_x
     PERIODIC_Y       = config%physics%periodic_y
     EXTERNAL_FORCING = config%physics%external_forcing
# if defined (FROUDE_CAP)
     FROUDECAP        = config%physics%froude_cap
# endif
     WAVE_AVERAGE_ON  = config%physics%wave_average_on
     Wave_Ave_Start   = config%physics%wave_ave_start
     Wave_Ave_End     = config%physics%wave_ave_end
     WaveheightID     = config%physics%waveheight_id

     ! --- Solver ---
     isolver = config%solver%solver_type
     itmax   = config%solver%max_iter
     tol     = config%solver%tolerance

     ! --- Boundary conditions ---
     Bc_X0    = config%bc%bc_x0
     Bc_Xn    = config%bc%bc_xn
     Bc_Y0    = config%bc%bc_y0
     Bc_Yn    = config%bc%bc_yn
     Bc_Z0    = config%bc%bc_z0
     Bc_Zn    = config%bc%bc_zn
     if (allocated(config%bc%boundary_type)) &
        BOUNDARY = config%bc%boundary_type
     if (config%bc%num_time_data > 0) then
        if (allocated(config%bc%boundary_file)) &
           BoundaryFile = config%bc%boundary_file
        NumTimeData = config%bc%num_time_data
        call move_alloc(config%bc%time_data,    TimeData)
        call move_alloc(config%bc%data_u_l,     DataU_L)
        call move_alloc(config%bc%data_u_r,     DataU_R)
        call move_alloc(config%bc%data_eta_l,   DataEta_L)
        call move_alloc(config%bc%data_eta_r,   DataEta_R)
        call move_alloc(config%bc%data_sal_l,   DataSal_L)
        call move_alloc(config%bc%data_sal_r,   DataSal_R)
        call move_alloc(config%bc%data_tem_l,   DataTem_L)
        call move_alloc(config%bc%data_tem_r,   DataTem_R)
        if (allocated(config%bc%z_pct_west)) &
           call move_alloc(config%bc%z_pct_west, Z_pct_West)
        if (allocated(config%bc%z_pct_east)) &
           call move_alloc(config%bc%z_pct_east, Z_pct_East)
     end if

     ! --- Wavemaker ---
     if (allocated(config%wavemaker%wavemaker_type)) then
        WaveMaker = config%wavemaker%wavemaker_type
     else
        WaveMaker = 'nothing'
     end if

     if (WaveMaker(1:3) == 'ABS') then
        if (allocated(config%wavemaker%wave_comp_file)) &
           Wave_Comp_File   = config%wavemaker%wave_comp_file
        Dep_Ser          = config%wavemaker%dep_ser
        U_FLOW_LEFT      = config%wavemaker%u_flow_left
        U_FLOW_RIGHT     = config%wavemaker%u_flow_right
        NumCompSer       = config%wavemaker%num_comp_ser
        call move_alloc(config%wavemaker%amp_ser,          Amp_Ser)
        call move_alloc(config%wavemaker%per_ser,          Per_Ser)
        call move_alloc(config%wavemaker%phase_ser,        Phase_Ser)
        call move_alloc(config%wavemaker%theta_ser,        Theta_Ser)
        call move_alloc(config%wavemaker%segma_ser,        Segma_Ser)
        call move_alloc(config%wavemaker%wave_number_ser,  Wave_Number_Ser)
        call move_alloc(config%wavemaker%stokes_drift_ser, Stokes_Drift_Ser)
        WaveMaker_West_Width = config%wavemaker%west_width
        WaveMaker_East_Width = config%wavemaker%east_width
        R_WaveMaker          = config%wavemaker%r_wavemaker
        A_WaveMaker          = config%wavemaker%a_wavemaker
     end if

     if (WaveMaker(1:3) == 'LEF' .or. WaveMaker(1:3) == 'INT' .or. &
         WaveMaker(1:3) == 'FLU') then
        Amp_Wave    = config%wavemaker%amp_wave
        Per_Wave    = config%wavemaker%per_wave
        Dep_Wave    = config%wavemaker%dep_wave
        Theta_Wave  = config%wavemaker%theta_wave
        if (WaveMaker(1:3) == 'INT') then
           Xsource_West = config%wavemaker%xsource_west
           Xsource_East = config%wavemaker%xsource_east
           Ysource_Suth = config%wavemaker%ysource_suth
           Ysource_Nrth = config%wavemaker%ysource_nrth
        end if

        ! Periodic-Y angle correction — uses MODULE GLOBAL Grav, Nglob, dy
        if (PERIODIC_Y .and. Theta_Wave .ne. 0.0_SP) then
           Segma_loc    = 2.0_SP * pi / Per_Wave
           Celerity_loc = sqrt(Grav * Dep_Wave)
           Wave_Length_loc = Celerity_loc * Per_Wave
           Wave_Number_loc = 2.0_SP * pi / Wave_Length_loc
           Iter_loc = 0
 75        Fk_loc = Grav * Wave_Number_loc * tanh(Wave_Number_loc * Dep_Wave) - Segma_loc**2
           if (abs(Fk_loc) <= 1.0e-8_SP .or. Iter_loc > 1000) goto 85
           Fkdif_loc = Grav * Wave_Number_loc * Dep_Wave * &
              (1.0_SP - tanh(Wave_Number_loc * Dep_Wave)**2) + &
              Grav * tanh(Wave_Number_loc * Dep_Wave)
           Wave_Number_loc = Wave_Number_loc - Fk_loc / Fkdif_loc
           Iter_loc = Iter_loc + 1
           goto 75
 85        continue
           if (Theta_Wave > 0.0_SP) then
              tmp_loc = 1.0e30_SP
              do Iter_loc = 1, 10000
                 Wnumy_loc = Iter_loc * 2.0_SP * pi / (Nglob * dy)
                 if (Wnumy_loc < Wave_Number_loc) then
                    tmp1_loc = asin(Wnumy_loc / Wave_Number_loc) * 180.0_SP / pi
                    if (abs(tmp1_loc - Theta_Wave) < tmp_loc) then
                       tmp_loc = abs(tmp1_loc - Theta_Wave)
                       Theta_Calc_loc = tmp1_loc
                    end if
                 end if
              end do
           else if (Theta_Wave < 0.0_SP) then
              tmp_loc = 1.0e30_SP
              do Iter_loc = 1, 10000
                 Wnumy_loc = Iter_loc * 2.0_SP * pi / (Nglob * dy)
                 if (Wnumy_loc < Wave_Number_loc) then
                    tmp1_loc = -asin(Wnumy_loc / Wave_Number_loc) * 180.0_SP / pi
                    if (abs(tmp1_loc - Theta_Wave) < tmp_loc) then
                       tmp_loc = abs(tmp1_loc - Theta_Wave)
                       Theta_Calc_loc = tmp1_loc
                    end if
                 end if
              end do
           end if
# if defined (PARALLEL)
           if (myid.eq.0) then
              write(3, '(A20,F6.3)') 'Wave angle you set= ',  Theta_Wave
              write(3, '(A28,F6.3)') 'Wave angle for periodic bc= ', Theta_Calc_loc
           end if
# else
           write(3, '(A20,F6.3)') 'Wave angle you set= ',  Theta_Wave
           write(3, '(A28,F6.3)') 'Wave angle for periodic bc= ', Theta_Calc_loc
# endif
           Theta_Wave = Theta_Calc_loc
        end if
     end if

     if (WaveMaker(5:7) == 'SPC') then
        NumFreq = config%wavemaker%num_freq
        NumDir  = config%wavemaker%num_dir
        Freq(1:NumFreq)                        = config%wavemaker%freq(1:NumFreq)
        Dire(1:NumDir)                         = config%wavemaker%dire(1:NumDir)
        Wave_Spc2d(1:NumDir, 1:NumFreq)        = config%wavemaker%wave_spc2d(1:NumDir, 1:NumFreq)
        Random_Phs(1:NumDir, 1:NumFreq)        = config%wavemaker%random_phs(1:NumDir, 1:NumFreq)
     end if

     if (WaveMaker(5:7) == 'JON') then
        Hm0      = config%wavemaker%hm0
        Tp       = config%wavemaker%tp
        Freq_Min = config%wavemaker%freq_min
        Freq_Max = config%wavemaker%freq_max
        NumFreq  = config%wavemaker%num_freq
        Freq(1:NumFreq)     = config%wavemaker%freq(1:NumFreq)
        Jon_Spc(1:NumFreq)  = config%wavemaker%jon_spc(1:NumFreq)
        RanPhs(1:NumFreq)   = config%wavemaker%ran_phs(1:NumFreq)
     end if

     ! --- Sponge ---
     SPONGE_ON           = config%sponge%is_activated
     if (SPONGE_ON) then
        Sponge_West_Width  = config%sponge%west_width
        Sponge_East_Width  = config%sponge%east_width
        Sponge_South_Width = config%sponge%south_width
        Sponge_North_Width = config%sponge%north_width
        R_Sponge           = config%sponge%r_sponge
        A_Sponge           = config%sponge%a_sponge
     end if

     ! --- Output ---
     if (allocated(config%output%result_folder)) &
        RESULT_FOLDER = config%output%result_folder
     OUT_DEP  = config%output%out_dep
     OUT_ETA  = config%output%out_eta
     OUT_U    = config%output%out_u
     OUT_V    = config%output%out_v
     OUT_W    = config%output%out_w
     OUT_P    = config%output%out_p
     OUT_TKE  = config%output%out_tke
     OUT_EPS  = config%output%out_eps
     OUT_S    = config%output%out_s
     OUT_MU   = config%output%out_mu
     OUT_BUB  = config%output%out_bub
     OUT_A    = config%output%out_a
     OUT_F    = config%output%out_f
     OUT_T    = config%output%out_t
     OUT_G    = config%output%out_g
     OUT_SALI = config%output%out_sali
     OUT_TEMP = config%output%out_temp
     OUT_RHO  = config%output%out_rho

# if defined (SEDIMENT)
     Sed_Type  = config%sed_type
     Sed_Load  = config%sed_load
     COUPLE_FS = config%couple_fs
     SD50      = config%sd50
     Shields_c = config%shields_c
     Af        = config%af
     Tau_ce    = config%tau_ce
     Tau_cd    = config%tau_cd
     Erate     = config%erate
     Mud_Visc  = config%mud_visc
     Tim_Sedi  = config%tim_sedi
     BED_CHANGE = config%bed_change
# endif

# if defined (OBSTACLE)
     MASK3D_File = config%mask3d_file
# endif

# if defined (COUPLING)
     if (config%coupling%is_activated) then
        COUPLING_FILE = config%coupling%coupling_file%root
     end if
# endif

# if defined (BAROCLINIC)
     if (config%baroclinic%is_activated) then
        INI_SALI_INPUT = config%baroclinic%ini_sali_input
        INI_SALI       = config%baroclinic%ini_sali
        if (allocated(config%baroclinic%ini_sali_file)) &
           INI_SALI_FILE = config%baroclinic%ini_sali_file
        INI_TEMP_INPUT = config%baroclinic%ini_temp_input
        INI_TEMP       = config%baroclinic%ini_temp
        if (allocated(config%baroclinic%ini_temp_file)) &
           INI_TEMP_FILE = config%baroclinic%ini_temp_file
        TID_LOW_PASS   = config%baroclinic%tid_low_pass
     end if
# endif

     ! --- Create result folder ---
# if defined (PARALLEL)
     if (myid .eq. 0) then
# endif
        MKFOLDER = 'mkdir -p '//TRIM(RESULT_FOLDER)
        CALL SYSTEM(TRIM(MKFOLDER))
# if defined (PARALLEL)
     end if
# endif

     ! --- Open station output files ---
     if (NSTAT > 0) then
        ALLOCATE(xstat(NSTAT), ystat(NSTAT))
        FDIR = TRIM(RESULT_FOLDER)
        FILE_NAME_number = ' '
        open(15, file=TRIM(STATIONS_FILE))
        do i = 1, NSTAT
           read(15, *) xstat(i), ystat(i)
           write(FILE_NAME_number(1:4), '(I4.4)') i
           TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME_number)
           iunit = 100 + i
           open(iunit, FILE=TMP_NAME)
        end do
        close(15)
     end if

END SUBROUTINE READ_INPUT


     subroutine preview
!--------------------------------------------------- 
!    This subroutine is used to preview
!    Called by                         
!       main 
!--------------------------------------------------
     use global
! fyshi add foam
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif
     implicit none
     integer :: i,j,k,I1,I2,I3,I4
!     character(len=80) :: FDIR=' ' ! move to mod_glob
     character(len=80) ::FILE_NAME=' '
     character(LEN=80) :: file=' '
     real(SP), dimension(Mloc,Nloc) :: Int2Flo
     real(SP), dimension(Mloc,Nloc,Kloc) :: Int2Flo3D

     ! file number
     Icount = Icount+1
   
     ! results directory
     FDIR = TRIM(RESULT_FOLDER)

# if defined (PARALLEL)
     if(myid.eq.0) write(*,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
     if(myid.eq.0) write(3,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME     
# else
     write(*,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
     write(3,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
# endif
102  FORMAT(A20,I4,A14,F16.3,A2,F16.3)
100  FORMAT(15000E16.6)

     I1 = mod(Icount/1000,10)
     I2 = mod(Icount/100,10)
     I3 = mod(Icount/10,10)
     I4 = mod(Icount,10)

     write(FILE_NAME(1:1),'(I1)') I1
     write(FILE_NAME(2:2),'(I1)') I2
     write(FILE_NAME(3:3),'(I1)') I3
     write(FILE_NAME(4:4),'(I1)') I4

# if defined (PARALLEL)
     if(myid.eq.0) then
# endif
     open(5,file=TRIM(FDIR)//'time',position="append")
     write(5,*) TIME
     close(5)
# if defined (PARALLEL)
     endif
# endif

     if(Icount==1) then
       if(OUT_DEP) then
         file=TRIM(FDIR)//'depth'
         call putfile2D(file,Hc(1:Mloc,1:Nloc))
       endif
     endif

     if(OUT_ETA) then
       file=TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
       call putfile2D(file,Eta(1:Mloc,1:Nloc))
     endif

     if(OUT_U) then
       file=TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
       call putfile3D(file,U(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_V) then
       file=TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
       call putfile3D(file,V(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_W) then
       file=TRIM(FDIR)//'w_'//TRIM(FILE_NAME)
       call putfile3D(file,W(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_P) then
       file=TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
       call putfile3D(file,P(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_TKE) then
       file=TRIM(FDIR)//'tke_'//TRIM(FILE_NAME)
       call putfile3D(file,Tke(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_EPS) then
       file=TRIM(FDIR)//'eps_'//TRIM(FILE_NAME)
       call putfile3D(file,Eps(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_S) then
       file=TRIM(FDIR)//'prod_'//TRIM(FILE_NAME)
       call putfile3D(file,Prod_s(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_MU) then
       file=TRIM(FDIR)//'mu_'//TRIM(FILE_NAME)
       call putfile3D(file,CmuVt(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_A) then
       file=TRIM(FDIR)//'upwp_'//TRIM(FILE_NAME)
       call putfile3D(file,UpWp(1:Mloc,1:Nloc,1:Kloc))
     endif
# if defined (OBSTACLE)
      file=TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
       Int2Flo3D=Mask3D
       call putfile3D(file,Int2Flo3D(1:Mloc,1:Nloc,1:Kloc))
# endif

# if defined (BUBBLE)
     if(OUT_BUB) then
       file=TRIM(FDIR)//'b_'//TRIM(FILE_NAME)
       call putfile3D(file,Vbg(1:Mloc,1:Nloc,1:Kloc))
     endif
# endif

# if defined (SEDIMENT)
     if(OUT_F) then
       file=TRIM(FDIR)//'f_'//TRIM(FILE_NAME)
       call putfile3D(file,Conc(1:Mloc,1:Nloc,1:Kloc))
     endif
     if(OUT_T) then
       file=TRIM(FDIR)//'t_'//TRIM(FILE_NAME)
       call putfile2D(file,Taub(1:Mloc,1:Nloc))
     endif
     if(OUT_G) then
       file=TRIM(FDIR)//'g_'//TRIM(FILE_NAME)
       call putfile2D(file,Bed(1:Mloc,1:Nloc))
     endif
# endif

# if defined (BAROCLINIC)
     if(OUT_SALI) then
       file=TRIM(FDIR)//'sali_'//TRIM(FILE_NAME)
       call putfile3D(file,Sali(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_TEMP) then
       file=TRIM(FDIR)//'temp_'//TRIM(FILE_NAME)
       call putfile3D(file,Temp(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_RHO) then
       file=TRIM(FDIR)//'rho_'//TRIM(FILE_NAME)
       call putfile3D(file,Rho(1:Mloc,1:Nloc,1:Kloc))
     endif
# endif

! fyshi add foam
# if defined (VESSEL)
     if(OUT_VESSEL) then
       file=TRIM(FDIR)//'Pves_'//TRIM(FILE_NAME)
       call putfile2D(file,VesselPressureTotal(1:Mloc,1:Nloc))
     endif
# endif

     end subroutine preview


# if defined (PARALLEL)

    subroutine putfile2D(file,phi)
    use global
    implicit none
    real(SP),dimension(Mloc,Nloc),intent(in) :: phi
    character(len=80) :: file
    integer,dimension(NumP) :: npxs,npys
    integer,dimension(1) :: req
    real(SP),dimension(Mloc,Nloc) :: xx
    real(SP),dimension(Mglob,Nglob) :: phiglob
    integer,dimension(MPI_STATUS_SIZE,1) :: status
    integer :: i,j,iglob,jglob,len,n

    call MPI_GATHER(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,  &
           0,MPI_COMM_WORLD,ier)
    call MPI_GATHER(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,  &
           0,MPI_COMM_WORLD,ier)

    ! put the data in master processor into the global var
    if(myid==0) then
      do j = Jbeg,Jend
      do i = Ibeg,Iend
        iglob = i-Nghost
        jglob = j-Nghost
        phiglob(iglob,jglob) = Phi(i,j)
      enddo
      enddo
    endif

    ! collect data from other processors into the master processor
    len = Mloc*Nloc

    do n = 1,NumP-1
      if(myid==0) then
        call MPI_IRECV(xx,len,MPI_SP,n,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
        do j = Jbeg,Jend
        do i = Ibeg,Iend
          iglob = npxs(n+1)*(Iend-Ibeg+1)+i-Nghost
          jglob = npys(n+1)*(Jend-Jbeg+1)+j-Nghost
          phiglob(iglob,jglob) = xx(i,j)
        enddo
        enddo
      endif

      if(myid==n) then
        call MPI_SEND(phi,len,MPI_SP,0,0,MPI_COMM_WORLD,ier)
      endif
    enddo       

    if(myid==0) then
      open(5,file=TRIM(file))
      do j = 1,Nglob
        write(5,100) (phiglob(i,j),i=1,Mglob)
      enddo
      close(5)
    endif
100 FORMAT(15000f15.5)

    end subroutine putfile2D

    subroutine putfile3D(file,phi)
    use global
    implicit none
    real(SP),dimension(Mloc,Nloc,Kloc),intent(in) :: phi
    character(len=80) :: file
    integer,dimension(NumP) :: npxs,npys
    integer,dimension(1) :: req
    real(SP),dimension(:,:),allocatable :: xx,philoc
    real(SP),dimension(Mglob,Nglob,Kglob) :: phiglob
    integer,dimension(MPI_STATUS_SIZE,1) :: status
    integer :: i,j,k,jk,iglob,jglob,kk,n,len,nreq,NKloc

    call MPI_GATHER(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,  &
          0,MPI_COMM_WORLD,ier)
    call MPI_GATHER(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,  &
          0,MPI_COMM_WORLD,ier)

    NKloc = Nloc*Kloc

    ! put the data in master processor into the global var
    if(myid==0) then
      do k = Kbeg,Kend
      do j = Jbeg,Jend
      do i = Ibeg,Iend
        iglob = i-Nghost
        jglob = j-Nghost
        kk = k-Nghost
        phiglob(iglob,jglob,kk) = Phi(i,j,k)
      enddo
      enddo
      enddo
    endif

    allocate(philoc(Mloc,NKloc))
    allocate(xx(Mloc,NKloc))

    do k = 1,Kloc
    do j = 1,Nloc
    do i = 1,Mloc
      jk = (k-1)*Nloc+j
      philoc(i,jk) = phi(i,j,k)
    enddo
    enddo
    enddo

    ! collect data from other processors into the master processor
    len = Mloc*NKloc

    do n = 1,NumP-1
      if(myid==0) then
        call MPI_IRECV(xx,len,MPI_SP,n,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
        do k = Kbeg,Kend
        do j = Jbeg,Jend
        do i = Ibeg,Iend
          iglob = npxs(n+1)*(Iend-Ibeg+1)+i-Nghost
          jglob = npys(n+1)*(Jend-Jbeg+1)+j-Nghost
          kk = k-Nghost
          jk = (k-1)*Nloc+j
          phiglob(iglob,jglob,kk) = xx(i,jk)
        enddo
        enddo
        enddo
      endif

      if(myid==n) then
        call MPI_SEND(philoc,len,MPI_SP,0,0,MPI_COMM_WORLD,ier)
      endif
    enddo

    if(myid.eq.0) then
      open(5,file=TRIM(file))
      do k = 1,Kglob
      do j = 1,Nglob
        write(5,100) (phiglob(i,j,k),i=1,Mglob)
      enddo
      enddo
      close(5)
    endif
100 FORMAT(15000E16.8)

    deallocate(philoc)
    deallocate(xx)

    end subroutine putfile3D


# else
  ! not parallel


     subroutine putfile2D(file,phi)

     use global
     implicit none
     real(SP),dimension(Mloc,Nloc),intent(in) :: phi
     character(len=80) :: file
     integer :: i,j

     open(5,file=trim(file))
# if defined(DEBUG)
     do j = 1,Nloc
       write(5,100) (phi(i,j),i=1,Mloc)
     enddo
# else
     do j = Jbeg,Jend
       write(5,100) (phi(i,j),i=Ibeg,Iend)
     enddo
# endif 
! end debug
     close(5)
 100 FORMAT(15000f15.5)

     end subroutine putfile2D


     subroutine putfile3D(file,phi)
     use global
     implicit none
     real(SP),dimension(Mloc,Nloc,Kloc),intent(in) :: phi
     character(len=80) :: file
     integer :: i,j,k


     open(5,file=trim(file))
# if defined (DEBUG)
     do k = 1,Kloc
     do j = 1,Nloc
       write(5,100) (phi(i,j,k),i=1,Mloc)
     enddo
     enddo
# else
     do k = Kbeg,Kend
     do j = Jbeg,Jend
       write(5,100) (phi(i,j,k),i=Ibeg,Iend)
     enddo
     enddo
# endif
! end debug
     close(5)
 100 FORMAT(15000f15.5)


     end subroutine putfile3D

# endif
 ! end parallel

     subroutine read_bathymetry
!------------------------------------------------------
!    This subroutine is used to read bathymetry
!    Called by
!       main
!    Last update: 21/12/2010, Gangfeng Ma
!-----------------------------------------------------
     use global
     implicit none
     integer :: i,j,m,n,iter,iglob,jglob,II,JJ
     integer :: Maskp(Mglob+1,Nglob+1)
     real(SP), dimension(Mglob+1,Nglob+1) :: HG

     ! read bathymetry at grid points
     if(trim(adjustl(DEPTH_TYPE))=='CELL_GRID') then
       if(ANA_BATHY) then
         do j = 1,Nglob+1
         do i = 1,Mglob+1
           HG(i,j) = 0.5
         enddo
         enddo
       else
! fyshi add standard depth file
         open(5,file=TRIM(Depth_File),status='old')
         do j = 1,Nglob+1
           read(5,*) (HG(i,j),i=1,Mglob+1)
         enddo
       endif

       ! find pernament dry points
       Maskp = 1
       do j = 1,Nglob+1
       do i = 1,Mglob+1
         if(HG(i,j)<-1000.0) Maskp(i,j) = 0
       enddo
       enddo
 
       ! interpolate depth into cell center
       do j = 1,Nglob
       do i = 1,Mglob
         HCG(i,j) = (HG(i,j)*Maskp(i,j)+HG(i+1,j)*Maskp(i+1,j)+  &
             HG(i,j+1)*Maskp(i,j+1)+HG(i+1,j+1)*Maskp(i+1,j+1))/  &
             (Maskp(i,j)+Maskp(i+1,j)+Maskp(i,j+1)+Maskp(i+1,j+1)+1.e-16)
       enddo
       enddo

# if defined (PARALLEL)
       do j = Jbeg,Jend
       do i = Ibeg,Iend
         iglob = npx*(Mloc-2*Nghost)+i-Nghost
         jglob = npy*(Nloc-2*Nghost)+j-Nghost
         Hc(i,j) = HCG(iglob,jglob)
       enddo
       enddo
# else
       do j = Jbeg,Jend
       do i = Ibeg,Iend
         iglob = i-Nghost
         jglob = j-Nghost
         Hc(i,j) = HCG(iglob,jglob)
       enddo
       enddo
# endif

     elseif(trim(adjustl(DEPTH_TYPE))=='CELL_CENTER') then
       ! read bathymetry at cell center
       if(ANA_BATHY) then

       else ! not analytical bathymetry, read from depth file

         open(5,file=TRIM(Depth_File),status='old')
         do j = 1,Nglob
           read(5,*) (HCG(i,j),i=1,Mglob)
         enddo
         close(5)

# if defined (PARALLEL)
         do j = Jbeg,Jend
         do i = Ibeg,Iend
           iglob = npx*(Mloc-2*Nghost)+i-Nghost
           jglob = npy*(Nloc-2*Nghost)+j-Nghost
           Hc(i,j) = HCG(iglob,jglob)
         enddo
         enddo
# else
         do j = Jbeg,Jend
         do i = Ibeg,Iend
           iglob = i-Nghost
           jglob = j-Nghost
           Hc(i,j) = HCG(iglob,jglob)
         enddo
         enddo
# endif

       endif

     endif

     ! find pernament dry cells
     Mask_Struct = 1
     do j = Jbeg,Jend
     do i = Ibeg,Iend
       if(Hc(i,j)<-10.0) then
         Mask_Struct(i,j) = 0
       endif
     enddo
     enddo

     ! collect data into ghost cells
     call phi_2D_coll(Hc)

     ! reconstruct depth at x-y faces
     do j = 1,Nloc
     do i = 2,Mloc
       Hfx(i,j) = 0.5*(Hc(i-1,j)+Hc(i,j))
     enddo
     Hfx(1,j) = Hc(1,j)
     Hfx(Mloc1,j) = Hc(Mloc,j)
     enddo

     do i = 1,Mloc
     do j = 2,Nloc
       Hfy(i,j) = 0.5*(Hc(i,j-1)+Hc(i,j))
     enddo
     Hfy(i,1) = Hc(i,1)
     Hfy(i,Nloc1) = Hc(i,Nloc)
     enddo

     ! derivatives of water depth at cell center
     do j = 1,Nloc
     do i = 1,Mloc
       DelxH(i,j) = (Hfx(i+1,j)-Hfx(i,j))/dx
       DelyH(i,j) = (Hfy(i,j+1)-Hfy(i,j))/dy
     enddo
     enddo

     ! save initial bathymetry
     Hc0 = Hc


     end subroutine read_bathymetry

  


# if defined (COUPLING)
SUBROUTINE READ_NESTING_FILE
!---------------------------------------------------
!    This subroutine is used to read nesting file at the first time
!    Called by 
!       main
!    Last update: 05/15/2013, fyshi
!---------------------------------------------------
     use global
     implicit none
     INTEGER :: I,J,K
       OPEN(11,FILE=TRIM(COUPLING_FILE))
         READ(11,*)  ! title
         READ(11,*)  ! boundary info
! boundary basic info including point number of coupling, start point, etc
! east
         READ(11,*)  ! east
         READ(11,*) N_COUPLING_EAST,J_START_EAST
! west 
         READ(11,*)  ! west
         READ(11,*) N_COUPLING_WEST,J_START_WEST
! south 
         READ(11,*)  ! south
         READ(11,*) N_COUPLING_SOUTH,I_START_SOUTH
! north 
         READ(11,*)  ! north
         READ(11,*) N_COUPLING_NORTH,I_START_NORTH

! read time and variable at the first level

         READ(11,*) ! time start title
         READ(11,*) TIME_COUPLING_1 
! initialize time_2
         TIME_COUPLING_2 = TIME_COUPLING_1

! east
         IF(N_COUPLING_EAST.GT.0)THEN
           ALLOCATE(U_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               V_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               W_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               Z_COUPLING_EAST(N_COUPLING_EAST,2), &
               P_COUPLING_EAST(N_COUPLING_EAST,Kglob,2), &
               S_COUPLING_EAST(N_COUPLING_EAST,Kglob,2), &
               T_COUPLING_EAST(N_COUPLING_EAST,Kglob,2) &
               )
             READ(11,*)   ! east
             READ(11,119)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119)((U_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((V_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((W_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((P_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((S_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((T_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
!   initialize first step
             U_COUPLING_EAST(:,:,1)=U_COUPLING_EAST(:,:,2)
             V_COUPLING_EAST(:,:,1)=V_COUPLING_EAST(:,:,2)
             W_COUPLING_EAST(:,:,1)=W_COUPLING_EAST(:,:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)
             P_COUPLING_EAST(:,:,1)=P_COUPLING_EAST(:,:,2)
             S_COUPLING_EAST(:,:,1)=S_COUPLING_EAST(:,:,2)
             T_COUPLING_EAST(:,:,1)=T_COUPLING_EAST(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_east
119      FORMAT(5E16.6)

! west
         IF(N_COUPLING_WEST.GT.0)THEN
           ALLOCATE(U_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               V_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               W_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               Z_COUPLING_WEST(N_COUPLING_WEST,2),&
               P_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               S_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               T_COUPLING_WEST(N_COUPLING_WEST,Kglob,2) &
               )
             READ(11,*)   ! west
             READ(11,119)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119)((U_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((V_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((W_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((P_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((S_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((T_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
!   initialize first step
             U_COUPLING_WEST(:,:,1)=U_COUPLING_WEST(:,:,2)
             V_COUPLING_WEST(:,:,1)=V_COUPLING_WEST(:,:,2)
             W_COUPLING_WEST(:,:,1)=W_COUPLING_WEST(:,:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)
             P_COUPLING_WEST(:,:,1)=P_COUPLING_WEST(:,:,2)
             S_COUPLING_WEST(:,:,1)=S_COUPLING_WEST(:,:,2)
             T_COUPLING_WEST(:,:,1)=T_COUPLING_WEST(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_west
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
           ALLOCATE(U_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               V_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               W_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               Z_COUPLING_SOUTH(N_COUPLING_SOUTH,2),&
               P_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               S_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               T_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2) &
               )
             READ(11,*)   ! south
             READ(11,119)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119)((U_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((V_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((W_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((P_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((S_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((T_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
!   initialize first step
             U_COUPLING_SOUTH(:,:,1)=U_COUPLING_SOUTH(:,:,2)
             V_COUPLING_SOUTH(:,:,1)=V_COUPLING_SOUTH(:,:,2)
             W_COUPLING_SOUTH(:,:,1)=W_COUPLING_SOUTH(:,:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)
             P_COUPLING_SOUTH(:,:,1)=P_COUPLING_SOUTH(:,:,2)
             S_COUPLING_SOUTH(:,:,1)=S_COUPLING_SOUTH(:,:,2)
             T_COUPLING_SOUTH(:,:,1)=T_COUPLING_SOUTH(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_south
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
           ALLOCATE(U_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               V_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               W_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               Z_COUPLING_NORTH(N_COUPLING_NORTH,2), &
               P_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2), &
               S_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2), &
               T_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2) &
               )
             READ(11,*)   ! north
             READ(11,119)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119)((U_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((V_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((W_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((P_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((S_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((T_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)

!   initialize first step
             U_COUPLING_NORTH(:,:,1)=U_COUPLING_NORTH(:,:,2)
             V_COUPLING_NORTH(:,:,1)=V_COUPLING_NORTH(:,:,2)
             W_COUPLING_NORTH(:,:,1)=W_COUPLING_NORTH(:,:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)
             P_COUPLING_NORTH(:,:,1)=P_COUPLING_NORTH(:,:,2)
             S_COUPLING_NORTH(:,:,1)=S_COUPLING_NORTH(:,:,2)
             T_COUPLING_NORTH(:,:,1)=T_COUPLING_NORTH(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_north


! specify boundary start points

! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
      Kstart_WEST=J_START_WEST+Nghost -npy*Nglob/py
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1 -npy*Nglob/py
      IF((Kstart_WEST>Nghost.AND.Kstart_WEST<Nloc-Nghost+1).OR.&
     (Kend_WEST<Nloc-Nghost+1.AND.Kend_WEST>Nghost))THEN
       IF(Kstart_WEST<Nghost+1)THEN
         Kshift_WEST= -Kstart_WEST+Nghost+1
         Kstart_WEST=Nghost+1
       ELSE
         Kshift_WEST=-(Kstart_WEST-Nghost)+1
       ENDIF
       IF(Kend_WEST>Nloc-Nghost)THEN
         Kend_WEST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_WEST=.TRUE.
      ELSE
       IF(Kstart_WEST<=Nghost.AND.Kend_WEST>=Nloc-Nghost+1)THEN
         Kshift_WEST=-Kstart_WEST+Nghost+1
         Kstart_WEST = Nghost+1
         Kend_WEST = Nloc-Nghost
         IN_DOMAIN_WEST=.TRUE.
       ELSE
         IN_DOMAIN_WEST=.FALSE.
       ENDIF
      ENDIF
! check print*,myid,Kshift_WEST,Kstart_WEST,Kend_WEST
     endif
# else
      Kstart_WEST=J_START_WEST+Nghost
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1
      Kshift_WEST = -(Kstart_WEST-Nghost)+1
      IN_DOMAIN_WEST = .TRUE.
# endif

   ENDIF

! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
      Kstart_EAST=J_START_EAST+Nghost -npy*Nglob/py
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1 -npy*Nglob/py
      IF((Kstart_EAST>Nghost.AND.Kstart_EAST<Nloc-Nghost+1).OR.&
     (Kend_EAST<Nloc-Nghost+1.AND.Kend_EAST>Nghost))THEN
       IF(Kstart_EAST<Nghost+1)THEN
         Kshift_EAST= -Kstart_EAST+Nghost+1
         Kstart_EAST=Nghost+1
       ELSE
         Kshift_EAST=-(Kstart_EAST-Nghost)+1
       ENDIF
       IF(Kend_EAST>Nloc-Nghost)THEN
         Kend_EAST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_EAST=.TRUE.
      ELSE

       IF(Kstart_EAST<=Nghost.AND.Kend_EAST>=Nloc-Nghost+1)THEN
         Kshift_EAST=-Kstart_EAST+Nghost+1
         Kstart_EAST = Nghost+1
         Kend_EAST = Nloc-Nghost
         IN_DOMAIN_EAST=.TRUE.
       ELSE
         IN_DOMAIN_EAST=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_EAST=J_START_EAST+Nghost
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1
      Kshift_EAST = -(Kstart_EAST-Nghost)+1
      IN_DOMAIN_EAST = .TRUE.
# endif
    ENDIF

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
      Kstart_SOUTH=I_START_SOUTH+Nghost -npx*Mglob/px
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1 -npx*Mglob/px
      IF((Kstart_SOUTH>Nghost.AND.Kstart_SOUTH<Mloc-Nghost+1).OR.&
     (Kend_SOUTH<Mloc-Nghost+1.AND.Kend_SOUTH>Nghost))THEN
       IF(Kstart_SOUTH<Nghost+1)THEN
         Kshift_SOUTH= -Kstart_SOUTH+Nghost+1
         Kstart_SOUTH=Nghost+1
       ELSE
         Kshift_SOUTH=-(Kstart_SOUTH-Nghost)+1
       ENDIF
       IF(Kend_SOUTH>Mloc-Nghost)THEN
         Kend_SOUTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_SOUTH=.TRUE.
      ELSE

       IF(Kstart_SOUTH<=Nghost.AND.Kend_SOUTH>=Mloc-Nghost+1)THEN
         Kshift_SOUTH=-Kstart_SOUTH+Nghost+1
         Kstart_SOUTH = Nghost+1
         Kend_SOUTH = Mloc-Nghost
         IN_DOMAIN_SOUTH=.TRUE.
       ELSE
         IN_DOMAIN_SOUTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SOUTH=I_START_SOUTH+Nghost
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1
      Kshift_SOUTH = -(Kstart_SOUTH-Nghost)+1
      IN_DOMAIN_SOUTH = .TRUE.
# endif
   ENDIF

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
      Kstart_NORTH=I_START_NORTH+Nghost -npx*Mglob/px
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1 -npx*Mglob/px
      IF((Kstart_NORTH>Nghost.AND.Kstart_NORTH<Mloc-Nghost+1).OR.&
     (Kend_NORTH<Mloc-Nghost+1.AND.Kend_NORTH>Nghost))THEN
       IF(Kstart_NORTH<Nghost+1)THEN
         Kshift_NORTH= -Kstart_NORTH+Nghost+1
         Kstart_NORTH=Nghost+1
       ELSE
         Kshift_NORTH=-(Kstart_NORTH-Nghost)+1
       ENDIF
       IF(Kend_NORTH>Mloc-Nghost)THEN
         Kend_NORTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_NORTH=.TRUE.
      ELSE

       IF(Kstart_NORTH<=Nghost.AND.Kend_NORTH>=Mloc-Nghost+1)THEN
         Kshift_NORTH=-Kstart_NORTH+Nghost+1
         Kstart_NORTH = Nghost+1
         Kend_NORTH = Mloc-Nghost
         IN_DOMAIN_NORTH=.TRUE.
       ELSE
         IN_DOMAIN_NORTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_NORTH=I_START_NORTH+Nghost
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1
      Kshift_NORTH = -(Kstart_NORTH-Nghost)+1
      IN_DOMAIN_NORTH = .TRUE.
# endif
   ENDIF

END SUBROUTINE READ_NESTING_FILE
# endif




