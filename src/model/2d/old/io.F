!-----------------------------------------------------------------------------------
!      FILE io.F
!
!      This file is part of the FUNWAVE-TVD program under the Simplified BSD license
!
!-------------------------------------------------------------------------------------
!
!    Copyright (c) 2016, FUNWAVE Development Team
!
!    (See http://www.udel.edu/kirby/programs/funwave/funwave.html
!     for Development Team membership)
!
!    All rights reserved.
!
!    FUNWAVE_TVD is free software: you can redistribute it and/or modify
!    it under the terms of the Simplified BSD License as released by
!    the Berkeley Software Distribution (BSD).
!
!    Redistribution and use in source and binary forms, with or without
!    modification, are permitted provided that the following conditions are met:
!
!    1. Redistributions of source code must retain the above copyright notice, this
!       list of conditions and the following disclaimer.
!    2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!    The views and conclusions contained in the software and documentation are those
!    of the authors and should not be interpreted as representing official policies,
!    either expressed or implied, of the FreeBSD Project.
!
!-------------------------------------------------------------------------------------
!
!    OUTPUT is subroutine for screen, station, and field print-out
!
!    HISTORY:
!      12/06/2017  Michael-Angelo Y.-H. Lam
!      01/10/2011  Fengyan SHi
!-------------------------------------------------------------------------------------
SUBROUTINE OUTPUT
   USE GLOBAL
# if defined(VESSEL)
   USE VESSEL_MODULE
# endif
# if defined(METEO)
   USE METEO_MODULE
# endif
# if defined(SEDIMENT)
   USE SEDIMENT_MODULE
# endif
# if defined (TRACKING)
   ! ONLY guard: the module re-exports II/JJ and its GLOBAL imports
   USE TRACER, ONLY: PLOT_INTV_TRACKING, PLOT_COUNT_TRACKING, OUTPUT_TRACKING
# endif
# if defined(FOAM)
   USE MOD_FOAM
# endif

   IMPLICIT NONE

   SCREEN_COUNT = SCREEN_COUNT + DT

   IF (SCREEN_COUNT >= SCREEN_INTV) THEN
      SCREEN_COUNT = SCREEN_COUNT - SCREEN_INTV
      CALL STATISTICS
   END IF

! stations
   IF (NumberStations > 0) THEN
      PLOT_COUNT_STATION = PLOT_COUNT_STATION + DT
      IF (PLOT_COUNT_STATION >= PLOT_INTV_STATION) THEN
         PLOT_COUNT_STATION = PLOT_COUNT_STATION - PLOT_INTV_STATION
# if defined (SPHERICAL_IJ_STATION)
         ! use IJ for coupling purpose
         CALL STATIONS_SPHERICAL_IJ
# else
         CALL STATIONS
# endif
      END IF
   END IF
! preview

   IF (TIME >= PLOT_START_TIME) THEN

      PLOT_COUNT = PLOT_COUNT + DT
      IF (PLOT_COUNT >= PLOT_INTV) THEN
         PLOT_COUNT = PLOT_COUNT - PLOT_INTV
         CALL PREVIEW
      END IF
# if defined (VESSEL)
      PLOT_COUNT_VESSEL = PLOT_COUNT_VESSEL + DT
      IF (PLOT_COUNT_VESSEL >= PLOT_INTV_VESSEL) THEN
         PLOT_COUNT_VESSEL = PLOT_COUNT_VESSEL - PLOT_INTV_VESSEL
         CALL OUTPUT_VESSEL
      END IF
# endif
# if defined (SEDIMENT)
      PLOT_COUNT_SEDIMENT = PLOT_COUNT_SEDIMENT + DT
      IF (PLOT_COUNT_SEDIMENT >= PLOT_INTV_SEDIMENT) THEN
         PLOT_COUNT_SEDIMENT = PLOT_COUNT_SEDIMENT - PLOT_INTV_SEDIMENT
         CALL OUTPUT_SEDIMENT
      END IF
# endif
# if defined (TRACKING)
      PLOT_COUNT_TRACKING = PLOT_COUNT_TRACKING + DT
      IF (PLOT_COUNT_TRACKING >= PLOT_INTV_TRACKING) THEN
         PLOT_COUNT_TRACKING = PLOT_COUNT_TRACKING - PLOT_INTV_TRACKING
         CALL OUTPUT_TRACKING
      END IF
# endif
# if defined (FOAM)
      PLOT_COUNT_FOAM = PLOT_COUNT_FOAM + DT
      IF (PLOT_COUNT_FOAM >= PLOT_INTV_FOAM) THEN
         PLOT_COUNT_FOAM = PLOT_COUNT_FOAM - PLOT_INTV_FOAM
         CALL OUTPUT_FOAM
      END IF
# endif

   END IF ! end plot start time

END SUBROUTINE OUTPUT

!-------------------------------------------------------------------------------------
!
!    READ_INPUT is subroutine to read from input.txt
!
!  HISTORY:
!  01/10/2011  Fengyan SHi
!  12/23/2014  Young-Kwang Choi, added option for intel compiler
!  07/17/2019  Zhouteng Ye, added 1. get input from command line argument
!                                 2. input file for post-processor
!
!
!-------------------------------------------------------------------------------------

SUBROUTINE READ_INPUT
   USE GLOBAL
   USE INPUT_READ
   USE TIDE_MODULE
# if defined (PRECIPITATION)
   ! ONLY guard: the module's FILE_NAME collides with the local one
   USE PRECIPITATION_MODULE, ONLY: RainWaveInteraction, &
                                   OUT_Precipitation, RAINFALL_FILE
# endif
# if defined (SUBGRID)
   ! ONLY guard: the module's LINE/FDIR collide with the local ones
   USE SUBGRID_MODULE, ONLY: SubMainGridRatio, OUT_POROSITY, &
                             DEPTH_SUBGRID_FILE
# endif
# if defined (TRACKING)
   ! ONLY guard: the module re-exports II/JJ and its GLOBAL imports
   USE TRACER, ONLY: TRACER_FILE
# endif
# if defined (FOAM)
   ! ONLY guard: the module declares its own SMALL, colliding with GLOBAL's
   USE MOD_FOAM, ONLY: PLOT_INTV_FOAM, f_source, FoamTimeScale, &
                       BurstTimeNonBreaking, MinThick, CdFoam
# endif
!    USE TIME_SPECTRA_MODULE
![jychoi added this for intel compiler 14.12.23
# if defined(INTEL)
   USE IFPORT
# endif
   use core_yaml_file_mod, only: type_yaml_reader
   use core_log_io_mod, only: type_log_writer
   use core_constants_mod, only: LABEL_SIZE
   use core_misc_mod, only: random2
   use model_main_mod, only: type_model_main
!jychoi 14.12.23]
   IMPLICIT NONE
   CHARACTER(LEN=80) FILE_NAME
   CHARACTER(LEN=80) MKFOLDER
   INTEGER::LINE
   INTEGER :: ierr
   INTEGER :: I_comp
   LOGICAL :: INPUT_PHASE = .FALSE.

   character(256) :: my_path
   character(2048) :: error_msg, log_path
   character(:), allocatable :: yaml_path
   !>by Zhouteng Ye
   CHARACTER(LEN=80)::INPUT_NAME = ''

![ykchoi
   CHARACTER(LEN=80)::FDIR = ' '
!ykchoi]

   ! NOTE: --- MY VARIABLES -------
   type(type_model_main) :: model
   type(type_yaml_reader), target :: yaml_reader
   type(type_log_writer), target :: log
   character(LABEL_SIZE) :: log_label = 'config'
   integer(MPI_ADDRESS_KIND):: address
   ! NOTE: --- MY VARIABLES -------

   ! NOTE: --- BELOW TO BE REMOVED -------

   FDIR = TRIM(RESULT_FOLDER)
   OPEN (10000, FILE='time_dt.out', STATUS='UNKNOWN')
   OPEN (3, FILE='LOG.txt')
   ! CALL getcwd(error_msg)

   !>by Zhouteng Ye
   !> Get the argument from the command.
   !> If no input in command, file name is 'input.txt' (same as before)
   !> If the input comes with other name, read the correponding file
   CALL GETARG(1, INPUT_NAME)
   if (INPUT_NAME .eq. '') Then
      FILE_NAME = 'input.txt'
   Else
      FILE_NAME = INPUT_NAME
   end if
   INPUT_FILE_NAME = FILE_NAME

! title
   !CALL READ_STRING(TITLE, FILE_NAME, 'TITLE', ierr)
   IF (ierr == 1) THEN
      !write(*,*) 'No TITLE in ', FILE_NAME, 'use default'
      TITLE = '---TEST RUN---'
   END IF
   call model%init()

# if defined (PARALLEL)
   myid   = model%env%comm%rank_id
   nprocs = model%env%comm%size
# endif

   PX = model%geometry%nx_proc
   PY = model%geometry%ny_proc

   select case (trim(model%geometry%bathy_type))
   case ('flat', 'slope')
      Mglob = model%geometry%grid_nx
      Nglob = model%geometry%grid_ny
   case ('file')
      Mglob = model%geometry%bathy_nx
      Nglob = model%geometry%bathy_ny
   end select

#if defined (CARTESIAN)
   dx = model%geometry%dx
   dy = model%geometry%dy
#else
   ! variable spacing: dx_file/dy_file applied later in init
#endif

   select case (trim(model%geometry%bathy_type))
   case ('slope'); DEPTH_TYPE = 'SLOPE'
   case ('flat');  DEPTH_TYPE = 'FLAT'
   case ('file');  DEPTH_TYPE = 'DATA'
   case default;   DEPTH_TYPE = trim(model%geometry%bathy_type)
   end select
   DEPTH_FILE       = model%geometry%bathy_file%root
   Depth_Flat       = model%geometry%bathy_depth
   SLP              = model%geometry%bathy_slope
   Xslp             = model%geometry%bathy_slope_x0
   BATHY_CORRECTION = model%geometry%bathy_correction

   TOTAL_TIME      = model%simulation%total_time
   PLOT_START_TIME = model%simulation%t_start
   PLOT_INTV       = model%simulation%plot_intv
   SCREEN_INTV     = model%simulation%screen_interval
   PLOT_INTV_STATION   = model%simulation%plot_intv_station
   StationOutputBuffer = model%simulation%station_output_buffer

   INI_UVZ        = model%hot_start%is_activated
   BED_DEFORMATION = model%hot_start%bed_deformation
   ETA_FILE       = model%hot_start%eta_file%root
   NO_UV_FILE     = model%hot_start%no_uv_file
   if (.not. NO_UV_FILE) then
      U_FILE = model%hot_start%u_file%root
      V_FILE = model%hot_start%v_file%root
   end if
   NO_MASK_FILE   = model%hot_start%no_mask_file
   if (.not. NO_MASK_FILE) MASK_FILE = model%hot_start%mask_file%root
   HotStartTime   = model%hot_start%time

   IF (INI_UVZ) THEN
      icount = model%hot_start%output_start_number - 1
      IF (BED_DEFORMATION) THEN
      END IF
   END IF  ! end initial hot start

   WaterLevel = model%physics%water_level

   WaveMaker = model%wavemaker%wavemaker_type

   IF (WaveMaker(1:7) == 'LEF_SOL') THEN
      AMP_SOLI = model%wavemaker%AMP_SOLI
      DEP_SOLI = model%wavemaker%DEP_SOLI
      LAG_SOLI = model%wavemaker%LAG_SOLI
   END IF

   IF (WaveMaker(1:7) == 'WK_TIME') THEN
      NumWaveComp  = model%wavemaker%NumWaveComp
      PeakPeriod   = model%wavemaker%PeakPeriod
      WaveCompFile = model%wavemaker%WaveCompFile
      Xc_WK        = model%wavemaker%Xc_WK
      Yc_WK        = model%wavemaker%Yc_WK
      DEP_WK       = model%wavemaker%DEP_WK
      Time_ramp    = model%wavemaker%Time_ramp
      Delta_WK     = model%wavemaker%Delta_WK
      Ywidth_WK    = model%wavemaker%Ywidth_WK

   END IF  ! end WK_TIME

   IF (WaveMaker(1:7) == 'INI_SOL') THEN

      SolitaryPositiveDirection = model%wavemaker%SolitaryPositiveDirection

      IF (SolitaryPositiveDirection) THEN
      ELSE
      END IF

      AMP_SOLI   = model%wavemaker%AMP_SOLI
      DEP_SOLI   = model%wavemaker%DEP_SOLI
      XWAVEMAKER = model%wavemaker%XWAVEMAKER

   END IF  ! end initial solitary

   IF (WaveMaker(1:6) == 'N_WAVE') THEN
      x1_Nwave    = model%wavemaker%x1_Nwave
      x2_Nwave    = model%wavemaker%x2_Nwave
      a0_Nwave    = model%wavemaker%a0_Nwave
      gamma_Nwave = model%wavemaker%gamma_Nwave
      dep_Nwave   = model%wavemaker%dep_Nwave

   END IF  ! end N_wave

   IF (WaveMaker(1:7) == 'INI_REC') THEN
      AMP_SOLI = model%wavemaker%AMP_SOLI
      Xc       = model%wavemaker%Xc
      Yc       = model%wavemaker%Yc
      WID      = model%wavemaker%WID

   END IF ! endif rectangular hump

   IF (WaveMaker(1:7) == 'INI_GAU' .OR. &
       WaveMaker(1:7) == 'INI_DIP') THEN

      AMP_SOLI = model%wavemaker%AMP_SOLI
      Xc       = model%wavemaker%Xc
      Yc       = model%wavemaker%Yc
      WID      = model%wavemaker%WID

   END IF ! endif gaussian hump

   IF (WaveMaker(1:6) == 'WK_REG') THEN
      Xc_WK     = model%wavemaker%Xc_WK
      Yc_WK     = model%wavemaker%Yc_WK
      Tperiod   = model%wavemaker%Tperiod
      AMP_WK    = model%wavemaker%AMP_WK
      DEP_WK    = model%wavemaker%DEP_WK
      Theta_WK  = model%wavemaker%Theta_WK
      Time_ramp = model%wavemaker%Time_ramp
      Delta_WK  = model%wavemaker%Delta_WK
      Ywidth_WK = model%wavemaker%Ywidth_WK

   END IF  ! endif WK_REG

   IF (WaveMaker(1:6) == 'WK_IRR' .OR. WaveMaker(1:6) == 'TMA_1D' &
       .OR. WaveMaker(1:6) == 'JON_1D' .OR. WaveMaker(1:6) == 'JON_2D') THEN

      Xc_WK     = model%wavemaker%Xc_WK
      Yc_WK     = model%wavemaker%Yc_WK
      DEP_WK    = model%wavemaker%DEP_WK
      Time_ramp = model%wavemaker%Time_ramp
      Delta_WK  = model%wavemaker%Delta_WK
      FreqPeak  = model%wavemaker%FreqPeak
      FreqMin   = model%wavemaker%FreqMin
      FreqMax   = model%wavemaker%FreqMax
      Hmo       = model%wavemaker%Hmo
      GammaTMA  = model%wavemaker%GammaTMA
      Nfreq     = model%wavemaker%Nfreq
      Ntheta    = model%wavemaker%Ntheta
      ThetaPeak = model%wavemaker%ThetaPeak
      Sigma_Theta = model%wavemaker%Sigma_Theta
      Ywidth_WK = model%wavemaker%Ywidth_WK

   END IF ! endif wk_irr

   ETA_LIMITER = model%wavemaker%ETA_LIMITER
   IF (ETA_LIMITER) THEN
      CrestLimit  = model%wavemaker%CrestLimit
      TroughLimit = model%wavemaker%TroughLimit
   END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

   IF (WaveMaker(1:13) == 'WK_NEW_DATA2D' .OR. WaveMaker(1:9) == 'WK_DATA2D') THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Xc_WK     = model%wavemaker%Xc_WK
      Yc_WK     = model%wavemaker%Yc_WK
      DEP_WK    = model%wavemaker%DEP_WK
      Time_ramp = model%wavemaker%Time_ramp
      Delta_WK  = model%wavemaker%Delta_WK

      WaveCompFile = model%wavemaker%WaveCompFile

! data reading will be in wavemaker.F

      Ywidth_WK = model%wavemaker%Ywidth_WK

   END IF ! end ierr

   EqualEnergy = model%wavemaker%EqualEnergy

! absorbing generating wavemaker, more tests needed
   IF (WaveMaker(1:3) == 'ABS' .OR. WaveMaker(1:11) == 'LEFT_BC_IRR') THEN

      WAVE_DATA_TYPE     = model%wavemaker%WAVE_DATA_TYPE
      DEP_Ser            = model%wavemaker%DepthWaveMaker

      IF (WaveMaker(1:3) == 'ABS') THEN
         WidthWaveMaker     = model%wavemaker%WidthWaveMaker
!  get R and A for sponge_wavemaker calculation
         R_sponge_wavemaker = model%wavemaker%R_sponge_wavemaker
         A_sponge_wavemaker = model%wavemaker%A_sponge_wavemaker
      END IF

      IF (WAVE_DATA_TYPE(1:4) == 'DATA') THEN
         ! missed in the io.F refactor: READ_STRING scanned the YAML
         ! file for the legacy key=value form (same as the DATA2D fix)
         WaveCompFile = model%wavemaker%WaveCompFile

         OPEN (1, FILE=TRIM(WaveCompFile))
         READ (1, *) NumFreq, NumDir
         ALLOCATE (Amp_Ser(NumFreq, NumDir), &
                   Per_Ser(NumFreq), Theta_Ser(NumDir), Phase_LEFT(NumFreq, NumDir))
         READ (1, *) PeakPeriod  ! useless for this application but should keep for consistency
         DO J = 1, NumFreq
            READ (1, *) Per_Ser(J)  ! read in as frequency
!print*,J,Per_Ser(J)
         END DO
         DO I = 1, NumDir
            READ (1, *) Theta_Ser(I)
         END DO
         DO I = 1, NumDir
            READ (1, *) (Amp_Ser(J, I), J=1, NumFreq)
!print*,I,Amp_Ser(J,I)
         END DO
         DO I = 1, NumDir
            READ (1, *, END=991) (Phase_LEFT(J, I), J=1, NumFreq)
         END DO
         CLOSE (1)
881      INPUT_PHASE = .TRUE.
991      CONTINUE

         DO J = 1, NumFreq
            DO I = 1, NumDir

               IF (INPUT_PHASE) THEN
                  Phase_LEFT(J, I) = Phase_LEFT(J, I)*3.1415926/180.0_SP
# if defined (ZERO_PHASE)
               ELSE
                  Phase_LEFT(J, I) = 0.0_SP
# else
               ELSE
                  Phase_LEFT(J, I) = random2()*2.0_SP*3.1415926
# endif
               END IF

            END DO
         END DO

! to make consistent with cm and sm approach we use phase_ser which is
! only is random with frequency
         ALLOCATE (Phase_Ser(NumFreq))

         ALLOCATE (Segma_Ser(NumFreq), Wave_Number_Ser(NumFreq))
         DO J = 1, NumFreq
            Phase_Ser(J) = Phase_LEFT(J, 1)
            IF (Per_Ser(J) .EQ. ZERO) THEN
               STOP
            ELSE
               Per_Ser(J) = 1.0_SP/Per_Ser(J)
            END IF
         END DO
         DO I = 1, NumDir
            Theta_Ser(I) = Theta_Ser(I)*DEG2RAD
         END DO

      ELSE ! use TMA or JON

         FreqPeak    = model%wavemaker%FreqPeak
         FreqMin     = model%wavemaker%FreqMin
         FreqMax     = model%wavemaker%FreqMax
         Hmo         = model%wavemaker%Hmo
         GammaTMA    = model%wavemaker%GammaTMA
         Nfreq       = model%wavemaker%Nfreq
         Ntheta      = model%wavemaker%Ntheta
         ThetaPeak   = model%wavemaker%ThetaPeak
         Sigma_Theta = model%wavemaker%Sigma_Theta

      END IF ! end if data or tma
   END IF ! end absorbing-generating or left bc wave maker

# if defined (CARTESIAN)
! south-north periodic boundary condition
   PERIODIC = model%physics%periodic
# endif

   DIFFUSION_SPONGE   = model%sponge%diffusion_sponge
   DIRECT_SPONGE      = model%sponge%direct_sponge
   FRICTION_SPONGE    = model%sponge%friction_sponge

   IF (DIRECT_SPONGE) THEN
   END IF

   IF (DIFFUSION_SPONGE) THEN
   END IF ! end diffusion_sponge

   IF (FRICTION_SPONGE) THEN
   END IF  ! endif friction_sponge

   Csp                = model%sponge%Csp
   CDsponge           = model%sponge%CDsponge
   Sponge_west_width  = model%sponge%Sponge_west_width
   Sponge_east_width  = model%sponge%Sponge_east_width
   Sponge_south_width = model%sponge%Sponge_south_width
   Sponge_north_width = model%sponge%Sponge_north_width
   R_sponge           = model%sponge%R_sponge
   A_sponge           = model%sponge%A_sponge

! to avoid longshore current caused by extra momentum flux
! we can add bottom friction make momentum balance

   WaveMakerCurrentBalance = model%wavemaker%WaveMakerCurrentBalance
   WaveMakerCd             = model%wavemaker%WaveMakerCd
   IF (WaveMakerCurrentBalance) THEN
   ELSE
   END IF

! obstacle structures
   OBSTACLE      = model%obstacle%obstacle
   BREAKWATER    = model%obstacle%breakwater
   BreakWaterAbsorbCoef = model%obstacle%BreakWaterAbsorbCoef

   IF (OBSTACLE) THEN
      OBSTACLE_FILE = model%obstacle%obstacle_file%root
   ELSE
   END IF

! breakwater
   IF (BREAKWATER) THEN
      BREAKWATER_FILE = model%obstacle%breakwater_file%root
   ELSE
   END IF

! physics
   DISPERSION         = model%physics%dispersion
   Gamma1             = model%physics%Gamma1
# if defined (CARTESIAN)
   Gamma2             = model%physics%Gamma2
   Beta_ref           = model%physics%Beta_ref
# endif
# if defined (ZALPHA)
   Beta_ref           = model%physics%Beta_ref
# endif
   Gamma3             = model%physics%Gamma3
   VISCOSITY_BREAKING = model%physics%viscosity_breaking
   SWE_ETA_DEP        = model%physics%SWE_ETA_DEP

   IF (VISCOSITY_BREAKING) THEN
   END IF

   IN_Cd    = model%friction%friction_matrix
   Cd_fixed = model%friction%Cd_fixed

   IF (IN_Cd) THEN
      IF (model%friction%no_cd_file) THEN
# if defined (PARALLEL)
         call MPI_FINALIZE(ier)
# endif
         STOP
      END IF
      CD_FILE = model%friction%cd_file%root
   END IF ! endif IN_Cd

! numerics schemes
   Time_Scheme = model%numerics%Time_Scheme
   CONSTR      = model%numerics%construction
   HIGH_ORDER  = model%numerics%high_order
   CFL         = model%numerics%CFL

   FIXED_DT = model%simulation%fixed_dt
   DT_fixed = model%simulation%dt_fixed
   IF (FIXED_DT) THEN
   END IF

   FroudeCap   = model%numerics%FroudeCap
   MinDepth    = model%numerics%MinDepth
   MinDepthFrc = model%numerics%MinDepthFrc

!  merge two parameters into the minimum one, change to min according to Harris 11/13/2023
   MinDepthFrc = MIN(MinDepthFrc, MinDepth)
   MinDepth    = MinDepthFrc

! Lauren - Arrival Time - Wave height threshold (in m) to pick up arrival time
   OUT_Time   = model%numerics%OUT_Time
   ArrTimeMin = model%numerics%ArrTimeMin

   IF (OUT_Time) THEN
   END IF
! end Laurens modification

! roller
   ROLLER           = model%breaking%roller
   SHOW_BREAKING    = model%breaking%show_breaking
   Cbrk1            = model%breaking%Cbrk1
   Cbrk2            = model%breaking%Cbrk2
   WAVEMAKER_Cbrk   = model%breaking%WAVEMAKER_Cbrk
   WAVEMAKER_VIS    = model%breaking%WAVEMAKER_VIS
   visbrk           = model%breaking%visbrk
   WAVEMAKER_visbrk = model%breaking%WAVEMAKER_visbrk

   IF (ROLLER) THEN
      ROLLER_SWITCH = 1.0_SP
   ELSE
      ROLLER_SWITCH = ZERO
   END IF

   IF (ROLLER) VISCOSITY_BREAKING = .TRUE.
   IF (VISCOSITY_BREAKING) SHOW_BREAKING = .TRUE.

   IF (VISCOSITY_BREAKING .AND. WAVEMAKER_VIS) THEN
# if defined (PARALLEL)
      call MPI_FINALIZE(ier)
# else
      STOP
# endif
   END IF

   IF (WAVEMAKER_VIS) THEN
   END IF

   T_INTV_mean = model%output%T_INTV_mean
   STEADY_TIME = model%output%STEADY_TIME
   C_smg       = model%physics%C_smg
   nu_bkg      = model%breaking%nu_bkg

# if defined (COUPLING)
   IF (.NOT. model%coupling%is_activated) THEN
# if defined (PARALLEL)
      call MPI_FINALIZE(ier)
# endif
      STOP
   END IF
   COUPLING_FILE = model%coupling%coupling_file%root
# endif
   ! end coupling file

! result folder
   RESULT_FOLDER = model%output%result_folder

![mayhl 17/06/12
! Note 1: Serial code only output is ASCII
! Note 2: Placing code after result folder creation seems to delay
!         writing to LOG.txt till after simulation is completed.
# if defined (PARALLEL)
   FIELD_IO_TYPE = model%output%field_io_type
# endif
! mayhl]

! create result folder
   MKFOLDER = "mkdir -p "//TRIM(RESULT_FOLDER)
# if defined (PARALLEL)
   IF (myid .eq. 0) THEN
# if defined (INTEL)
      RES = SYSTEM(TRIM(MKFOLDER))
# else
      CALL SYSTEM(TRIM(MKFOLDER))
# endif
   END IF
# else
# if defined(INTEL)
   RES = SYSTEM(TRIM(MKFOLDER))
# else
   CALL SYSTEM(TRIM(MKFOLDER))
# endif
# endif

! station files
   NumberStations = model%output%number_stations
   IF (NumberStations > 0) THEN
      STATIONS_FILE = model%output%stations_file
   END IF

! output parameters
   OUTPUT_RES = model%output%output_res

   OUT_DEPTH      = model%output%depth_out
   OUT_U          = model%output%OUT_U
   OUT_V          = model%output%OUT_V
   OUT_ETA        = model%output%OUT_ETA
# if defined(UseEtaScreen)
   OUT_EtaScreen  = model%output%OUT_EtaScreen
# endif
   OUT_Hmax       = model%output%OUT_Hmax
   OUT_Hmin       = model%output%OUT_Hmin
   OUT_Umax       = model%output%OUT_Umax
   OUT_MFmax      = model%output%OUT_MFmax
   OUT_VORmax     = model%output%OUT_VORmax
   OUT_MASK       = model%output%OUT_MASK
   OUT_MASK9      = model%output%OUT_MASK9
   OUT_Umean      = model%output%OUT_Umean
   OUT_Vmean      = model%output%OUT_Vmean
   OUT_ETAmean    = model%output%OUT_ETAmean
   OUT_WaveHeight = model%output%OUT_WaveHeight
   OUT_SXL        = model%output%OUT_SXL
   OUT_SXR        = model%output%OUT_SXR
   OUT_SYL        = model%output%OUT_SYL
   OUT_SYR        = model%output%OUT_SYR
   OUT_SourceX    = model%output%OUT_SourceX
   OUT_SourceY    = model%output%OUT_SourceY
   OUT_FrcX       = model%output%OUT_FrcX
   OUT_FrcY       = model%output%OUT_FrcY
   OUT_BrkdisX    = model%output%OUT_BrkdisX
   OUT_BrkdisY    = model%output%OUT_BrkdisY
   OUT_P          = model%output%OUT_P
   OUT_Q          = model%output%OUT_Q
   OUT_Fx         = model%output%OUT_Fx
   OUT_Fy         = model%output%OUT_Fy
   OUT_Gx         = model%output%OUT_Gx
   OUT_Gy         = model%output%OUT_Gy
   OUT_AGE        = model%output%OUT_AGE
   OUT_ROLLER     = model%output%OUT_ROLLER
   OUT_UNDERTOW   = model%output%OUT_UNDERTOW
   OUT_NU         = model%output%OUT_NU
   OUT_TMP        = model%output%OUT_TMP
   OUT_Radiation  = model%output%OUT_Radiation

   EtaBlowVal = model%output%EtaBlowVal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

   IF (WaveMaker(1:10) == 'WK_NEW_IRR') THEN
      Xc_WK       = model%wavemaker%Xc_WK
      Yc_WK       = model%wavemaker%Yc_WK
      DEP_WK      = model%wavemaker%DEP_WK
      Time_ramp   = model%wavemaker%Time_ramp
      Delta_WK    = model%wavemaker%Delta_WK
      FreqPeak    = model%wavemaker%FreqPeak
      FreqMin     = model%wavemaker%FreqMin
      FreqMax     = model%wavemaker%FreqMax
      Hmo         = model%wavemaker%Hmo
      GammaTMA    = model%wavemaker%GammaTMA
      Nfreq       = model%wavemaker%Nfreq
      Ntheta      = model%wavemaker%Ntheta
      ThetaPeak   = model%wavemaker%ThetaPeak
      Sigma_Theta = model%wavemaker%Sigma_Theta
      Ywidth_WK   = model%wavemaker%Ywidth_WK
      alpha_c     = model%wavemaker%alpha_c

   END IF ! IF(WaveMaker(1:10)=='WK_NEW_IRR')THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! tide bridge: TIDE_MODULE reads its keys itself in the original
   ! (TIDE_INITIAL READ_* against input.txt); against the YAML those
   ! fall through to defaults, so the module variables are seeded here
   ! from the modern reader and the READ calls in mod_tide.F are
   ! stubbed out
   TIDAL_BC_ABS     = model%tide%tidal_bc_abs
   TIDAL_BC_GEN_ABS = model%tide%tidal_bc_gen_abs
   TideBcType       = model%tide%tide_bc_type
   Iwidth_tide      = model%tide%iwidth
   TideWest  = model%tide%tide_west
   TideEast  = model%tide%tide_east
   TideSouth = model%tide%tide_south
   TideNorth = model%tide%tide_north
   TideWest_ETA  = model%tide%eta_west
   TideWest_U    = model%tide%u_west
   TideWest_V    = model%tide%v_west
   TideEast_ETA  = model%tide%eta_east
   TideEast_U    = model%tide%u_east
   TideEast_V    = model%tide%v_east
   TideSouth_ETA = model%tide%eta_south
   TideSouth_U   = model%tide%u_south
   TideSouth_V   = model%tide%v_south
   TideNorth_ETA = model%tide%eta_north
   TideNorth_U   = model%tide%u_north
   TideNorth_V   = model%tide%v_north
   ! type_path roots are allocatable — set only when configured
   IF (ALLOCATED(model%tide%file_west%root)) &
      TideWestFileName = model%tide%file_west%root
   IF (ALLOCATED(model%tide%file_east%root)) &
      TideEastFileName = model%tide%file_east%root
   IF (ALLOCATED(model%tide%file_south%root)) &
      TideSouthFileName = model%tide%file_south%root
   IF (ALLOCATED(model%tide%file_north%root)) &
      TideNorthFileName = model%tide%file_north%root

# if defined (PRECIPITATION)
   ! precipitation bridge: same pattern as the tide bridge above —
   ! PRECIPITATION_MODULE variables seeded from the modern reader, the
   ! READ calls in mod_precipitation.F stubbed out.  The modern reader
   ! already stops when RAINFALL_FILE is missing (legacy STOP)
   RainWaveInteraction = model%precipitation%rain_wave_interaction
   OUT_Precipitation   = model%precipitation%out_precipitation
   IF (ALLOCATED(model%precipitation%rainfall_file%root)) &
      RAINFALL_FILE = model%precipitation%rainfall_file%root
# endif

# if defined (SUBGRID)
   ! subgrid bridge: SUBGRID_MODULE variables seeded from the modern
   ! reader, the READ calls in mod_subgrid.F stubbed out.  NumPixel is
   ! still derived in SUBGRID_INITIAL.  DEPTH_FILE is left alone here —
   ! legacy re-reads it inside SUBGRID_INITIAL from the same key the
   ! geometry reader already consumed
   SubMainGridRatio = model%subgrid%ratio
   OUT_POROSITY     = model%subgrid%out_porosity
   IF (ALLOCATED(model%subgrid%depth_subgrid_file%root)) &
      DEPTH_SUBGRID_FILE = model%subgrid%depth_subgrid_file%root
# endif

# if defined (FOAM)
   ! foam bridge: MOD_FOAM variables seeded from the modern reader, the
   ! READ calls in mod_foam.F stubbed out.  PLOT_INTV_FOAM only drives
   ! OUTPUT_FOAM, whose body is an empty stub — bridged so the legacy
   ! engine sees the same value, but it steers nothing
   PLOT_INTV_FOAM       = model%foam%plot_intv
   f_source             = model%foam%f_source
   FoamTimeScale        = model%foam%time_scale
   BurstTimeNonBreaking = model%foam%burst_time_nb
   MinThick             = model%foam%min_thick
   CdFoam               = model%foam%cd_foam
# endif

# if defined (TRACKING)
   ! the tracker table itself still comes from TRACER_FILE, which both
   ! engines read, so only the path needs bridging
   TRACER_FILE = model%tracer%tracer_file
# endif

END SUBROUTINE READ_INPUT

# if defined (CARTESIAN)
!-------------------------------------------------------------------------------------
!
!    STATIONS is a subroutine to write station data
! Fengyan Shi modified based on Jeff Harris for Spherical
! here simply specify grid number i and j instead of x and y
!
! HISTORY:
!    09/16/2011  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE STATIONS
   USE GLOBAL
   USE INPUT_READ
# if defined (FOAM)
   USE MOD_FOAM
# endif
   IMPLICIT NONE

   INTEGER :: iunit
   INTEGER, DIMENSION(:), ALLOCATABLE :: BufferCount
   REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
   REAL(SP) :: dum1, dum2
   REAL(SP) :: eta_sta, u_sta, v_sta
   CHARACTER(LEN=80)::FILE_NAME = ' '
   CHARACTER(LEN=80)::TMP_NAME = ' '
   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallStation = .TRUE.
   SAVE FirstCallStation, BufferCount, ZUV_Buffer
# if defined (FOAM)
   REAL(SP) :: eddy_sta, EtaFoam_sta, EtaFoamMax_sta
# endif

! initialize stations
   FDIR = TRIM(RESULT_FOLDER)
   if (FirstCallStation) then
      FirstCallStation = .FALSE.
      ALLOCATE (ista(NumberStations), &
                jsta(NumberStations), &
                nsta(NumberStations))

# if defined (FOAM)
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 7))
# else
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
# endif

      BufferCount = 0
! calculate how many output components

! check existing

      INQUIRE (FILE=TRIM(STATIONS_FILE), EXIST=FILE_EXIST)
      IF (.NOT. FILE_EXIST) THEN
# if defined (PARALLEL)
         IF (MYID == 0) &
            WRITE (*, *) TRIM(STATIONS_FILE), ' THE STATION FILE CANNOT BE FOUND. STOP'
         CALL MPI_FINALIZE(ier)
         STOP
# else
         WRITE (*, *) TRIM(STATIONS_FILE), ' THE STATION FILE CANNOT BE FOUND. STOP'
         STOP
# endif
      END IF  ! exist

      open (100, FILE=TRIM(STATIONS_FILE))
      do i = 1, NumberStations
         read (100, *) dum1, dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
         !ista(i) = Nghost+dum1-npx*Mglob/px
         !jsta(i) = Nghost+dum2-npy*Nglob/py
         ista(i) = Nghost + dum1 - (iista - 1)
         jsta(i) = Nghost + dum2 - (jjsta - 1)
!---ykchoi Jan/23/2018]
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if

# else
         ista(i) = Nghost + dum1
         jsta(i) = Nghost + dum2
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if
# endif
      end do
   end if

! write to stations

   do i = 1, NumberStations
      if (nsta(i) .eq. 1) then
         iunit = 100 + i
         IF (mask(ista(i), jsta(i)) < 1) THEN
            eta_sta = ZERO
            u_sta = ZERO
            v_sta = ZERO
# if defined (FOAM)
            eddy_sta = ZERO
            EtaFoam_sta = ZERO
            EtaFoamMax_sta = ZERO
# endif
         ELSE  ! to avoid topography on nested water surface
            eta_sta = eta(ista(i), jsta(i))
            u_sta = u(ista(i), jsta(i))
            v_sta = v(ista(i), jsta(i))
# if defined (FOAM)
            eddy_sta = nu_break(ista(i), jsta(i))
            EtaFoam_sta = EtaFoam(ista(i), jsta(i))
            EtaFoamMax_sta = EtaFoamMax(ista(i), jsta(i))
# endif
         END IF

         IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
            BufferCount(i) = BufferCount(i) + 1
            ZUV_BUFFER(BufferCount(i), i, 1) = time
            ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
            ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
            ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
# if defined (FOAM)
            ZUV_BUFFER(BufferCount(i), i, 5) = eddy_sta
            ZUV_BUFFER(BufferCount(i), i, 6) = EtaFoam_sta
            ZUV_BUFFER(BufferCount(i), i, 7) = EtaFoamMax_sta
# endif
         ELSE
            DO j = 1, StationOutputBuffer
# if defined (FOAM)
               write (iunit, '(E21.10E4, 6E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 7)
# else
               write (iunit, '(E21.10E4, 3E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
# endif
            END DO
            BufferCount(i) = 0
            ZUV_BUFFER(:, i, 1) = TOTAL_TIME
# if defined (FOAM)
            ZUV_BUFFER(:, i, 2:7) = ZERO
# else
            ZUV_BUFFER(:, i, 2:4) = ZERO
# endif

# if defined (PARALLEL)
            if (myid .eq. 0) WRITE (*, *) 'WRITE OUT STATION ...'
# else
            WRITE (*, *) 'WRITE OUT STATIONS ...'
# endif

         END IF ! end buffer

      end if
   end do

! close station files
   if (TIME .ge. TOTAL_TIME) then
      do i = 1, NumberStations
         if (nsta(i) .eq. 1) then
            iunit = 100 + i
            close (iunit)
         end if
      end do
   end if

END SUBROUTINE STATIONS

# else

!-------------------------------------------------------------------------------------
!
!    STATIONS_SPHERICAL_IJ is a subroutine to write station data
!       Fengyan Shi modified based on Jeff Harris for Spherical
!       here simply specify grid number i and j instead of x and y
!
! HISTORY:
!    09/16/2011  Fengyan Shi
!
!-------------------------------------------------------------------------------------

SUBROUTINE STATIONS_SPHERICAL_IJ
   USE GLOBAL
# if defined (FOAM)
   USE MOD_FOAM
# endif
   IMPLICIT NONE

   INTEGER :: iunit
   INTEGER, DIMENSION(:), ALLOCATABLE :: BufferCount
   REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
   REAL(SP) :: dum1, dum2
   REAL(SP) :: eta_sta, u_sta, v_sta
   CHARACTER(LEN=80)::FILE_NAME = ' '
   CHARACTER(LEN=80)::TMP_NAME = ' '
   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallStation = .TRUE.
   SAVE FirstCallStation, BufferCount, ZUV_Buffer

# if defined (FOAM)
   REAL(SP) :: eddy_sta, EtaFoam_sta, EtaFoamMax_sta
# endif

! initialize stations
   FDIR = TRIM(RESULT_FOLDER)
   if (FirstCallStation) then
      FirstCallStation = .FALSE.
      ALLOCATE (ista(NumberStations), &
                jsta(NumberStations), &
                nsta(NumberStations))
# if defined (FOAM)
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 7))
# else
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
# endif

      BufferCount = 0

! calculate how many output components

      open (100, FILE=TRIM(STATIONS_FILE))
      do i = 1, NumberStations
         read (100, *) dum1, dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
         !ista(i) = Nghost+dum1-npx*Mglob/px
         !jsta(i) = Nghost+dum2-npy*Nglob/py
         ista(i) = Nghost + dum1 - (iista - 1)
         jsta(i) = Nghost + dum2 - (jjsta - 1)
!---ykchoi Jan/23/2018]
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if

# else
         ista(i) = Nghost + dum1
         jsta(i) = Nghost + dum2
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if
# endif
      end do
   end if

! write to stations

   do i = 1, NumberStations
      if (nsta(i) .eq. 1) then
         iunit = 100 + i
         IF (mask(ista(i), jsta(i)) < 1) THEN
            eta_sta = ZERO
            u_sta = ZERO
            v_sta = ZERO
# if defined (FOAM)
            eddy_sta = ZERO
            EtaFoam_sta = ZERO
            EtaFoamMax_sta = ZERO
# endif
         ELSE  ! to avoid topography on nested water surface
            eta_sta = eta(ista(i), jsta(i))
            u_sta = u(ista(i), jsta(i))
            v_sta = v(ista(i), jsta(i))
# if defined (FOAM)
            eddy_sta = nu(ista(i), jsta(i))
            EtaFoam_sta = EtaFoam(ista(i), jsta(i))
            EtaFoamMax_sta = EtaFoamMax(ista(i), jsta(i))
# endif
         END IF

         IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
            BufferCount(i) = BufferCount(i) + 1
            ZUV_BUFFER(BufferCount(i), i, 1) = time
            ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
            ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
            ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
# if defined (FOAM)
            ZUV_BUFFER(BufferCount(i), i, 5) = eddy_sta
            ZUV_BUFFER(BufferCount(i), i, 6) = EtaFoam_sta
            ZUV_BUFFER(BufferCount(i), i, 7) = EtaFoamMax_sta
# endif
         ELSE
            DO j = 1, StationOutputBuffer
# if defined (FOAM)
               write (iunit, '(E21.10E4, 6E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 7)
# else
               write (iunit, '(E21.10E4, 3E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
# endif
            END DO
            BufferCount(i) = 0
            ZUV_BUFFER(:, i, 1) = TOTAL_TIME
# if defined (FOAM)
            ZUV_BUFFER(:, i, 2:7) = ZERO
# else
            ZUV_BUFFER(:, i, 2:4) = ZERO
# endif

# if defined (PARALLEL)
            if (myid .eq. 0) WRITE (*, *) 'WRITE OUT STATION ...'
# else
            WRITE (*, *) 'WRITE OUT STATIONS ...'
# endif

         END IF ! end buffer

      end if
   end do

! close station files
   if (TIME .ge. TOTAL_TIME) then
      do i = 1, NumberStations
         if (nsta(i) .eq. 1) then
            iunit = 100 + i
            close (iunit)
         end if
      end do
   end if

END SUBROUTINE STATIONS_SPHERICAL_IJ

!-------------------------------------------------------------------------------------
!
!    STATIONS is a subroutine to write station data,  works in spherical
!
!    HISTORY:
!   04/05/2011  Jeff Harris
!
!-------------------------------------------------------------------------------------
SUBROUTINE STATIONS
   USE GLOBAL
# if defined (FOAM)
   USE MOD_FOAM
# endif
   IMPLICIT NONE

   INTEGER :: iunit
   INTEGER, DIMENSION(:), ALLOCATABLE :: BufferCount
   REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
   REAL(SP) :: dum1, dum2
   REAL(SP) :: eta_sta, u_sta, v_sta
   CHARACTER(LEN=80)::FILE_NAME = ' '
   CHARACTER(LEN=80)::TMP_NAME = ' '
   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallStationSP = .TRUE.
   SAVE FirstCallStationSP, BufferCount, ZUV_Buffer
# if defined (FOAM)
   REAL(SP) :: eddy_sta, EtaFoam_sta, EtaFoamMax_sta
# endif

! initialize stations
   FDIR = TRIM(RESULT_FOLDER)
   if (FirstCallStationSP) then
      FirstCallStationSP = .FALSE.
      ALLOCATE (ista(NumberStations), &
                jsta(NumberStations), &
                nsta(NumberStations))
# if defined (FOAM)
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 7))
# else
      ALLOCATE (BufferCount(NumberStations), &
                ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
# endif

      BufferCount = 0

!       ALLOCATE(envelope(Mloc,Nloc))
!       envelope = 0.d0
      open (100, FILE=TRIM(STATIONS_FILE))
      do i = 1, NumberStations
         read (100, *) dum1, dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
!          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) &
!                     -npx*Mglob/px
!          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) &
!                     -npy*Nglob/py
         ista(i) = Nghost + 1 + nint((dum2 - Lon_West)/Dphi) &
                   - (iista - 1)
         jsta(i) = Nghost + 1 + nint((dum1 - Lat_South)/Dtheta) &
                   - (jjsta - 1)
!---ykchoi Jan/23/2018]
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if
# else
         ista(i) = Nghost + 1 + nint((dum2 - Lon_West)/Dphi)
         jsta(i) = Nghost + 1 + nint((dum1 - Lat_South)/Dtheta)
         if ((ista(i) .ge. Ibeg) .and. (ista(i) .le. Iend) .and. &
             (jsta(i) .ge. Jbeg) .and. (jsta(i) .le. Jend)) then
            nsta(i) = 1
            write (file_name(1:4), '(I4.4)') i
            TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
            iunit = 100 + i
            open (iunit, FILE=TMP_NAME)
         else
            nsta(i) = 0
         end if
# endif
      end do
   end if

! write to stations

   do i = 1, NumberStations
      if (nsta(i) .eq. 1) then
         iunit = 100 + i

! for coupling, make sure dry point
         IF (mask(ista(i), jsta(i)) < 1) THEN
            eta_sta = ZERO
            u_sta = ZERO
            v_sta = ZERO
# if defined (FOAM)
            eddy_sta = ZERO
            EtaFoam_sta = ZERO
            EtaFoamMax_sta = ZERO
# endif
         ELSE  ! to avoid topography on nested water surface
            eta_sta = eta(ista(i), jsta(i))
            u_sta = u(ista(i), jsta(i))
            v_sta = v(ista(i), jsta(i))
# if defined (FOAM)
            eddy_sta = nu(ista(i), jsta(i))
            EtaFoam_sta = EtaFoam(ista(i), jsta(i))
            EtaFoamMax_sta = EtaFoamMax(ista(i), jsta(i))
# endif
         END IF

         IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
            BufferCount(i) = BufferCount(i) + 1
            ZUV_BUFFER(BufferCount(i), i, 1) = time
            ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
            ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
            ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
# if defined (FOAM)
            ZUV_BUFFER(BufferCount(i), i, 5) = eddy_sta
            ZUV_BUFFER(BufferCount(i), i, 6) = EtaFoam_sta
            ZUV_BUFFER(BufferCount(i), i, 7) = EtaFoamMax_sta
# endif
         ELSE
            DO j = 1, StationOutputBuffer
# if defined (FOAM)
               write (iunit, '(E21.10E4, 6E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 7)
# else
               write (iunit, '(E21.10E4, 3E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
# endif
            END DO
            BufferCount(i) = 0
            ZUV_BUFFER(:, i, 1) = TOTAL_TIME
# if defined (FOAM)
            ZUV_BUFFER(:, i, 2:7) = ZERO
# else
            ZUV_BUFFER(:, i, 2:4) = ZERO
# endif

# if defined (PARALLEL)
            if (myid .eq. 0) WRITE (*, *) 'WRITE OUT STATION ...'
# else
            WRITE (*, *) 'WRITE OUT STATIONS ...'
# endif

         END IF ! end buffer

      end if
   end do

! close station files
   if (TIME .ge. TOTAL_TIME) then

      do i = 1, NumberStations
         if (nsta(i) .eq. 1) then
            iunit = 100 + i
            close (iunit)
         end if
      end do
   end if

END SUBROUTINE STATIONS

# endif

!-------------------------------------------------------------------------------------
!
!    PREVIEW is subroutine for print-out of field data
!
!  HISTORY:
!    05/01/2010  Fengyan Shi
!    06/01/2015  Young-Kwang Choi, change file number to 5 digits,
!                        such as eta_00001
!
!-------------------------------------------------------------------------------------
SUBROUTINE PREVIEW
   USE GLOBAL
# if defined (VESSEL)
   USE VESSEL_MODULE
# endif
# if defined(METEO)
   USE METEO_MODULE
# endif
# if defined(SEDIMENT)
   USE SEDIMENT_MODULE
# endif
# if defined(FOAM)
   USE MOD_FOAM
# endif

   IMPLICIT NONE

   CHARACTER(LEN=80)::FILE_NAME = ' '
   CHARACTER(LEN=80)::FILE_NAME_MEAN = ' '
   CHARACTER(LEN=80)::TMP_NAME = ' '
   CHARACTER(LEN=80)::FDIR = ' '

   FDIR = TRIM(RESULT_FOLDER)

   ICOUNT = ICOUNT + 1

# if defined (PARALLEL)
   if (myid .eq. 0) then
      WRITE (3, 102) 'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME, '/', Total_Time
      WRITE (*, 102) 'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME, '/', Total_Time
   end if
# else
   WRITE (*, 102) 'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME, '/', Total_Time
# endif

102 FORMAT(A20, I6, A14, F12.3, A2, F12.3)

   !ykchoi
   !itmp1=mod(icount/1000,10)
   !itmp2=mod(icount/100,10)
   !itmp3=mod(icount/10,10)
   !itmp4=mod(icount,10)
   itmp1 = mod(icount/10000, 10)
   itmp2 = mod(icount/1000, 10)
   itmp3 = mod(icount/100, 10)
   itmp4 = mod(icount/10, 10)
   itmp5 = mod(icount, 10)

   write (file_name(1:1), '(I1)') itmp1
   write (file_name(2:2), '(I1)') itmp2
   write (file_name(3:3), '(I1)') itmp3
   write (file_name(4:4), '(I1)') itmp4
   write (file_name(5:5), '(I1)') itmp5   !ykchoi

   IF (ICOUNT == 1) THEN
   IF (OUT_DEPTH .OR. BREAKWATER) THEN
      TMP_NAME = TRIM(FDIR)//'dep.out'
      call PutFile(TMP_NAME, DEPTH)
      TMP_NAME = TRIM(FDIR)//'cd_breakwater.out'
      call PutFile(TMP_NAME, CD_breakwater)
   END IF
   END IF
![ykchoi
   write (10000, *) time, dt
!ykchoi]

   IF (OUT_ETA) THEN
      TMP_NAME = TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, Eta)
   END IF

# if defined (UseEtaScreen)
   IF (OUT_ETAscreen) THEN
      TMP_NAME = TRIM(FDIR)//'etasrn_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, EtaScreen)
   END IF
# endif

# if defined(AB_OUTPUT)
   TMP_NAME = TRIM(FDIR)//'Ax_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, grdAx)
   TMP_NAME = TRIM(FDIR)//'Ay_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, grdAy)
   TMP_NAME = TRIM(FDIR)//'Bx_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, grdBx)
   TMP_NAME = TRIM(FDIR)//'By_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, grdBy)
# endif

   IF (OUT_Hmax) THEN
      TMP_NAME = TRIM(FDIR)//'hmax_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, HeightMax)
   END IF

   IF (OUT_Hmin) THEN
      TMP_NAME = TRIM(FDIR)//'hmin_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, HeightMin)
   END IF

   IF (OUT_Umax) THEN
      TMP_NAME = TRIM(FDIR)//'umax_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, VelocityMax)
   END IF

   IF (OUT_MFmax) THEN
      TMP_NAME = TRIM(FDIR)//'MFmax_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, MomentumFluxMax)
   END IF

   IF (OUT_VORmax) THEN
      TMP_NAME = TRIM(FDIR)//'VORmax_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, VorticityMax)
   END IF

   IF (OUT_U) THEN
      TMP_NAME = TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, U)
   END IF

   IF (OUT_V) THEN
      TMP_NAME = TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, V)
   END IF

   IF (OUT_MASK) THEN
      TMP_NAME = TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
      Int2Flo = MASK
      call PutFile(TMP_NAME, Int2Flo)
   END IF

   IF (OUT_MASK9) THEN
      TMP_NAME = TRIM(FDIR)//'mask9_'//TRIM(FILE_NAME)
      Int2Flo = MASK9
      call PutFile(TMP_NAME, Int2Flo)
   END IF

210 FORMAT(5000I3)

   IF (OUT_P) THEN
      TMP_NAME = TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, P(1:Mloc, 1:Nloc))
   END IF

   IF (OUT_Q) THEN
      TMP_NAME = TRIM(FDIR)//'q_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, Q(1:Mloc, 1:Nloc))
   END IF

   IF (OUT_AGE) THEN
      IF (SHOW_BREAKING) THEN
         TMP_NAME = TRIM(FDIR)//'age_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, AGE_BREAKING)
      END IF
   END IF

   IF (OUT_ROLLER) THEN
      TMP_NAME = TRIM(FDIR)//'roller_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, ROLLER_FLUX)
   END IF

   IF (OUT_UNDERTOW) THEN
      TMP_NAME = TRIM(FDIR)//'U_undertow_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, UNDERTOW_U)
      TMP_NAME = TRIM(FDIR)//'V_undertow_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, UNDERTOW_V)
   END IF

   IF (VISCOSITY_BREAKING) THEN
      IF (OUT_NU) THEN
         TMP_NAME = TRIM(FDIR)//'nubrk_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, nu_break)
      END IF
!        TMP_NAME = TRIM(FDIR)//'etat_'//TRIM(FILE_NAME)
!         call PutFile(TMP_NAME,etat)
   END IF

   IF (OUT_FrcX) THEN
      TMP_NAME = TRIM(FDIR)//'FrcInsX_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, FrcInsX)
   END IF
   IF (OUT_FrcY) THEN
      TMP_NAME = TRIM(FDIR)//'FrcInsY_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, FrcInsY)
   END IF
   IF (OUT_BrkdisX) THEN
      TMP_NAME = TRIM(FDIR)//'BrkSrcX_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, BreakSourceX)
   END IF
   IF (OUT_BrkdisY) THEN
      TMP_NAME = TRIM(FDIR)//'BrkSrcY_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, BreakSourceY)
   END IF

   IF (OUT_Time) THEN
      TMP_NAME = TRIM(FDIR)//'time_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, ARRTIME)
   END IF

# if defined (METEO)
   IF (OUT_METEO) THEN

      IF (WindHollandModel) THEN
         TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, StormPressureTotal)
         TMP_NAME = TRIM(FDIR)//'Ustorm_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, WindU2D)
         TMP_NAME = TRIM(FDIR)//'Vstorm_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, WindV2D)
      END IF

      IF (MeteoGausian) THEN
         TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, StormPressureTotal)
      END IF

      IF (SlideModel) THEN
         TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME, StormPressureTotal)
      END IF

   END IF
# endif

# if defined (VESSEL)
   IF (OUT_VESSEL) THEN
# if defined (VESSEL_PANEL_SOURCE)
      TMP_NAME = TRIM(FDIR)//'Fves_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, VesselFluxGradient)
# else
      TMP_NAME = TRIM(FDIR)//'Pves_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, VesselPressureTotal)
# endif
      ! end panel source

# if defined (PROPELLER)
      TMP_NAME = TRIM(FDIR)//'VesUp_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, UpTotal)
      TMP_NAME = TRIM(FDIR)//'VesVp_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, VpTotal)
# endif

   END IF
# endif

! sediment
# if defined (SEDIMENT)
   TMP_NAME = TRIM(FDIR)//'C_'//TRIM(FILE_NAME)
!        call PutFile(TMP_NAME,CH *Sdensity*RHO_WATER)
   ! change to non-dimensional
   call PutFile(TMP_NAME, CH)
   TMP_NAME = TRIM(FDIR)//'Pick_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Pickup)
   TMP_NAME = TRIM(FDIR)//'Depo_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, D)

   TMP_NAME = TRIM(FDIR)//'Pavg_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, P_ave)
   TMP_NAME = TRIM(FDIR)//'Davg_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, D_ave)

   TMP_NAME = TRIM(FDIR)//'DchgS_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, TotalSuspendLoad/(1.0_SP - n_porosity))
   TMP_NAME = TRIM(FDIR)//'DchgB_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, TotalBedLoad/(1.0_SP - n_porosity))

   TMP_NAME = TRIM(FDIR)//'BedFx_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, BedFluxX)
   TMP_NAME = TRIM(FDIR)//'BedFy_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, BedFluxY)

   TMP_NAME = TRIM(FDIR)//'dep_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Depth)

   TMP_NAME = TRIM(FDIR)//'BedStr_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Tau_xy)

   TMP_NAME = TRIM(FDIR)//'Aval_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Zb_avalanching)

   TMP_NAME = TRIM(FDIR)//'AvalAc_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Aval_accum)

#if defined (SEDIMENT_DEBUG)
   TMP_NAME = TRIM(FDIR)//'Cb_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Cb_debug)
   TMP_NAME = TRIM(FDIR)//'Ca_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Ca_debug)
   TMP_NAME = TRIM(FDIR)//'Redu_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Reduct_debug)
   TMP_NAME = TRIM(FDIR)//'TauEx_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Tau_extra)
   TMP_NAME = TRIM(FDIR)//'Hpo_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, Hpo)
# endif

# endif
! end sediment

! foam
# if defined (FOAM)
   TMP_NAME = TRIM(FDIR)//'FoamEta_'//TRIM(FILE_NAME)
   call PutFile(TMP_NAME, EtaFoam)
# endif
! end foam

   IF (OUT_TMP) THEN
      TMP_NAME = TRIM(FDIR)//'tmp_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, tmp4preview)
   END IF

101 continue

END SUBROUTINE PREVIEW

!-------------------------------------------------------------------------------------
!
!    PREVIEW_MEAN is subroutine for print-out of mean field data
!
!  HISTORY:
!    03/22/2016  Fengyan Shi
!-------------------------------------------------------------------------------------
SUBROUTINE PREVIEW_MEAN
   USE GLOBAL
   IMPLICIT NONE
   REAL(SP), DIMENSION(Mloc, Nloc) :: tmpout

   CHARACTER(LEN=80)::FILE_NAME = ' '
   CHARACTER(LEN=80)::FDIR = ' '
   CHARACTER(LEN=80)::TMP_NAME = ' '

   FDIR = TRIM(RESULT_FOLDER)

   ICOUNT_MEAN = ICOUNT_MEAN + 1

# if defined (PARALLEL)
   if (myid .eq. 0) then
      WRITE (3, 102) 'PRINTING MEAN FILE', icount_mean
      WRITE (*, 102) 'PRINTING MEAN FILE', icount_mean
   end if
# else
   WRITE (*, 102) 'PRINTING MEAN FILE', icount_mean
# endif

102 FORMAT(A20, I6)

   itmp1 = mod(icount_mean/10000, 10)
   itmp2 = mod(icount_mean/1000, 10)
   itmp3 = mod(icount_mean/100, 10)
   itmp4 = mod(icount_mean/10, 10)
   itmp5 = mod(icount_mean, 10)

   write (file_name(1:1), '(I1)') itmp1
   write (file_name(2:2), '(I1)') itmp2
   write (file_name(3:3), '(I1)') itmp3
   write (file_name(4:4), '(I1)') itmp4
   write (file_name(5:5), '(I1)') itmp5

   IF (OUT_Umean) THEN
      TMP_NAME = TRIM(FDIR)//'umean_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, Umean)
      tmpout = P_mean/Max(Depth + ETAmean, MinDepthFrc)
      TMP_NAME = TRIM(FDIR)//'ulagm_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, tmpout)
   END IF
   IF (OUT_Vmean) THEN
      TMP_NAME = TRIM(FDIR)//'vmean_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, Vmean)
      tmpout = Q_mean/Max(Depth + ETAmean, MinDepthFrc)
      TMP_NAME = TRIM(FDIR)//'vlagm_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, tmpout)
   END IF
   IF (OUT_ETAmean) THEN
      TMP_NAME = TRIM(FDIR)//'etamean_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, ETAmean)
   END IF
   IF (OUT_WaveHeight) THEN
      TMP_NAME = TRIM(FDIR)//'Hrms_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, WaveHeightRMS)
      TMP_NAME = TRIM(FDIR)//'Havg_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, WaveHeightAve)
      TMP_NAME = TRIM(FDIR)//'Hsig_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, SigWaveHeight)
   END IF

   ! output FRCYsum,FRCYmean
   !        DxSxx,DySxy,DySyy,DxSxy,PgrdX,PgrdY,DxUUH,DyUVH,DyVVH,DxUVH

   IF (OUT_Radiation) THEN
      TMP_NAME = TRIM(FDIR)//'Sxx_'//TRIM(FILE_NAME)
      tmpout = UUmean - WWmean + 0.5*9.8*ETA2mean
      call PutFile(TMP_NAME, tmpout)
      TMP_NAME = TRIM(FDIR)//'Sxy_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, UVmean)
      TMP_NAME = TRIM(FDIR)//'Syy_'//TRIM(FILE_NAME)
      tmpout = VVmean - WWmean + 0.5*9.8*ETA2mean
      call PutFile(TMP_NAME, tmpout)

      TMP_NAME = TRIM(FDIR)//'DxSxx_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DxSxx)
      TMP_NAME = TRIM(FDIR)//'DySxy_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DySxy)
      TMP_NAME = TRIM(FDIR)//'DySyy_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DySyy)
      TMP_NAME = TRIM(FDIR)//'DxSxy_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DxSxy)
      TMP_NAME = TRIM(FDIR)//'PgrdX_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, PgrdX)
      TMP_NAME = TRIM(FDIR)//'PgrdY_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, PgrdY)
      TMP_NAME = TRIM(FDIR)//'DxUUH_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DxUUH)
      TMP_NAME = TRIM(FDIR)//'DyUVH_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DyUVH)
      TMP_NAME = TRIM(FDIR)//'DyVVH_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DyVVH)
      TMP_NAME = TRIM(FDIR)//'DxUVH_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, DxUVH)
      TMP_NAME = TRIM(FDIR)//'FRCX_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, FRCXmean)
      TMP_NAME = TRIM(FDIR)//'FRCY_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, FRCYmean)
      TMP_NAME = TRIM(FDIR)//'BrkDissX_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, BreakDissX)
      TMP_NAME = TRIM(FDIR)//'BrkDissY_'//TRIM(FILE_NAME)
      call PutFile(TMP_NAME, BreakDissY)

   END IF

END SUBROUTINE PREVIEW_MEAN

# if defined (PARALLEL)
!-------------------------------------------------------------------------------------
!
!    GetFile is subroutine for reading field data
!
!    HISTORY:
!    05/01/2010  Fengyan Shi
!    05/08/2017  Young-Kwang Choi
!-------------------------------------------------------------------------------------

SUBROUTINE GetFile(FILE, PHI)
   USE GLOBAL
   IMPLICIT NONE

   REAL(SP), DIMENSION(MGlob + 2*Nghost, NGlob + 2*Nghost) :: PHIGLOB
   CHARACTER(LEN=80) FILE
   REAL(SP), DIMENSION(Mloc, Nloc), INTENT(OUT) :: PHI

![-------ykchoi (08/May/2017)
   INTEGER :: irank, lenx, leny, lenxy, ireq
   INTEGER :: Nista, Niend, Njsta, Njend
   INTEGER :: istanum, iendnum, jstanum, jendnum
   INTEGER, ALLOCATABLE :: Nistas(:), Niends(:), Njstas(:), Njends(:)
   INTEGER :: istatus(mpi_status_size)
   REAL(SP), ALLOCATABLE :: xx(:, :)
! -------ykchoi (08/May/2017) ]

! TEMP

   if (myid .eq. 0) then
      OPEN (1, FILE=TRIM(FILE))
      DO J = Nghost + 1, NGlob + NGhost
         READ (1, *) (PHIGLOB(I, J), I=Nghost + 1, MGlob + Nghost)
      END DO
      CLOSE (1)
! ghost cells
      DO I = Nghost + 1, MGlob + Nghost
         DO J = 1, Nghost
            PHIGLOB(I, J) = PHIGLOB(I, Nghost + 1)
         END DO
         DO J = NGlob + Nghost + 1, NGlob + 2*Nghost
            PHIGLOB(I, J) = PHIGLOB(I, NGlob + Nghost)
         END DO
      END DO
      DO J = 1, NGlob + 2*Nghost
         DO I = 1, Nghost
            PHIGLOB(I, J) = PHIGLOB(Nghost + 1, J)
         END DO
         DO I = MGlob + Nghost + 1, MGlob + 2*Nghost
            PHIGLOB(I, J) = PHIGLOB(MGlob + Nghost, J)
         END DO
      END DO
   end if

![-------ykchoi (08/May/2017)
   Nista = iista + Nghost; 
   Niend = iiend + Nghost; 
   Njsta = jjsta + Nghost; 
   Njend = jjend + Nghost; 
   allocate (Nistas(nprocs), Niends(nprocs), Njstas(nprocs), Njends(nprocs))

   call MPI_Gather(Nista, 1, MPI_INTEGER, Nistas, 1, MPI_INTEGER, &
                   0, MPI_COMM_WORLD, ier)
   call MPI_Gather(Niend, 1, MPI_INTEGER, Niends, 1, MPI_INTEGER, &
                   0, MPI_COMM_WORLD, ier)
   call MPI_Gather(Njsta, 1, MPI_INTEGER, Njstas, 1, MPI_INTEGER, &
                   0, MPI_COMM_WORLD, ier)
   call MPI_Gather(Njend, 1, MPI_INTEGER, Njends, 1, MPI_INTEGER, &
                   0, MPI_COMM_WORLD, ier)

   if (myid == 0) then
      PHI = PHIGLOB(1:Mloc, 1:Nloc)
   end if

   do irank = 1, px*py - 1
      if (myid == 0) then
         istanum = Nistas(irank + 1) - Nghost
         iendnum = Niends(irank + 1) + Nghost
         jstanum = Njstas(irank + 1) - Nghost
         jendnum = Njends(irank + 1) + Nghost

         lenx = iendnum - istanum + 1
         leny = jendnum - jstanum + 1
         lenxy = lenx*leny
         allocate (xx(lenx, leny))

         xx = PHIGLOB(istanum:iendnum, jstanum:jendnum)
         call mpi_isend(xx, lenxy, mpi_sp, irank, 1, mpi_comm_world, ireq, ier)
         call mpi_wait(ireq, istatus, ier)
         deallocate (xx)

      elseif (myid == irank) then

         lenx = Niend - Nista + 1 + 2*Nghost
         leny = Njend - Njsta + 1 + 2*Nghost
         lenxy = lenx*leny

         call mpi_irecv(PHI, lenxy, mpi_sp, 0, 1, mpi_comm_world, ireq, ier)
         call mpi_wait(ireq, istatus, ier)

      end if
   end do

   deallocate (Nistas, Niends, Njstas, Njends)

! -------ykchoi (08/May/2017) ]

END SUBROUTINE Getfile

# endif

# if defined (PARALLEL)
!-------------------------------------------------------------------------------------
!
!    PutFile is subroutine for print-out of field data
!
!    HISTORY:
!      05/01/2010  Fengyan Shi
!      05/06/2017  Young-Kwang Choi
!-------------------------------------------------------------------------------------

SUBROUTINE PutFile(FILE_NAME, PHI)
   USE GLOBAL
   USE PARALLEL_FIELD_IO
   IMPLICIT NONE

   CHARACTER(LEN=80) FILE_NAME
   REAL(SP), DIMENSION(Mloc, Nloc), INTENT(IN) :: PHI

   CHARACTER(LEN=80)::TMP_NAME = ' '

   SELECT CASE (TRIM(FIELD_IO_TYPE))
   CASE ('ASCII', 'ascii')
      CALL PutFileASCII(FILE_NAME, PHI)
   CASE ('BINARY', 'binary')
      Call PutFileBinary(FILE_NAME, PHI)
   CASE DEFAULT
      !Defaults to ASCII case for non-valid input
      CALL PutFileASCII(FILE_NAME, PHI)
   END SELECT

END SUBROUTINE Putfile

# else
!-------------------------------------------------------------------------------------
!
!    PutFile is subroutine for print-out of field data
!
!    HISTORY:
!      05/01/2010  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE PutFile(FILE, PHI)
   USE PARAM
   USE GLOBAL
   IMPLICIT NONE
   REAL(SP), DIMENSION(Mloc, Nloc), INTENT(IN) :: PHI
   CHARACTER(LEN=80) FILE
   LOGICAL :: FirstCallPutFile = .TRUE.
   SAVE FirstCallPutFile

! first time call
   IF (FirstCallPutFile) THEN
      FirstCallPutFile = .FALSE.
! format length
      write (FORMAT_LEN(1:1), '(A1)') '('
      write (FORMAT_LEN(2:8), '(I7)') Mglob
      write (FORMAT_LEN(9:15), '(A7)') 'E16.6E4'
      write (FORMAT_LEN(16:16), '(A1)') ')'
   END IF

   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallVessel = .TRUE.
   SAVE FirstCallVessel

# if defined (PARALLEL)
   IF (myid .eq. 0) THEN
# endif

! initialize stations
      if (FirstCallVessel) then
         FDIR = TRIM(RESULT_FOLDER)
         FirstCallVessel = .FALSE.
         open (99, FILE=TRIM(FDIR)//'Resis.txt')
      end if

! write to stations

      write (99, '(60E16.5)') time, (ResistanceX(i), i=1, NumVessel), &
         (ResPosX(i), i=1, NumVessel), &
         (ResNegX(i), i=1, NumVessel), &
         (ResistanceY(i), i=1, NumVessel), &
         (ResPosY(i), i=1, NumVessel), &
         (ResNegY(i), i=1, NumVessel)

! close station files
      if (TIME .ge. TOTAL_TIME) then
         CLOSE (99)
      end if

# if defined (PARALLEL)
   END IF
# endif

END SUBROUTINE OUTPUT_VESSEL

# endif
! end vessel

# if defined (SEDIMENT)
!-------------------------------------------------------------------------------------
!
!    OUTPUT_SEDIMENT is a subroutine to write vessel data
!
! HISTORY:
!    02/01/2018  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE OUTPUT_SEDIMENT
   USE GLOBAL
   USE INPUT_READ
# if defined (VESSEL)
   USE VESSEL_MODULE
# endif
   IMPLICIT NONE
   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallSediment = .TRUE.
   SAVE FirstCallSediment

# if defined (PARALLEL)
   IF (myid .eq. 0) THEN
# endif

! initialize stations
      if (FirstCallSediment) then
         FDIR = TRIM(RESULT_FOLDER)
         FirstCallSediment = .FALSE.
      end if

! time series here

# if defined (PARALLEL)
   END IF
# endif

END SUBROUTINE OUTPUT_SEDIMENT

# endif
! end sediment

! ----------
# if defined (FOAM)
!-------------------------------------------------------------------------------------
!
!    OUTPUT_FOAM is a subroutine to write foam data
!
!
!-------------------------------------------------------------------------------------
SUBROUTINE OUTPUT_FOAM
   USE GLOBAL
   USE INPUT_READ
# if defined (VESSEL)
   USE VESSEL_MODULE
# endif
   IMPLICIT NONE
   CHARACTER(LEN=80)::FDIR = ' '
   LOGICAL :: FirstCallFoam = .TRUE.
   SAVE FirstCallFoam

# if defined (PARALLEL)
   IF (myid .eq. 0) THEN
# endif

! initialize stations
      if (FirstCallFoam) then
         FDIR = TRIM(RESULT_FOLDER)
         FirstCallFoam = .FALSE.
      end if

! time series here

# if defined (PARALLEL)
   END IF
# endif

END SUBROUTINE OUTPUT_FOAM

# endif
! end foam

