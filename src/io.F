!------------------------------------------------------------------------------------
!
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
     USE TRACER
# endif

    IMPLICIT NONE

     SCREEN_COUNT=SCREEN_COUNT+DT

     IF(SCREEN_COUNT>=SCREEN_INTV)THEN
      SCREEN_COUNT=SCREEN_COUNT-SCREEN_INTV
      CALL STATISTICS
     ENDIF

! stations
      IF(NumberStations>0)THEN
      PLOT_COUNT_STATION=PLOT_COUNT_STATION+DT
      IF(PLOT_COUNT_STATION>=PLOT_INTV_STATION)THEN
       PLOT_COUNT_STATION=PLOT_COUNT_STATION-PLOT_INTV_STATION
# if defined (SPHERICAL_IJ_STATION)
       ! use IJ for coupling purpose
       CALL STATIONS_SPHERICAL_IJ
# else
       CALL STATIONS
# endif
      ENDIF
      ENDIF
! preview

      IF(TIME>=PLOT_START_TIME)THEN

	PLOT_COUNT=PLOT_COUNT+DT
      IF(PLOT_COUNT>=PLOT_INTV)THEN
       PLOT_COUNT=PLOT_COUNT-PLOT_INTV
       CALL PREVIEW
      ENDIF
# if defined (VESSEL)
      PLOT_COUNT_VESSEL=PLOT_COUNT_VESSEL+DT
      IF(PLOT_COUNT_VESSEL>=PLOT_INTV_VESSEL)THEN
       PLOT_COUNT_VESSEL=PLOT_COUNT_VESSEL-PLOT_INTV_VESSEL
       CALL OUTPUT_VESSEL
      ENDIF      
# endif
# if defined (SEDIMENT)
      PLOT_COUNT_SEDIMENT=PLOT_COUNT_SEDIMENT+DT
      IF(PLOT_COUNT_SEDIMENT>=PLOT_INTV_SEDIMENT)THEN
       PLOT_COUNT_SEDIMENT=PLOT_COUNT_SEDIMENT-PLOT_INTV_SEDIMENT
       CALL OUTPUT_SEDIMENT
      ENDIF      
# endif
# if defined (TRACKING)
      PLOT_COUNT_TRACKING=PLOT_COUNT_TRACKING+DT
      IF(PLOT_COUNT_TRACKING>=PLOT_INTV_TRACKING)THEN
       PLOT_COUNT_TRACKING=PLOT_COUNT_TRACKING-PLOT_INTV_TRACKING
       CALL OUTPUT_TRACKING
      ENDIF      
# endif

   ENDIF ! end plot start time


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
    USE LOGGER
![jychoi added this for intel compiler 14.12.23    
# if defined(INTEL)
     USE IFPORT
# endif

!jychoi 14.12.23]
    IMPLICIT NONE
    CHARACTER(LEN=80) FILE_NAME
    CHARACTER(LEN=80) MKFOLDER
    INTEGER::LINE
    INTEGER :: ierr
    INTEGER :: I_comp
    LOGICAL :: INPUT_PHASE = .FALSE.
    TYPE(LOG_BUFFER), TARGET :: logbuff 
    !>by Zhouteng Ye
    CHARACTER(LEN=80)::INPUT_NAME=''
    LOGICAL :: read_check
![ykchoi
    CHARACTER(LEN=80)::FDIR=' '
!ykchoi]

# if defined (PARALLEL)
    CALL MPI_COMM_SIZE (MPI_COMM_WORLD, nprocs, ier)   !ykchoi(04/May/2017)
    CALL MPI_COMM_RANK (MPI_COMM_WORLD, myid, ier)
# endif

      FDIR=TRIM(RESULT_FOLDER)
      OPEN(10000,FILE='time_dt.out',STATUS='UNKNOWN')

      OPEN(3,FILE='LOG.txt')   

! read everything from input.txt

      !>by Zhouteng Ye
      !> Get the argument from the command.
      !> If no input in command, file name is 'input.txt' (same as before)
      !> If the input comes with other name, read the correponding file
      CALL GETARG(1,INPUT_NAME) 
      if (INPUT_NAME .eq. '') Then
        FILE_NAME='input.txt'
      Else
        FILE_NAME=INPUT_NAME
      endif
      INPUT_FILE_NAME=FILE_NAME

      CALL SET_INPUT_LOGGER(FILE_NAME, logbuff)

      CALL WRITE_BANNER('LOG FILE')
      CALL READ_PARAMETER(title, 'TITLE', '---TEST RUN----')
      ! Clearing buffer as output not needed 
      CALL logbuff % CLEAR()

      CALL WRITE_BANNER('Input Start')

! -----------------------------------------------------------------------------

# if defined (PARALLEL)
      CALL WRITE_BANNER('PARALLEL')
      CALL READ_PARAMETER(PX, 'PX', 1)
      CALL READ_PARAMETER(PY, 'PY', 1)
      CALL logbuff % FLUSH2OUTPUT()
# endif

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('GRID INFO')
      CALL READ_PARAMETER(Mglob, 'Mglob')
      CALL READ_PARAMETER(Nglob, 'Nglob')

# if defined (CARTESIAN)
      CALL READ_PARAMETER(DX, 'DX')
      CALL READ_PARAMETER(DY, 'DY')
#else
      CALL READ_PARAMETER(StretchGrid, 'StretchGrid', .FALSE.)

      IF (StretchGrid) THEN

         CALL WRITE_ALL('Stretch Grid Mode')
         CALL READ_PARAMETER(DX_FILE      , 'DX_FILE')
         CALL READ_PARAMETER(DY_FILE      , 'DY_FILE')
         CALL READ_PARAMETER(Coriolis_FILE, 'CORIOLIS_FILE')

      ELSE

         CALL WRITE_ADLL('Spherical Grid Mode'
         CALL READ_PARAMETER(Lon_West , 'Lon_West')
         CALL READ_PARAMETER(Lat_South, 'Lat_South')
         CALL READ_PARAMETER(Dphi     , 'Dphi')
         CALL READ_PARAMETER(Dtheta   , 'Dtheta')

      END IF 
# endif
      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('BATHYMETRY INFO')

      CALL READ_PARAMETER(DEPTH_TYPE, 'DEPTH_TYPE', 'FLAT')
      
      IF ( DEPTH_TYPE(1:3)=='DAT') THEN 
         CALL READ_PARAMETER(DEPTH_FILE, 'DEPTH_FILE')

      ELSE IF (DEPTH_TYPE(1:3)=='FLA') THEN

         !ADD units 
         CALL READ_PARAMETER(DEPTH_FLAT, 'DEPTH_FLAT', 10.0_SP, 'm')

      ELSE IF (DEPTH_TYPE(1:3)=='SLO') THEN

         CALL READ_PARAMETER(DEPTH_FLAT, 'DEPTH_FLAT', 10.0_SP, 'm')
         CALL READ_PARAMETER(SLP       , 'SLP'       , 0.1_SP      )
         CALL READ_PARAMETER(Xslp      , 'Xslp'      , 0.0_SP , 'm')

      ELSE
         CALL WRITE_STOP('INVALID DEPTH TYPE. STOP!!!')
      END IF 

      IF (BATHY_CORRECTION) CALL WRITE_ALL('Bathymetry is being corrected !')      

      CALL logbuff % FLUSH2OUTPUT()


      CALL WRITE_BANNER('TIME INFO')

      CALL READ_PARAMETER(TOTAL_TIME     , 'TOTAL_TIME', units='s')
      CALL READ_PARAMETER(PLOT_START_TIME, 'PLOT_START_TIME', 0.0_SP, 's')
      CALL READ_PARAMETER(PLOT_INTV      , 'PLOT_INTV'      , 1.0_SP, 's')

      ! Move to stations section 

      CALL READ_PARAMETER(PLOT_INTV_STATION, 'PLOT_INTV_STATION'  , 1.0_SP, 's')
      CALL READ_PARAMETER(StationOutputBuffer, 'StationOutputBuffer', 1000)

      CALL READ_PARAMETER(SCREEN_INTV, 'SCREEN_INTV', 1.0_SP, 's')
     
      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('HOT START INFO ')
      
      CALL READ_PARAMETER(INI_UVZ, 'INI_UVZ', .FALSE. )

      IF (INI_UVZ) THEN

         CALL READ_PARAMETER(BED_DEFORMATION, 'BED_DEFORMATION', .FALSE.)

         IF (BED_DEFORMATION) CALL WRITE_ALL('Bathymetry is adjusted based on Bed deformation.')

         CALL READ_PARAMETER(ETA_FILE, 'ETA_FILE')

         ! Hacky solutions, does not all files called ZERO
         CALL READ_PARAMETER(U_FILE, 'U_FILE', 'ZERO')
         NO_UV_FILE = U_FILE.EQ.'ZERO'

         CALL READ_PARAMETER(V_FILE, 'V_FILE', 'ZERO')
         ! NOTE: Check logic in orginial code 
         NO_UV_FILE = NO_UV_FILE.OR.(V_FILE.EQ.'ZERO')

         CALL READ_PARAMETER(MASK_FILE, 'MASK_FILE', 'NO_MASK')
         NO_MASK_FILE = MASK_FILE.EQ.'NO_MASK'
        
         CALL READ_PARAMETER(HotStartTime, 'HotStartTime'    , 0.0_SP, 's')
         CALL READ_PARAMETER(icount      ,'OutputStartNumber', 1)

         icount = icount-1
         CALL logbuff % FLUSH2OUTPUT()
      ELSE
         CALL logbuff % CLEAR() 
      END IF  ! end initial hot start file

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('STILL WATER INFO ')
      CALL READ_PARAMETER(WaterLevel, 'WaterLevel', 0.0_SP, 'm')
      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('WAVEMAKER INFO')
    
      read_check = .TRUE.
      CALL READ_PARAMETER(WaveMaker, 'WAVEMAKER', check=read_check)

      IF (.NOT.read_check) THEN

         CALL WRITE_ALL('No Wavemaker')

      ELSE IF (WaveMaker(1:7)=='LEF_SOL') THEN

         CALL READ_PARAMETER(AMP_SOLI, 'AMP')     
         CALL READ_PARAMETER(DEP_SOLI, 'DEP')
         CALL READ_PARAMETER(LAG_SOLI, 'LAGTIME', 0.0_SP, 's')

      ELSE IF (WaveMaker(1:7)=='WK_TIME') THEN

         CALL READ_PARAMETER(NumWaveComp , 'NumWaveComp'  )
         CALL READ_PARAMETER(PeakPeriod  , 'PeakPeriod'   )
         CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile' )
         CALL READ_PARAMETER(Xc_WK       , 'Xc_WK'        )
         CALL READ_PARAMETER(Yc_WK       , 'Yc_WK'        , 0.0_SP, 'm')
         CALL READ_PARAMETER(DEP_WK      , 'DEP_WK'       )
         CALL READ_PARAMETER(Time_ramp   , 'Time_ramp'    , 0.0_SP, 's')
         CALL READ_PARAMETER(Delta_WK    , 'Delta_WK'     , 0.5_SP)
         CALL READ_PARAMETER(Ywidth_WK   , 'Ywidth_WK'    , LARGE)

      ELSE IF (WaveMaker(1:7)=='INI_SOL') THEN

         CALL READ_PARAMETER(SolitaryPositiveDirection, 'SolitaryPositiveDirection', .TRUE.)

         IF (SolitaryPositiveDirection) THEN
            CALL WRITE_ALL('Solitary wave propagate in + X direction')
         ELSE
            CALL WRITE_ALL('Solitary wave propagate in - X direction')
         END IF

         CALL READ_PARAMETER(AMP_SOLI  , 'AMP')
         CALL READ_PARAMETER(DEP_SOLI  , 'DEP')
         CALL READ_PARAMETER(XWAVEMAKER, 'XWAVEMAKER')

      ELSE IF (WaveMaker(1:6)=='N_WAVE') THEN

         CALL READ_PARAMETER(x1_Nwave   , 'x1_Nwave')
         CALL READ_PARAMETER(x2_Nwave   , 'x2_Nwave')
         CALL READ_PARAMETER(a0_Nwave   , 'a0_Nwave')
         CALL READ_PARAMETER(gamma_Nwave, 'gamma_Nwave')
         CALL READ_PARAMETER(dep_Nwave  , 'dep_Nwave')

      ELSE IF (WaveMaker(1:7)=='INI_REC') THEN

         CALL READ_PARAMETER(AMP_SOLI, 'AMP')
         CALL READ_PARAMETER(Xc      , 'Xc')
         CALL READ_PARAMETER(Yc      , 'Yc', 0.0_SP, 'm')
         CALL READ_PARAMETER(WID     , 'WID' )

      ELSE IF (WaveMaker(1:7)=='INI_GAU'.OR.&
               WaveMaker(1:7)=='INI_DIP') THEN

         CALL READ_PARAMETER(AMP_SOLI, 'AMP')
         CALL READ_PARAMETER(Xc      , 'Xc')
         CALL READ_PARAMETER(Yc      , 'Yc')
         CALL READ_PARAMETER(WID     , 'WID' )

      ELSE IF (WaveMaker(1:6)=='WK_REG') THEN

         CALL READ_PARAMETER(Xc_WK    , 'Xc_WK')
         CALL READ_PARAMETER(Yc_WK    , 'Yc_WK'   , 0.0_SP, 'm')
         CALL READ_PARAMETER(Tperiod  , 'Tperiod')
         CALL READ_PARAMETER(AMP_WK   , 'AMP_WK')
         CALL READ_PARAMETER(DEP_WK   , 'DEP_WK')
         CALL READ_PARAMETER(Theta_WK , 'Theta_WK' , 0.0_SP)
         CALL READ_PARAMETER(Time_ramp, 'Time_ramp', 0.0_SP, 's')
         CALL READ_PARAMETER(Delta_WK , 'Delta_WK' , 0.5_SP)
         CALL READ_PARAMETER(Ywidth_WK, 'Ywidth_WK', LARGE)

      ELSE IF (WaveMaker(1:6)=='WK_IRR'.OR.WaveMaker(1:6)=='TMA_1D'.OR.&
               WaveMaker(1:6)=='JON_1D'.OR.WaveMaker(1:6)=='JON_2D') THEN

         CALL READ_PARAMETER(Xc_WK    , 'Xc_WK')
         CALL READ_PARAMETER(Yc_WK    , 'Yc_WK'   , 0.0_SP, 'm')
         CALL READ_PARAMETER(DEP_WK   , 'DEP_WK')
         CALL READ_PARAMETER(Time_ramp, 'Time_ramp', 0.0_SP, 's')
         CALL READ_PARAMETER(Delta_WK , 'Delta_WK' , 0.5_SP)
         CALL READ_PARAMETER(FreqPeak , 'FreqPeak')
         CALL READ_PARAMETER(FreqMin  , 'FreqMin')
         CALL READ_PARAMETER(FreqMax  , 'FreqMax')
         CALL READ_PARAMETER(Hmo      , 'Hmo')
         CALL READ_PARAMETER(Nfreq    , 'Nfreq', 45 )

         IF (WaveMaker(5:6)=='1D') THEN
            Ntheta = 1
            ThetaPeak = 0.0_SP
         ELSE
            CALL READ_PARAMETER(Ntheta     , 'Ntheta'     , 24)
            CALL READ_PARAMETER(ThetaPeak  , 'ThetaPeak'  , 0.0_SP)
            CALL READ_PARAMETER(Sigma_Theta, 'Sigma_Theta', 10.0_SP)
         END IF
         
         CALL READ_PARAMETER(Ywidth_WK  , 'Ywidth_WK'  , LARGE)
         CALL READ_PARAMETER(EqualEnergy, 'EqualEnergy', .FALSE.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

      ELSE IF (WaveMaker(1:13)=='WK_NEW_DATA2D'.OR.&
               WaveMaker(1:9)=='WK_DATA2D') THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         CALL READ_PARAMETER(Xc_WK       , 'Xc_WK')
         CALL READ_PARAMETER(Yc_WK       , 'Yc_WK'       , 0.0_SP, 'm')
         CALL READ_PARAMETER(DEP_WK      , 'DEP_WK')
         CALL READ_PARAMETER(Time_ramp   , 'Time_ramp'   , 0.0_SP, 's')
         CALL READ_PARAMETER(Delta_WK    , 'Delta_WK'    , 0.5_SP)
         CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile')
         CALL READ_PARAMETER(Ywidth_WK   , 'Ywidth_WK'   , LARGE)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! absorbing generating wavemaker, more tests needed 
      ELSE IF (WaveMaker(1:3) =='ABS'.OR. &
               WaveMaker(1:11)=='LEFT_BC_IRR') THEN

         CALL READ_PARAMETER(WAVE_DATA_TYPE, 'WAVE_DATA_TYPE', 'NONE')

         read_check = .TRUE. 
         CALL READ_PARAMETER(DEP_Ser, 'DepthWaveMaker', check=read_check)

         IF (.NOT.read_check) THEN

            CALL WRITE_ALL('DepthWaveMaker not defined, reading DEP_WK')
            read_check = .TRUE.
            CALL READ_PARAMETER(DEP_Ser, 'DEP_WK', check=read_check)
            IF (.NOT.read_check) THEN
               CALL WRITE_STOP('Neither DepthWaveMaker or DEP_WK specified. STOP!!!')
            END IF

         END IF

         IF (WAVE_DATA_TYPE(1:3)=='ABS') THEN

            CALL READ_PARAMETER(WidthWaveMaker    , 'WidthWaveMaker')
            CALL READ_PARAMETER(R_sponge_wavemaker, 'R_sponge_wavemaker')
            CALL READ_PARAMETER(A_sponge_wavemaker, 'A_sponge_wavemaker')

         ELSE IF (WAVE_DATA_TYPE(1:4)=='DATA') THEN
            CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile')
  
            OPEN(1,FILE=TRIM(WaveCompFile))
            READ(1,*) NumFreq, NumDir

            ALLOCATE (Amp_Ser(NumFreq,NumDir),  &
                       Per_Ser(NumFreq),Theta_Ser(NumDir),Phase_LEFT(NumFreq,NumDir))

            CALL logbuf % ADD('NumFreq', FORMAT_INTEGER(NumFreq))
            CALL logbuf % ADD('NumDir' , FORMAT_INTEGER(NumDir))

            READ(1,*)PeakPeriod  ! useless for this application but should keep for consistency
            DO J=1,NumFreq
               READ(1,*)Per_Ser(J)  ! read in as frequency
               !print*,J,Per_Ser(J)
             ENDDO
             DO I=1,NumDir
                 READ(1,*)Theta_Ser(I)
             ENDDO
             DO I=1,NumDir
                READ(1,*)(Amp_Ser(J,I),J=1,NumFreq)
                !print*,I,Amp_Ser(J,I)
             ENDDO
             DO I=1,NumDir
                READ(1,*,END=991)(Phase_LEFT(J,I),J=1,NumFreq)
             ENDDOVE_DATA_TYPE
             CLOSE(1)
881          INPUT_PHASE = .TRUE.
991          CONTINUE

             DO J=1,NumFreq
                DO I=1,NumDir
                   IF(INPUT_PHASE)THEN
                      Phase_LEFT(J,I)=Phase_LEFT(J,I)*3.1415926/180.0_SP
                   ELSE
# if defined (INTEL)
                      Phase_LEFT(J,I)=rand()*2.0_SP*3.1415926
# elif defined (CRAY)
                      Phase_LEFT(J,I)=rand()*2.0_SP*3.1415926
# else
                      Phase_LEFT(J,I)=rand(0)*2.0_SP*3.1415926
# endif
                   ENDIF
                ENDDO
             ENDDO

! to make consistent with cm and sm approach we use phase_ser which is 
! only is random with frequency
             ALLOCATE(Phase_Ser(NumFreq))
             ALLOCATE(Segma_Ser(NumFreq),Wave_Number_Ser(NumFreq) )
             DO J=1,NumFreq
                Phase_Ser(J)=Phase_LEFT(J,1)
                IF(Per_Ser(J).EQ.ZERO)THEN
                   CALL WRITE_STOP('wave frequency is zero, STOP!!!')
                   STOP
                ELSE
                   Per_Ser(J)=1.0_SP/Per_Ser(J)
                ENDIF
             ENDDO
             DO I=1,NumDir
                Theta_Ser(I)=Theta_Ser(I)*DEG2RAD
             ENDDO
             CALL WRITE_ALL('Using absorbing generating wave maker with DATA')

         ELSE IF (WAVE_DATA_TYPE(1:6)=='WK_IRR'.OR.WAVE_DATA_TYPE(1:6)=='TMA_1D'.OR.&
                  WAVE_DATA_TYPE(1:6)=='JON_1D'.OR.WAVE_DATA_TYPE(1:6)=='JON_2D') THEN 

            CALL READ_PARAMETER(FreqPeak , 'FreqPeak')
            CALL READ_PARAMETER(FreqMin  , 'FreqMin')
            CALL READ_PARAMETER(FreqMax  , 'FreqMax')
            CALL READ_PARAMETER(Hmo      , 'Hmo')
            CALL READ_PARAMETER(Nfreq    , 'Nfreq', 45 )
            CALL READ_PARAMETER(GammaTMA , 'GammaTMA', 3.3_SP )

            IF (WAVE_DATA_TYPE(5:6)=='1D') THEN
               Ntheta = 1
               ThetaPeak = 0.0_SP
            ELSE
               CALL READ_PARAMETER(Ntheta     , 'Ntheta'     , 24)
               CALL READ_PARAMETER(ThetaPeak  , 'ThetaPeak'  , 0.0_SP)
               CALL READ_PARAMETER(Sigma_Theta, 'Sigma_Theta', 10.0_SP)
            END IF

         ELSE
            CALL WRITE_STOP("INVALID WAVE_DATA_TYPE SPECIFIED. STOP!!!")
         END IF 

      ELSE
         CALL WRITE_STOP("INVALID WAVE MAKER SPECIFIED. STOP!!!")
      END IF

      IF (WaveMaker(1:4).NE.'NONE') THEN

         CALL READ_PARAMETER(ETA_LIMITER, 'ETA_LIMITER', .FALSE.)
         IF (ETA_LIMITER) THEN
            CALL READ_PARAMETER(CrestLimit , 'CrestLimit')
            CALL READ_PARAMETER(TroughLimit, 'TroughLimit')
         END IF 

! to avoid longshore current caused by extra momentum flux
! we can add bottom friction make momentum balance

         read_check = .TRUE.
         CALL READ_PARAMETER(WaveMakerCd, 'WaveMakerCd', check=read_check)
         WaveMakerCurrentBalance = read_check

      END IF

      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

# if defined (CARTESIAN)
      CALL WRITE_BANNER('PERIODIC BC INFO')
      CALL READ_PARAMETER(PERIODIC, 'PERIODIC', .FALSE.)
      CALL logbuff % FLUSH2OUTPUT()
# endif      

! -----------------------------------------------------------------------------


      CALL WRITE_BANNER('SPONGE INFO')

      CALL READ_PARAMETER(DIRECT_SPONGE, 'DIRECT_SPONGE', .FALSE.)

      IF (DIRECT_SPONGE) THEN
         CALL WRITE_ALL('Direct sponge used')
      END IF

      CALL READ_PARAMETER(DIFFUSION_SPONGE, 'DIFFUSION_SPONGE', .FALSE.)

      IF (DIFFUSION_SPONGE) THEN         
         CALL WRITE_ALL('Diffusion sponge used')
         CALL READ_PARAMETER(Csp, 'Csp', 0.1_SP)
      END IF 

      CALL READ_PARAMETER(FRICTION_SPONGE, 'FRICTION_SPONGE', .FALSE.)
      
      IF (FRICTION_SPONGE) THEN
         CALL WRITE_ALL('Diffusion sponge used')
         CALL READ_PARAMETER(CDsponge, 'CDsponge', 5.0_SP)
      END IF

      IF (DIFFUSION_SPONGE.OR.DIRECT_SPONGE.OR.FRICTION_SPONGE) THEN
         CALL READ_PARAMETER(Sponge_west_width , 'Sponge_west_width' , 0.0_SP , 'm')
         CALL READ_PARAMETER(Sponge_east_width , 'Sponge_east_width' , 0.0_SP , 'm')
         CALL READ_PARAMETER(Sponge_south_width, 'Sponge_south_width', 0.0_SP , 'm')
         CALL READ_PARAMETER(Sponge_north_width, 'Sponge_north_width', 0.0_SP , 'm')
         CALL READ_PARAMETER(R_sponge          , 'R_sponge'          , 0.85_SP)
         CALL READ_PARAMETER(A_sponge          , 'A_sponge'          , 5.0_SP)
      END IF 

      CALL logbuff % FLUSH2OUTPUT()
 
! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('OBSTACLE and BREAKWATER INFO')

      read_check = .TRUE.
      CALL READ_READ_PARAMETER(OBSTACLE_FILE, 'OBSTACLE_FILE', check=read_check)
      OBSTACLE = read_check
      IF (.NOT.OBSTACLE) CALL WRITE_ALL( 'No obstacle file.')
    
      read_check = .TRUE.
      CALL READ_READ_PARAMETER(BREAKWATER_FILE, 'BREAKWATER_FILE', check=read_check)
      BREAKWATER = read_check

      IF (.NOT.BREAKWATER) THEN
         CALL WRITE_ALL( 'No breakwater file.')      
      ELSE
         CALL READ_PARAMETER(BreakWaterAbsorbCoef, 'BreakWaterAbsorbCoef', 10.0_SP)
      END IF

      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('PHYSIC INFO')

      CALL READ_PARAMETER(DISPERSION, 'DISPERSION',.TRUE.)
      CALL READ_PARAMETER(Gamma1    , 'Gamma1'    , 1.0_SP)
# if defined (CARTESIAN)
      CALL READ_PARAMETER(Gamma2    , 'Gamma2'    , 1.0_SP)
      CALL READ_PARAMETER(Beta_ref  , 'Beta_ref'  , -0.531_SP)
      CALL READ_PARAMETER(Gamma3    , 'Gamma3'    , 1.0_SP)
# else
# if define (ZALPHA)
      CALL READ_PARAMETER(Beta_ref  , 'Beta_ref'  , -0.531_SP)
# endif 
# endif 


      CALL READ_PARAMETER(VISCOSITY_BREAKING, 'VISCOSITY_BREAKING', .TRUE.)


! Move to breaking

      IF(ROLLER) VISCOSITY_BREAKING = .TRUE.

      IF (VISCOSITY_BREAKING) CALL WRITE_ALL('Viscosity breaking used.')

      CALL READ_PARAMETER(SWE_ETA_DEP, 'SWE_ETA_DEP', 0.8_SP)

      ! Check depends on VISCOSITY_BREAKING

      CALL READ_PARAMETER(In_Cd, 'FRICTION_MATRIX', .FALSE.)

      IF (IN_Cd) THEN
         CALL READ_PARAMETER(CD_FILE, 'FRICTION_FILE')
      ELSE
         CALL READ_PARAMETER(Cd_fixed, 'Cd', 0.0_SP)
      END IF 

      CALL logbuff % FLUSH2OUTPUT()

! -----------------------------------------------------------------------------

      CALL WRITE_BANNER('NUMERICS INFO')

      CALL READ_PARAMETER(Time_Scheme, 'Time_Scheme', 'Runge_Kutta')

      IF ((Time_Scheme.NE.'Predictor_Corrector').AND.&
          (Time_Scheme.NE.'Runge_Kutta') THEN
         CALL WRITE_STOP('INVALID TIME SCHEME OPTION. STOP!!!')
      END IF


      CALL READ_PARAMETER(CONSTR, 'CONSTRUCTION', 'HLLC')

      ! Check options, seems second/third order was deprecated + other options
      CALL READ_PARAMETER(HIGH_ORDER, 'HIGH_ORDER', 'FOURTH')

      !IF ((HIGH_ORDER.NE.'FOURTH').AND.&
      !    (HIGH_ORDER.NE.'THIRD').AND.&
      !    (HIGH_ORDER.NE.'SECOND')) THEN
      !   CALL WRITE_STOP('INVALID SCHEME ORDER OPTION. STOP!!!')
      !END IF

      CALL READ_PARAMETER(CFL, 'CFL', 0.5_SP)

      read_check=.FALSE.
      CALL READ_PARAMETER(DT_fixed, 'DT_fixed', check=read_check)
      FIXED_DT = read_check
      IF (FIXED_DT) THEN 
         CALL logbuf % ADD('FIXED_DT', FORMAT_FLOAT(FIXED_DT))
         CALL WRITE_ALL('Used fixed DT, but judged by CFL. IF not satisfy CLF, DT/2...')
      END IF       
 
      CALL READ_PARAMETER(FroudeCap  , 'FroudeCap'  , 3.0_SP)
      CALL READ_PARAMETER(MinDepth   , 'MinDepth'   , 0.1_SP, 'm')  
      CALL READ_PARAMETER(MinDepthFrc,' MinDepthFrc', 0.1_SP, 'm')


      CALL READ_FLOAT(MinDepthFrc,FILE_NAME,'MinDepthFrc',ierr)
      IF(ierr==1)THEN
        MinDepthFrc = 0.1_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'MinDepthFrc Default:  0.1 m'
         WRITE(3,'(A40)')'MinDepthFrc Default:  0.1 m'
      endif
# else
         WRITE(*,'(A40)')'MinDepthFrc Default:  0.1 m'
         WRITE(3,'(A40)')'MinDepthFrc Default:  0.1 m'
# endif
       ENDIF

!  merge two parameters into the minimum one
       MinDepthFrc=MAX(MinDepthFrc,MinDepth)
       MinDepth=MinDepthFrc

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A40)')'USE MIN(MinDepthFrc, MinDepth)'
# else
      WRITE(3,'(A40)')'USE MIN(MinDepthFrc, MinDepth)'
# endif


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# else
      WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# endif

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.6)')'MinDepthFrc=', MinDepthFrc
# else
      WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc
# endif

! Lauren - Arrival Time - Wave height threshold (in m) to pick up arrival time

      CALL READ_LOGICAL(OUT_Time,FILE_NAME,'OUT_Time',ierr)
      IF(ierr==1)THEN
        OUT_Time = .FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A60)')'Dont record wave arrival time'
         WRITE(3,'(A60)')'Dont record wave arrival time'
      endif
# else
         WRITE(*,'(A60)')'Dont record wave arrival time'
         WRITE(3,'(A60)')'Dont record wave arrival time'
# endif
       ELSE
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A60)')'Record wave arrival time'
         WRITE(3,'(A60)')'Record wave arrival time'
      endif
# else
         WRITE(*,'(A60)')'Record wave arrival time'
         WRITE(3,'(A60)')'Record wave arrival time'
# endif
       ENDIF

     IF(OUT_Time)THEN
      CALL READ_FLOAT(ArrTimeMin,FILE_NAME,'ArrTimeMinH',ierr)
      IF(ierr==1)THEN
        ArrTimeMin = 0.001 ! set equal to 0.1 cm if no threshold is given
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'ArrTimeMinH Default:  0.001 m'
         WRITE(3,'(A40)')'ArrTimeMinH Default:  0.001 m'
      endif
# else
         WRITE(*,'(A40)')'ArrTimeMinH Default:  0.001 m'
         WRITE(3,'(A40)')'ArrTimeMinH Default:  0.001 m'
# endif
       ENDIF
       
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'ArrTimeMinH', ArrTimeMin
# else
      WRITE(3,'(A10,F12.6)')'ArrTimeMinH=', ArrTimeMin
# endif
     ENDIF
 
! end Laurens modification

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'--------- WAVE BREAKING -----------------'
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)'-------- WAVE BREAKING -----------------'   
# endif

! roller

      CALL READ_LOGICAL(ROLLER,FILE_NAME,'ROLLER_EFFECT',ierr)
      IF(ierr==1)THEN
        ROLLER = .FALSE.
      ENDIF

       IF(ROLLER)THEN
         ROLLER_SWITCH = 1.0_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'ROLLER_EFFECT:  INCLUDED'
         WRITE(3,'(A40)')'ROLLER_EFFECT:  INCLUDED'
      endif
# else
         WRITE(*,'(A40)')'ROLLER_EFFECT:  INCLUDED'
         WRITE(3,'(A40)')'ROLLER_EFFECT:  INCLUDED'
# endif
       ELSE
         ROLLER_SWITCH = ZERO
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'ROLLER_EFFECT:  NO'
         WRITE(3,'(A40)')'ROLLER_EFFECT:  NO'
      endif
# else
         WRITE(*,'(A40)')'ROLLER_EFFECT:  NO'
         WRITE(3,'(A40)')'ROLLER_EFFECT:  NO'
# endif
       ENDIF

! end roller

! show breaking
      CALL READ_LOGICAL(SHOW_BREAKING,FILE_NAME,'SHOW_BREAKING',ierr)
      IF(ierr==1)THEN
        SHOW_BREAKING = .TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'SHOW_BREAKING Default:  TRUE'
         WRITE(3,'(A40)')'SHOW_BREAKING Default:  TRUE'
      endif
# else
         WRITE(*,'(A40)')'SHOW_BREAKING Default:  TRUE'
         WRITE(3,'(A40)')'SHOW_BREAKING Default:  TRUE'
# endif
       ENDIF
	
      IF(VISCOSITY_BREAKING) SHOW_BREAKING = .TRUE.

      IF(SHOW_BREAKING)THEN
      CALL READ_FLOAT(Cbrk1,FILE_NAME,'Cbrk1',ierr)
      IF(ierr==1)THEN
        Cbrk1 = 0.65_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'Cbrk1 Default:  0.65'
         WRITE(3,'(A40)')'Cbrk1 Default:  0.65'
      endif
# else
         WRITE(*,'(A40)')'Cbrk1 Default:  0.65'
         WRITE(3,'(A40)')'Cbrk1 Default:  0.65'
# endif
       ENDIF

      IF(VISCOSITY_BREAKING)THEN
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# else
      WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# endif
      ENDIF

      CALL READ_FLOAT(Cbrk2,FILE_NAME,'Cbrk2',ierr)
      IF(ierr==1)THEN
        Cbrk2 = 0.35_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'Cbrk2 Default:  0.35'
         WRITE(3,'(A40)')'Cbrk2 Default:  0.35'
      endif
# else
         WRITE(*,'(A40)')'Cbrk2 Default:  0.35'
         WRITE(3,'(A40)')'Cbrk2 Default:  0.35'
# endif
       ENDIF

      IF(VISCOSITY_BREAKING)THEN
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# else
      WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# endif
      ENDIF

      CALL READ_FLOAT(WAVEMAKER_Cbrk,FILE_NAME,'WAVEMAKER_Cbrk',ierr)
      IF(ierr==1)THEN
        WAVEMAKER_Cbrk = 1.0_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'WAVEMAKER_Cbrk Default:  1.0'
         WRITE(3,'(A40)')'WAVEMAKER_Cbrk Default:  1.0'
      endif
# else
         WRITE(*,'(A40)')'WAVEMAKER_Cbrk Default:  1.0'
         WRITE(3,'(A40)')'WAVEMAKER_Cbrk Default:  1.0'
# endif
       ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A18,F17.6)')'WAVEMAKER_Cbrk =', WAVEMAKER_Cbrk
# else
      WRITE(3,'(A18,F17.6)')'WAVEMAKER_Cbrk =', WAVEMAKER_Cbrk
# endif
      ENDIF

	![ykchoi(08.18.2015) : for viscosity of wavemaker
      CALL READ_LOGICAL(WAVEMAKER_VIS,FILE_NAME,'WAVEMAKER_VIS',ierr)  
      IF(ierr==1)THEN
        WAVEMAKER_VIS = .FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'WAVEMAKER_VIS Default:  FALSE'
         WRITE(3,'(A40)')'WAVEMAKER_VIS Default:  FALSE'
      endif
# else
         WRITE(*,'(A40)')'WAVEMAKER_VIS Default:  FALSE'
         WRITE(3,'(A40)')'WAVEMAKER_VIS Default:  FALSE'
# endif
       ENDIF
	
	IF( VISCOSITY_BREAKING .AND. WAVEMAKER_VIS ) THEN
# if defined (PARALLEL)
	  IF (myid.eq.0) then
	     WRITE(*,*) "==============================================="
	     WRITE(*,*)  "STOP :: VISCOSITY_BREAKING=T, WAVEMAKER_VIS=T"
	     WRITE(*,*) "==============================================="
          ENDIF
          call MPI_FINALIZE ( ier )
# else
	     WRITE(*,*) "==============================================="
	     WRITE(*,*)  "STOP :: VISCOSITY_BREAKING=T, WAVEMAKER_VIS=T"
	     WRITE(*,*) "==============================================="
	  STOP
# endif
      ENDIF

      IF(WAVEMAKER_VIS)THEN
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,*)'WAVEMAKER_VIS'
# else
       WRITE(3,*)'WAVEMAKER_VIS'
# endif    
      ENDIF

!  suggest dont use wavemaker_vis 04/30
      IF(WAVEMAKER_VIS)THEN
      CALL READ_FLOAT(visbrk,FILE_NAME,'visbrk',ierr)
	CALL READ_FLOAT(WAVEMAKER_visbrk,FILE_NAME,'WAVEMAKER_visbrk',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'visbrk =', visbrk
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'WAVEMAKER_visbrk =', WAVEMAKER_visbrk
# else
      WRITE(3,'(A10,F12.6)')'visbrk =', visbrk
      WRITE(3,'(A20,F12.6)')'WAVEMAKER_visbrk =', WAVEMAKER_visbrk
# endif
	ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'-------- WAVE-AVERAGED PROPERTY -----------------'
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)'------- WAVE-AVERAGED PROPERTY -----------------'   
# endif

      CALL READ_FLOAT(T_INTV_mean,FILE_NAME,'T_INTV_mean',ierr)
      IF(ierr==1)THEN
        T_INTV_mean = LARGE
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'T_INTV_mean Default:  LARGE'
         WRITE(3,'(A40)')'T_INTV_mean Default:  LARGE'
      endif
# else
         WRITE(*,'(A40)')'T_INTV_mean Default:  LARGE'
         WRITE(3,'(A40)')'T_INTV_mean Default:  LARGE'
# endif
       ENDIF

	CALL READ_FLOAT(STEADY_TIME,FILE_NAME,'STEADY_TIME',ierr)
      IF(ierr==1)THEN
        STEADY_TIME = LARGE
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'STEADY_TIME Default:  LARGE'
         WRITE(3,'(A40)')'STEADY_TIME Default:  LARGE'
      endif
# else
         WRITE(*,'(A40)')'STEADY_TIME Default:  LARGE'
         WRITE(3,'(A40)')'STEADY_TIME Default:  LARGE'
# endif
       ENDIF

      CALL READ_FLOAT(C_smg,FILE_NAME,'C_smg',ierr)
      IF(ierr==1)THEN
        C_smg = 0.0_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'C_smg Default:  0.0'
         WRITE(3,'(A40)')'C_smg Default:  0.0'
      endif
# else
         WRITE(*,'(A40)')'C_smg Default:  0.0'
         WRITE(3,'(A40)')'C_smg Default:  0.0'
# endif
       ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'STEADY_TIME =', STEADY_TIME
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# else
      WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      WRITE(3,'(A14,F12.6)')'STEADY_TIME =', STEADY_TIME
      WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# endif
	
      CALL READ_FLOAT(nu_bkg,FILE_NAME,'nu_bkg',ierr)
      IF(ierr==1)THEN
        nu_bkg = 0.0_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'nu_bkg Default:  0.0'
         WRITE(3,'(A40)')'nu_bkg Default:  0.0'
      endif
# else
         WRITE(*,'(A40)')'nu_bkg Default:  0.0'
         WRITE(3,'(A40)')'nu_bkg Default:  0.0'
# endif
       ENDIF

# if defined (COUPLING)

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'-------------- COUPLING -----------------'
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)'-------------- COUPLING -----------------'   
# endif

      CALL READ_STRING(COUPLING_FILE,FILE_NAME,'COUPLING_FILE',ierr)
      IF(ierr==1)THEN
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A80)')'You use coupling option, specify COUPLING_FILE in input.txt'
         WRITE(3,'(A80)')'You use coupling option, specify COUPLING_FILE in input.txt'
      endif
       call MPI_FINALIZE ( ier )
# else
         WRITE(*,'(A80)')'You use coupling option, specify COUPLING_FILE in input.txt'
         WRITE(3,'(A80)')'You use coupling option, specify COUPLING_FILE in input.txt'
# endif
        STOP
      ENDIF


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# else
      WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# endif
# endif
  ! end coupling file


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)'-------------- OUTPUT INFO -----------------'
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)'-------------- OUTPUT INFO -----------------'   
# endif

! result folder
      CALL READ_STRING(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',ierr)
      IF(ierr==1)THEN
        RESULT_FOLDER = './output/'
      ENDIF

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# else
      WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# endif

![mayhl 17/06/12
! Note 1: Serial code only output is ASCII
! Note 2: Placing code after result folder creation seems to delay
!         writing to LOG.txt till after simulation is completed.
# if defined (PARALLEL)
      CALL READ_STRING(FIELD_IO_TYPE,FILE_NAME,'FIELD_IO_TYPE',ierr)

      IF(ierr.EQ.1) FIELD_IO_TYPE = 'ASCII'
      
      IF (myid.EQ.0) WRITE(3,*) 'FIELD_IO_TYPE = ' , FIELD_IO_TYPE
# endif
! mayhl]
 


! create result folder
      MKFOLDER = "mkdir -p "//TRIM(RESULT_FOLDER)
# if defined (PARALLEL)
      IF (myid.eq.0) THEN
# if defined (INTEL)
        RES = SYSTEM(TRIM(MKFOLDER))
# else
        CALL SYSTEM(TRIM(MKFOLDER))
# endif
      ENDIF
# else
# if defined(INTEL)
      RES = SYSTEM(TRIM(MKFOLDER))
# else
      CALL SYSTEM(TRIM(MKFOLDER))
# endif
# endif

! station files
      CALL READ_INTEGER(NumberStations,FILE_NAME,'NumberStations',ierr)
      IF(NumberStations>0)THEN
      CALL READ_STRING(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',ierr)
      ENDIF

! output parameters
      CALL READ_INTEGER(OUTPUT_RES,FILE_NAME,'OUTPUT_RES',ierr)
      IF(ierr==1)THEN
        OUTPUT_RES = 1
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A60)')'OUTPUT_RES NOT FOUND, OUTPUT_RES=1: full resolution'
         WRITE(3,'(A60)')'OUTPUT_RES NOT FOUND, OUTPUT_RES=1: full resolution'
      endif
# else
         WRITE(*,'(A60)')'OUTPUT_RES NOT FOUND, OUTPUT_RES=1: full resolution'
         WRITE(3,'(A60)')'OUTPUT_RES NOT FOUND, OUTPUT_RES=1: full resolution'
# endif
       ENDIF

# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A15,I10)')'OUTPUT_RES',OUTPUT_RES
# else
       WRITE(3,'(A15,I10)')'OUTPUT_RES',OUTPUT_RES
# endif 

      CALL READ_LOGICAL(OUT_DEPTH,FILE_NAME,'DEPTH_OUT',ierr)
      CALL READ_LOGICAL(OUT_U,FILE_NAME,'U',ierr)
      CALL READ_LOGICAL(OUT_V,FILE_NAME,'V',ierr)
      CALL READ_LOGICAL(OUT_ETA,FILE_NAME,'ETA',ierr)
# if defined(UseEtaScreen)
      CALL READ_LOGICAL(OUT_EtaScreen,FILE_NAME,'ETAscreen',ierr)
# endif
      CALL READ_LOGICAL(OUT_Hmax,FILE_NAME,'Hmax',ierr)
      CALL READ_LOGICAL(OUT_Hmin,FILE_NAME,'Hmin',ierr)
      CALL READ_LOGICAL(OUT_Umax,FILE_NAME,'Umax',ierr)
      CALL READ_LOGICAL(OUT_MFmax,FILE_NAME,'MFmax',ierr)
      CALL READ_LOGICAL(OUT_VORmax,FILE_NAME,'VORmax',ierr)
      CALL READ_LOGICAL(OUT_MASK,FILE_NAME,'MASK',ierr)
      CALL READ_LOGICAL(OUT_MASK9,FILE_NAME,'MASK9',ierr)
      CALL READ_LOGICAL(OUT_Umean,FILE_NAME,'Umean',ierr)
      CALL READ_LOGICAL(OUT_Vmean,FILE_NAME,'Vmean',ierr)
      CALL READ_LOGICAL(OUT_ETAmean,FILE_NAME,'ETAmean',ierr)
      CALL READ_LOGICAL(OUT_WaveHeight,FILE_NAME,'WaveHeight',ierr)
      CALL READ_LOGICAL(OUT_SXL,FILE_NAME,'SXL',ierr)
      CALL READ_LOGICAL(OUT_SXR,FILE_NAME,'SXR',ierr)
      CALL READ_LOGICAL(OUT_SYL,FILE_NAME,'SYL',ierr)
      CALL READ_LOGICAL(OUT_SYR,FILE_NAME,'SYR',ierr)
      CALL READ_LOGICAL(OUT_SourceX,FILE_NAME,'SourceX',ierr)
      CALL READ_LOGICAL(OUT_SourceY,FILE_NAME,'SourceY',ierr)
      CALL READ_LOGICAL(OUT_P,FILE_NAME,'P',ierr)
      CALL READ_LOGICAL(OUT_Q,FILE_NAME,'Q',ierr)
      CALL READ_LOGICAL(OUT_Fx,FILE_NAME,'Fx',ierr)
      CALL READ_LOGICAL(OUT_Fy,FILE_NAME,'Fy',ierr)
      CALL READ_LOGICAL(OUT_Gx,FILE_NAME,'Gx',ierr)
      CALL READ_LOGICAL(OUT_Gy,FILE_NAME,'Gy',ierr)
      CALL READ_LOGICAL(OUT_AGE,FILE_NAME,'AGE',ierr)
      CALL READ_LOGICAL(OUT_ROLLER,FILE_NAME,'ROLLER',ierr)
      CALL READ_LOGICAL(OUT_UNDERTOW,FILE_NAME,'UNDERTOW',ierr)
      CALL READ_LOGICAL(OUT_NU,FILE_NAME,'OUT_NU',ierr)
      CALL READ_LOGICAL(OUT_TMP,FILE_NAME,'TMP',ierr) 
      CALL READ_LOGICAL(OUT_Radiation,FILE_NAME,'Radiation',ierr) 

!ykchoi
!	CALL READ_FLOAT(EtaBlowVal,FILE_NAME,'EtaBlowVal',ierr)
      IF(ierr==1)THEN
        EtaBlowVal = 10.0_SP
# if defined (PARALLEL)
      if (myid.eq.0) THEN
         WRITE(*,'(A40)')'EtaBlowVal Default:  100xmax_depth'
         WRITE(3,'(A40)')'EtaBlowVal Default:  100xmax_depth'
      endif
# else
         WRITE(*,'(A40)')'EtaBlowVal Default:  100xmax_depth'
         WRITE(3,'(A40)')'EtaBlowVal Default:  100xmax_depth'
# endif
       ENDIF

!  fyshi set blowup value is 100xmax_depth in init.F  

# if defined (PARALLEL)
      if (myid.eq.0)   then
# endif
      WRITE(3,'(A15,L2)')'OUT_DEPTH',OUT_DEPTH
      WRITE(3,'(A15,L2)')'OUT_U',OUT_U
      WRITE(3,'(A15,L2)')'OUT_V',OUT_V
      WRITE(3,'(A15,L2)')'OUT_ETA',OUT_ETA
# if defined (UseEtaScreen)
      WRITE(3,'(A15,L2)')'OUT_ETAscreen',OUT_EtaScreen
# endif
      WRITE(3,'(A15,L2)')'OUT_Hmax',OUT_Hmax
      WRITE(3,'(A15,L2)')'OUT_Hmin',OUT_Hmin
      WRITE(3,'(A15,L2)')'OUT_Umax',OUT_Umax
      WRITE(3,'(A15,L2)')'OUT_MFmax',OUT_MFmax
      WRITE(3,'(A15,L2)')'OUT_VORmax',OUT_VORmax
      WRITE(3,'(A15,L2)')'OUT_MASK',OUT_MASK
      WRITE(3,'(A15,L2)')'OUT_MASK9',OUT_MASK9
      WRITE(3,'(A15,L2)')'OUT_Umean',OUT_Umean
      WRITE(3,'(A15,L2)')'OUT_Vmean',OUT_Vmean
      WRITE(3,'(A15,L2)')'OUT_ETAmean',OUT_ETAmean
      WRITE(3,'(A15,L2)')'OUT_WaveHeight',OUT_WaveHeight
      WRITE(3,'(A15,L2)')'OUT_SXL',OUT_SXL
      WRITE(3,'(A15,L2)')'OUT_SXR',OUT_SXR
      WRITE(3,'(A15,L2)')'OUT_SYL',OUT_SYL
      WRITE(3,'(A15,L2)')'OUT_SYR',OUT_SYR
      WRITE(3,'(A15,L2)')'OUT_SourceX',OUT_SourceX
      WRITE(3,'(A15,L2)')'OUT_SourceY',OUT_SourceY
      WRITE(3,'(A15,L2)')'OUT_P',OUT_P
      WRITE(3,'(A15,L2)')'OUT_Q',OUT_Q
      WRITE(3,'(A15,L2)')'OUT_Fx',OUT_Fx
      WRITE(3,'(A15,L2)')'OUT_Fy',OUT_Fy
      WRITE(3,'(A15,L2)')'OUT_Gx',OUT_Gx
      WRITE(3,'(A15,L2)')'OUT_Gy',OUT_Gy
      WRITE(3,'(A15,L2)')'OUT_AGE',OUT_AGE
      WRITE(3,'(A15,L2)')'OUT_ROLLER',OUT_ROLLER
      WRITE(3,'(A15,L2)')'OUT_UNDERTOW',OUT_UNDERTOW
      WRITE(3,'(A15,L2)')'OUT_NU',OUT_NU
      WRITE(3,'(A15,L2)')'OUT_TMP',OUT_TMP
      WRITE(3,'(A15,L2)')'OUT_TIME',OUT_Time
# if defined (PARALLEL)
      endif
# endif


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'                                         '
      if (myid.eq.0) WRITE(3,*)' --------------input end --------------' 
      if (myid.eq.0) WRITE(3,*)'                                         '
# else
      WRITE(3,*)'                                         '
      WRITE(3,*)' --------------input end --------------' 
      WRITE(3,*)'                                         '
# endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

    IF(WaveMaker(1:10)=='WK_NEW_IRR')THEN
        CALL READ_FLOAT(Xc_WK,FILE_NAME,'Xc_WK',ierr)
        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A60)')'Xc_WK NOT FOUND, specify Xc_WK in input.txt'
                WRITE(3,'(A60)')'Xc_WK NOT FOUND, specify Xc_WK in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A60)')'Xc_WK NOT FOUND, specify Xc_WK in input.txt'
            WRITE(3,'(A60)')'Xc_WK NOT FOUND, specify Xc_WK in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(Yc_WK,FILE_NAME,'Yc_WK',ierr)

        IF(ierr==1)THEN
            Yc_WK = ZERO
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A40)')'Yc_WK defalt: 0.0'
                WRITE(3,'(A40)')'Yc_WK defalt: 0.0'
            endif
# else
            WRITE(*,'(A40)')'Yc_WK defalt: 0.0'
            WRITE(3,'(A40)')'Yc_WK defalt: 0.0'
# endif
        ENDIF

        CALL READ_FLOAT(DEP_WK,FILE_NAME,'DEP_WK',ierr)

        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A60)')'DEP_WK NOT FOUND, specify DEP_WK in input.txt'
                WRITE(3,'(A60)')'DEP_WK NOT FOUND, specify DEP_WK in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A60)')'DEP_WK NOT FOUND, specify DEP_WK in input.txt'
            WRITE(3,'(A60)')'DEP_WK NOT FOUND, specify DEP_WK in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(Time_ramp,FILE_NAME,'Time_ramp',ierr)

        IF(ierr==1)THEN
            Time_ramp = 0.0_SP
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A40)')'Time_ramp Default:  0.0'
                WRITE(3,'(A40)')'Time_ramp Default:  0.0'
            endif
# else
            WRITE(*,'(A40)')'Time_ramp Default:  0.0'
            WRITE(3,'(A40)')'Time_ramp Default:  0.0'
# endif
        ENDIF

        CALL READ_FLOAT(Delta_WK,FILE_NAME,'Delta_WK',ierr)

        IF(ierr==1)THEN
            Delta_WK = 0.5_SP
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A40)')'Delta_WK Default:  0.5'
                WRITE(3,'(A40)')'Delta_WK Default:  0.5'
            endif
# else
            WRITE(*,'(A40)')'Delta_WK Default:  0.5'
            WRITE(3,'(A40)')'Delta_WK Default:  0.5'
# endif
        ENDIF

        CALL READ_FLOAT(FreqPeak,FILE_NAME,'FreqPeak',ierr)

        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A80)')'FreqPeak NOT FOUND, specify FreqPeak in input.txt'
                WRITE(3,'(A80)')'FreqPeak NOT FOUND, specify FreqPeak in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A80)')'FreqPeak NOT FOUND, specify FreqPeak in input.txt'
            WRITE(3,'(A80)')'FreqPeak NOT FOUND, specify FreqPeak in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(FreqMin,FILE_NAME,'FreqMin',ierr)

        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A80)')'FreqMin NOT FOUND, specify FreqMin in input.txt'
                WRITE(3,'(A80)')'FreqMin NOT FOUND, specify FreqMin in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A80)')'FreqMin NOT FOUND, specify FreqMin in input.txt'
            WRITE(3,'(A80)')'FreqMin NOT FOUND, specify FreqMin in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(FreqMax,FILE_NAME,'FreqMax',ierr)

        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A80)')'FreqMax NOT FOUND, specify FreqMax in input.txt'
                WRITE(3,'(A80)')'FreqMax NOT FOUND, specify FreqMax in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A80)')'FreqMax NOT FOUND, specify FreqMax in input.txt'
            WRITE(3,'(A80)')'FreqMax NOT FOUND, specify FreqMax in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(Hmo,FILE_NAME,'Hmo',ierr)

        IF(ierr==1)THEN
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A50)')'Hmo NOT FOUND, specify Hmo in input.txt'
                WRITE(3,'(A50)')'Hmo NOT FOUND, specify Hmo in input.txt'
            endif
            call MPI_FINALIZE ( ier )
# else
            WRITE(*,'(A50)')'Hmo NOT FOUND, specify Hmo in input.txt'
            WRITE(3,'(A50)')'Hmo NOT FOUND, specify Hmo in input.txt'
# endif
            STOP
        ENDIF

        CALL READ_FLOAT(GammaTMA,FILE_NAME,'GammaTMA',ierr)

        IF(ierr==1)THEN
            GammaTMA = 3.3
# if defined (PARALLEL)
            if (myid.eq.0) WRITE(3,'(A14,A50)')'GammaTMA', 'NOT DEFINED, USE 3.3'
# else
            WRITE(3,'(A14,A50)')'GammaTMA', 'NOT DEFINED, USE 3.3'
# endif
        ENDIF

        CALL READ_INTEGER(Nfreq,FILE_NAME,'Nfreq',ierr)

        IF(ierr==1)THEN
            Nfreq = 1080
# if defined (PARALLEL)
            if (myid.eq.0) WRITE(3,'(A25,A50)')'Nfreq for TMA or JON', 'NOT DEFINED, USE 24*45=1080'
# else
            WRITE(3,'(A25,A50)')'Nfreq for TMA or JON', 'NOT DEFINED, USE 24*45=1080'
# endif
        ENDIF

        CALL READ_INTEGER(Ntheta,FILE_NAME,'Ntheta',ierr)

        IF(ierr==1)THEN
            Ntheta = 24
# if defined (PARALLEL)
            if (myid.eq.0) WRITE(3,'(A25,A50)')'Ntheta for TMA or JON', 'NOT DEFINED, USE 24'
# else
            WRITE(3,'(A25,A50)')'Ntheta for TMA or JON', 'NOT DEFINED, USE 24'
# endif
        ENDIF

        CALL READ_FLOAT(ThetaPeak,FILE_NAME,'ThetaPeak',ierr)

        IF(ierr==1)THEN
            ThetaPeak = 0.0_SP
# if defined (PARALLEL)
            if (myid.eq.0) WRITE(3,'(A25,A50)')'ThetaPeak', 'NOT DEFINED, USE 0.0'
# else
            WRITE(3,'(A25,A50)')'ThetaPeak', 'NOT DEFINED, USE 0.0'
# endif
        ENDIF ! end ierr

        CALL READ_FLOAT(Sigma_Theta,FILE_NAME,'Sigma_Theta',ierr)

        IF(ierr==1)THEN
            Sigma_Theta = 10.0_SP
# if defined (PARALLEL)
            if (myid.eq.0) WRITE(3,'(A25,A50)')'Sigma_Theta', 'NOT DEFINED, USE 10.0'
# else
            WRITE(3,'(A25,A50)')'Sigma_Theta', 'NOT DEFINED, USE 10.0'
# endif
        ENDIF ! end ierr

        CALL READ_FLOAT(Ywidth_WK,FILE_NAME,'Ywidth_WK',ierr)

        IF(ierr==1)THEN
            Ywidth_WK = LARGE
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A50)')'Ywidth_WK Default:  LARGE'
                WRITE(3,'(A50)')'Ywidth_WK Default:  LARGE'
            endif
# else
            WRITE(*,'(A50)')'Ywidth_WK Default:  LARGE'
            WRITE(3,'(A50)')'Ywidth_WK Default:  LARGE'
# endif
        ENDIF

        CALL READ_FLOAT(alpha_c,FILE_NAME,'alpha_c',ierr)

        IF(ierr==1)THEN
            alpha_c = 0.0_SP
# if defined (PARALLEL)
            if (myid.eq.0) THEN
                WRITE(*,'(A50)')'Wave Coherence Percentage:  0.0%'
                WRITE(3,'(A50)')'Wave Coherence Percentage:  0.0%'
            endif
# else
            WRITE(*,'(A50)')'Wave Coherence Percentage:  0.0%'
            WRITE(3,'(A50)')'Wave Coherence Percentage:  0.0%'
# endif
        ENDIF

# if defined (PARALLEL)
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
        if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Yc_WK   =', Yc_WK
        if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
        if (myid.eq.0) WRITE(3,'(A12,I12)')'Nfreq=  ', Nfreq
        if (myid.eq.0) WRITE(3,'(A12,I12)')'Ntheta=  ', Ntheta
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqPeak=  ', FreqPeak
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqMin =  ', FreqMin
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqMax =  ', FreqMax
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Hmo     =  ', Hmo
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'GammaTMA=  ', GammaTMA
        if (myid.eq.0) WRITE(3,'(A12,F12.2)')'ThetaPeak= ', ThetaPeak
        if (myid.eq.0) WRITE(3,'(A13,F12.2)')'Sigma_Theta=', Sigma_Theta
        if (myid.eq.0) WRITE(3,'(A13,F12.2)')'alpha_c=', alpha_c
# else
        WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
        WRITE(3,'(A10,F12.2)')'Yc_WK   =', Yc_WK
        WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
        WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
        WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
        WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
        WRITE(3,'(A12,I12)')'Nfreq=  ', Nfreq
        WRITE(3,'(A12,I12)')'Ntheta=  ', Ntheta
        WRITE(3,'(A12,F12.2)')'FreqPeak=  ', FreqPeak
        WRITE(3,'(A12,F12.2)')'FreqMin =  ', FreqMin
        WRITE(3,'(A12,F12.2)')'FreqMax =  ', FreqMax
        WRITE(3,'(A12,F12.2)')'Hmo     =  ', Hmo
        WRITE(3,'(A12,F12.2)')'GammaTMA=  ', GammaTMA
        WRITE(3,'(A12,F12.2)')'ThetaPeak= ', ThetaPeak
        WRITE(3,'(A13,F12.2)')'Sigma_Theta=', Sigma_Theta
        WRITE(3,'(A13,F12.2)')'alpha_c=', alpha_c
# endif

    ENDIF ! IF(WaveMaker(1:10)=='WK_NEW_IRR')THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
     IMPLICIT NONE

     INTEGER :: iunit
     INTEGER, DIMENSION(:),ALLOCATABLE :: BufferCount
     REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: ZUV_Buffer
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStation = .TRUE.
     SAVE FirstCallStation, BufferCount,ZUV_Buffer

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStation) then
       FirstCallStation = .FALSE.
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
       ALLOCATE(BufferCount(NumberStations), &
            ZUV_Buffer(StationOutputBuffer,NumberStations,4))
       BufferCount = 0
! calculate how many output components

! check existing

 INQUIRE(FILE=TRIM(STATIONS_FILE),EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0)  &
   WRITE(*,*) TRIM(STATIONS_FILE), ' THE STATION FILE CANNOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(STATIONS_FILE), ' THE STATION FILE CANNOT BE FOUND. STOP'
    STOP
# endif
  ENDIF  ! exist
              
       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
          !ista(i) = Nghost+dum1-npx*Mglob/px
          !jsta(i) = Nghost+dum2-npy*Nglob/py
		ista(i) = Nghost+dum1-( iista - 1 )
		jsta(i) = Nghost+dum2-( jjsta - 1 )
!---ykchoi Jan/23/2018]
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif

# else
          ista(i) = Nghost+dum1
          jsta(i) = Nghost+dum2
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo
     endif

! write to stations

     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=100+i
           IF(mask(ista(i),jsta(i))<1)THEN
             eta_sta=ZERO
             u_sta=ZERO
             v_sta=ZERO
           ELSE  ! to avoid topography on nested water surface  
             eta_sta=eta(ista(i),jsta(i))
             u_sta=u(ista(i),jsta(i))
             v_sta=v(ista(i),jsta(i)) 
           ENDIF

            IF(BufferCount(i)<StationOutputBuffer.AND.TIME<TOTAL_TIME)THEN
              BufferCount(i) = BufferCount(i) +1
              ZUV_BUFFER(BufferCount(i),i,1)=time
              ZUV_BUFFER(BufferCount(i),i,2)=eta_sta
              ZUV_BUFFER(BufferCount(i),i,3)=u_sta
              ZUV_BUFFER(BufferCount(i),i,4)=v_sta
            ELSE
              DO j=1,StationOutputBuffer
                write (iunit,'(4E16.5E4)') (ZUV_BUFFER(j,i,k),k=1,4)
              ENDDO              
              BufferCount(i) = 0
              ZUV_BUFFER(:,i,1)=TOTAL_TIME
              ZUV_BUFFER(:,i,2:4)=ZERO

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(*,*) 'WRITE OUT STATAION ...'
# else
      WRITE(*,*) 'WRITE OUT STATIONS ...'
# endif


            ENDIF ! end buffer

       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

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
     IMPLICIT NONE

     INTEGER :: iunit
     INTEGER, DIMENSION(:),ALLOCATABLE :: BufferCount
     REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: ZUV_Buffer
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStation = .TRUE.
     SAVE FirstCallStation, BufferCount,ZUV_Buffer
     INTEGER :: unit_sf, unit_sta, err

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStation) then
       FirstCallStation = .FALSE.
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
       ALLOCATE(BufferCount(NumberStations), &
            ZUV_Buffer(StationOutputBuffer,NumberStations,4))
       BufferCount = 0

       err = unit_ranges%add('stations', NumberStations)
       unit_sta = unit_ranges % getFirstUnit ('stations')
 
! calculate how many output components
       unit_sf = get_unit()              
       open(unit_sf, FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(unit_sf ,*) dum1,dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
          !ista(i) = Nghost+dum1-npx*Mglob/px
          !jsta(i) = Nghost+dum2-npy*Nglob/py
          ista(i) = Nghost+dum1-( iista - 1 )
          jsta(i) = Nghost+dum2-( jjsta - 1 )
!---ykchoi Jan/23/2018]
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=unit_sta + i - 1
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif

# else
          ista(i) = Nghost+dum1
          jsta(i) = Nghost+dum2
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=unit_sta + i - 1
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo

     else
        unit_sta = unit_ranges % getFirstUnit ('stations')
     endif

! write to stations

     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=unit_sta + i - 1
          IF(mask(ista(i),jsta(i))<1)THEN
            eta_sta=ZERO
            u_sta=ZERO
            v_sta=ZERO
          ELSE  ! to avoid topography on nested water surface
            eta_sta=eta(ista(i),jsta(i))
            u_sta=u(ista(i),jsta(i))
            v_sta=v(ista(i),jsta(i)) 
          ENDIF

            IF(BufferCount(i)<StationOutputBuffer.AND.TIME<TOTAL_TIME)THEN
              BufferCount(i) = BufferCount(i) +1
              ZUV_BUFFER(BufferCount(i),i,1)=time
              ZUV_BUFFER(BufferCount(i),i,2)=eta_sta
              ZUV_BUFFER(BufferCount(i),i,3)=u_sta
              ZUV_BUFFER(BufferCount(i),i,4)=v_sta
            ELSE
              DO j=1,StationOutputBuffer
                write (iunit,'(4E16.5E4)') (ZUV_BUFFER(j,i,k),k=1,4)
              ENDDO              
              BufferCount(i) = 0
              ZUV_BUFFER(:,i,1)=TOTAL_TIME
              ZUV_BUFFER(:,i,2:4)=ZERO

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(*,*) 'WRITE OUT STATAION ...'
# else
      WRITE(*,*) 'WRITE OUT STATIONS ...'
# endif


            ENDIF ! end buffer


!          write (iunit,'(20E16.5E4)') time, eta_sta,&
!                          u_sta,v_sta
       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=unit_sta + i - 1
             close(iunit)
          endif
       enddo
     endif

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
     IMPLICIT NONE

     INTEGER :: iunit
     INTEGER, DIMENSION(:),ALLOCATABLE :: BufferCount
     REAL(SP),DIMENSION(:,:,:),ALLOCATABLE :: ZUV_Buffer
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStationSP = .TRUE.
     SAVE FirstCallStationSP, BufferCount,ZUV_Buffer
     INTEGER :: unit_sf, unit_sta, err

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStationSP) then
       FirstCallStationSP = .FALSE.
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
       ALLOCATE(BufferCount(NumberStations), &
            ZUV_Buffer(StationOutputBuffer,NumberStations,4))
       BufferCount = 0
       err = unit_ranges%add('stations', NumberStations)
       unit_sta = unit_ranges % getFirstUnit ('stations')

!       ALLOCATE(envelope(Mloc,Nloc))
!       envelope = 0.d0
       unit_sf = get_unit() 
       open(unit_sf,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(unit_sa,*) dum1,dum2
# if defined (PARALLEL)
![---ykchoi Jan/23/2018
!          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) &
!                     -npx*Mglob/px
!          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) &
!                     -npy*Nglob/py
          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) &
                     -( iista - 1 )
          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) &
                     -( jjsta - 1 )
!---ykchoi Jan/23/2018]
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=get_unit()
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# else
          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) 
          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) 
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=unit_sta + i - 1
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo

     else
        unit_sta = unit_ranges % getFirstUnit ('stations')
     endif

! write to stations
 
     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=unit_sta + i - 1

! for coupling, make sure dry point
          IF(mask(ista(i),jsta(i))<1)THEN
            eta_sta=ZERO
            u_sta=ZERO
            v_sta=ZERO
          ELSE  ! to avoid topography on nested water surface
            eta_sta=eta(ista(i),jsta(i))
            u_sta=u(ista(i),jsta(i))
            v_sta=v(ista(i),jsta(i)) 
          ENDIF

            IF(BufferCount(i)<StationOutputBuffer.AND.TIME<TOTAL_TIME)THEN
              BufferCount(i) = BufferCount(i) +1
              ZUV_BUFFER(BufferCount(i),i,1)=time
              ZUV_BUFFER(BufferCount(i),i,2)=eta_sta
              ZUV_BUFFER(BufferCount(i),i,3)=u_sta
              ZUV_BUFFER(BufferCount(i),i,4)=v_sta
            ELSE
              DO j=1,StationOutputBuffer
                write (iunit,'(4E16.5E4)') (ZUV_BUFFER(j,i,k),k=1,4)
              ENDDO              
              BufferCount(i) = 0
              ZUV_BUFFER(:,i,1)=TOTAL_TIME
              ZUV_BUFFER(:,i,2:4)=ZERO

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(*,*) 'WRITE OUT STATAION ...'
# else
      WRITE(*,*) 'WRITE OUT STATIONS ...'
# endif


            ENDIF ! end buffer


!          write (iunit,'(20E16.5E4)') time, eta_sta,&
!                          u_sta,v_sta

!          write (iunit,*) time, eta(ista(i),jsta(i)),u(ista(i),jsta(i)),&
!                                v(ista(i),jsta(i))

       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then

       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=unit_sta + i - 1
             close(iunit)
          endif
       enddo
     endif

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

     IMPLICIT NONE

     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::FILE_NAME_MEAN=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT=ICOUNT+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time        
        endif
# else
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
# endif

102     FORMAT(A20,I6,A14,F12.3,A2,F12.3)

        !ykchoi
	  !itmp1=mod(icount/1000,10)
        !itmp2=mod(icount/100,10)
        !itmp3=mod(icount/10,10)
        !itmp4=mod(icount,10)
	   itmp1=mod(icount/10000,10)
	   itmp2=mod(icount/1000,10)
	   itmp3=mod(icount/100,10)
	   itmp4=mod(icount/10,10)
	   itmp5=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
    	  write(file_name(5:5),'(I1)')itmp5   !ykchoi

     IF(ICOUNT==1)THEN
     IF(OUT_DEPTH.OR.BREAKWATER)THEN
        TMP_NAME = TRIM(FDIR)//'dep.out'
        call PutFile(TMP_NAME,DEPTH)
        TMP_NAME = TRIM(FDIR)//'cd_breakwater.out'
        call PutFile(TMP_NAME,CD_breakwater)
     ENDIF
     ENDIF
![ykchoi
     write(10000,*)time, dt
!ykchoi]

     IF(OUT_ETA)THEN
        TMP_NAME = TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Eta)
     ENDIF

# if defined (UseEtaScreen)
     IF(OUT_ETAscreen)THEN
        TMP_NAME = TRIM(FDIR)//'etasrn_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,EtaScreen)
     ENDIF
# endif

     IF(OUT_Hmax)THEN
        TMP_NAME = TRIM(FDIR)//'hmax_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,HeightMax)
     ENDIF

     IF(OUT_Hmin)THEN
        TMP_NAME = TRIM(FDIR)//'hmin_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,HeightMin)
     ENDIF

     IF(OUT_Umax)THEN
        TMP_NAME = TRIM(FDIR)//'umax_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,VelocityMax)
     ENDIF
     
     IF(OUT_MFmax)THEN                                                                                            
        TMP_NAME = TRIM(FDIR)//'MFmax_'//TRIM(FILE_NAME)                                                          
        call PutFile(TMP_NAME,MomentumFluxMax)                                                                              
     ENDIF      
     
     IF(OUT_VORmax)THEN                                                                                            
        TMP_NAME = TRIM(FDIR)//'VORmax_'//TRIM(FILE_NAME)                                                          
        call PutFile(TMP_NAME,VorticityMax)                                                                              
     ENDIF            
     
     IF(OUT_U)THEN
        TMP_NAME = TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,U)
     ENDIF

     IF(OUT_V)THEN
        TMP_NAME = TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,V)
     ENDIF

     IF(OUT_MASK)THEN
        TMP_NAME = TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
        Int2Flo=MASK
        call PutFile(TMP_NAME,Int2Flo)
     ENDIF

     IF(OUT_MASK9)THEN
        TMP_NAME = TRIM(FDIR)//'mask9_'//TRIM(FILE_NAME)
        Int2Flo=MASK9
        call PutFile(TMP_NAME,Int2Flo)
     ENDIF

210   FORMAT(5000I3)

     IF(OUT_P)THEN
        TMP_NAME = TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,P(1:Mloc,1:Nloc))
     ENDIF

     IF(OUT_Q)THEN
        TMP_NAME = TRIM(FDIR)//'q_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Q(1:Mloc,1:Nloc))
     ENDIF


     IF(OUT_AGE)THEN
      IF(SHOW_BREAKING)THEN
        TMP_NAME = TRIM(FDIR)//'age_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,AGE_BREAKING)
      ENDIF
     ENDIF

     IF(OUT_ROLLER)THEN
        TMP_NAME = TRIM(FDIR)//'roller_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,ROLLER_FLUX)
     ENDIF

     IF(OUT_UNDERTOW)THEN
        TMP_NAME = TRIM(FDIR)//'U_undertow_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,UNDERTOW_U)
        TMP_NAME = TRIM(FDIR)//'V_undertow_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,UNDERTOW_V)
     ENDIF

      IF(VISCOSITY_BREAKING)THEN
       IF(OUT_NU)THEN
        TMP_NAME = TRIM(FDIR)//'nubrk_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,nu_break)
       ENDIF
!        TMP_NAME = TRIM(FDIR)//'etat_'//TRIM(FILE_NAME)
!         call PutFile(TMP_NAME,etat) 
      ENDIF

      IF(OUT_Time)THEN
        TMP_NAME = TRIM(FDIR)//'time_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,ARRTIME)
      ENDIF

# if defined (METEO)
   IF(OUT_METEO)THEN

    IF(WindHollandModel)THEN
        TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,StormPressureTotal)
        TMP_NAME = TRIM(FDIR)//'Ustorm_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,WindU2D)
        TMP_NAME = TRIM(FDIR)//'Vstorm_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,WindV2D)
    ENDIF

    IF(MeteoGausian)THEN
        TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,StormPressureTotal)
    ENDIF

    IF(SlideModel)THEN
        TMP_NAME = TRIM(FDIR)//'Pstorm_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,StormPressureTotal)      
    ENDIF

  ENDIF
# endif
     
# if defined (VESSEL)
     IF(OUT_VESSEL)THEN
# if defined (VESSEL_PANEL_SOURCE)
        TMP_NAME = TRIM(FDIR)//'Fves_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,VesselFluxGradient)
# else
        TMP_NAME = TRIM(FDIR)//'Pves_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,VesselPressureTotal)
# endif
   ! end panel source

     ENDIF
# endif

! sediment
# if defined (SEDIMENT)
        TMP_NAME = TRIM(FDIR)//'C_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,CH *Sdensity*RHO_WATER)
        TMP_NAME = TRIM(FDIR)//'Pick_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Pickup)
        TMP_NAME = TRIM(FDIR)//'Depo_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,D)

        TMP_NAME = TRIM(FDIR)//'DchgS_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,TotalSuspendLoad)
        TMP_NAME = TRIM(FDIR)//'DchgB_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,TotalBedLoad)

        TMP_NAME = TRIM(FDIR)//'BedFx_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,BedFluxX)
        TMP_NAME = TRIM(FDIR)//'BedFy_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,BedFluxY)

        TMP_NAME = TRIM(FDIR)//'dep_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Depth)

# endif
! end sediment

     IF(OUT_TMP)THEN
        TMP_NAME = TRIM(FDIR)//'tmp_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,tmp4preview)
     ENDIF

101   continue

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
     REAL(SP),DIMENSION(Mloc,Nloc) :: tmpout 

     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     CHARACTER(LEN=80)::TMP_NAME=' '

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT_MEAN=ICOUNT_MEAN+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING MEAN FILE', icount_mean
        WRITE(*,102)'PRINTING MEAN FILE', icount_mean
        endif
# else
        WRITE(*,102)'PRINTING MEAN FILE', icount_mean
# endif

102     FORMAT(A20,I6)

	   itmp1=mod(icount_mean/10000,10)
	   itmp2=mod(icount_mean/1000,10)
	   itmp3=mod(icount_mean/100,10)
	   itmp4=mod(icount_mean/10,10)
	   itmp5=mod(icount_mean,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
    	write(file_name(5:5),'(I1)')itmp5  

        IF(OUT_Umean)THEN
          TMP_NAME = TRIM(FDIR)//'umean_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,Umean)
          tmpout = P_mean / Max(Depth+ETAmean,MinDepthFrc)
          TMP_NAME = TRIM(FDIR)//'ulagm_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,tmpout)
        ENDIF
        IF(OUT_Vmean)THEN
          TMP_NAME = TRIM(FDIR)//'vmean_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,Vmean)
          tmpout = Q_mean / Max(Depth+ETAmean,MinDepthFrc)
          TMP_NAME = TRIM(FDIR)//'vlagm_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,tmpout)
        ENDIF
        IF(OUT_ETAmean)THEN
          TMP_NAME = TRIM(FDIR)//'etamean_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,ETAmean)
        ENDIF
        IF(OUT_WaveHeight)THEN
          TMP_NAME = TRIM(FDIR)//'Hrms_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,WaveHeightRMS)
          TMP_NAME = TRIM(FDIR)//'Havg_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,WaveHeightAve)
          TMP_NAME = TRIM(FDIR)//'Hsig_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,SigWaveHeight)
        ENDIF

 ! output FRCYsum,FRCYmean
 !        DxSxx,DySxy,DySyy,DxSxy,PgrdX,PgrdY,DxUUH,DyUVH,DyVVH,DxUVH

        IF(OUT_Radiation)THEN
          TMP_NAME = TRIM(FDIR)//'Sxx_'//TRIM(FILE_NAME)
          tmpout = UUmean-WWmean+0.5*9.8*ETA2mean
          call PutFile(TMP_NAME,tmpout)
          TMP_NAME = TRIM(FDIR)//'Sxy_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,UVmean)
          TMP_NAME = TRIM(FDIR)//'Syy_'//TRIM(FILE_NAME)
          tmpout = VVmean-WWmean+0.5*9.8*ETA2mean
          call PutFile(TMP_NAME,tmpout)

          TMP_NAME = TRIM(FDIR)//'DxSxx_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DxSxx)
          TMP_NAME = TRIM(FDIR)//'DySxy_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DySxy)
          TMP_NAME = TRIM(FDIR)//'DySyy_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DySyy)
          TMP_NAME = TRIM(FDIR)//'DxSxy_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DxSxy)
          TMP_NAME = TRIM(FDIR)//'PgrdX_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,PgrdX)
          TMP_NAME = TRIM(FDIR)//'PgrdY_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,PgrdY)
          TMP_NAME = TRIM(FDIR)//'DxUUH_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DxUUH)
          TMP_NAME = TRIM(FDIR)//'DyUVH_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DyUVH)
          TMP_NAME = TRIM(FDIR)//'DyVVH_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DyVVH)
          TMP_NAME = TRIM(FDIR)//'DxUVH_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,DxUVH)
          TMP_NAME = TRIM(FDIR)//'FRCX_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,FRCXmean)
          TMP_NAME = TRIM(FDIR)//'FRCY_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,FRCYmean)
        ENDIF
                    

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

SUBROUTINE GetFile(FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE

     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

![-------ykchoi (08/May/2017)
     INTEGER :: irank, lenx, leny, lenxy, ireq
     INTEGER :: Nista, Niend, Njsta, Njend
     INTEGER :: istanum, iendnum, jstanum, jendnum
     INTEGER, ALLOCATABLE :: Nistas(:), Niends(:), Njstas(:), Njends(:)
     INTEGER :: istatus(mpi_status_size)
     REAL(SP), ALLOCATABLE :: xx(:,:)
! -------ykchoi (08/May/2017) ]

! TEMP

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,NGlob+NGhost
           READ(1,*)(PHIGLOB(I,J),I=Nghost+1,MGlob+Nghost)
        ENDDO
        CLOSE(1)
! ghost cells
        DO I=Nghost+1,MGlob+Nghost
           DO J=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(I,Nghost+1)
           ENDDO
           DO J=NGlob+Nghost+1,NGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(I,NGlob+Nghost)
           ENDDO
        ENDDO
        DO J=1,NGlob+2*Nghost
           DO I=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(Nghost+1,J)
           ENDDO
           DO I=MGlob+Nghost+1,MGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(MGlob+Nghost,J)
           ENDDO
        ENDDO
     endif

![-------ykchoi (08/May/2017)
     Nista = iista + Nghost;
     Niend = iiend + Nghost;
     Njsta = jjsta + Nghost;
     Njend = jjend + Nghost;

     allocate( Nistas(nprocs), Niends(nprocs), Njstas(nprocs), Njends(nprocs) )

     call MPI_Gather( Nista, 1, MPI_INTEGER, Nistas, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( Niend, 1, MPI_INTEGER, Niends, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( Njsta, 1, MPI_INTEGER, Njstas, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )
     call MPI_Gather( Njend, 1, MPI_INTEGER, Njends, 1, MPI_INTEGER, &
                      0, MPI_COMM_WORLD, ier )

     if( myid == 0 )then
	 PHI = PHIGLOB( 1:Mloc, 1:Nloc )
     endif

     do irank=1, px*py-1
	  if( myid == 0 ) then
	    istanum = Nistas(irank+1) - Nghost
	    iendnum = Niends(irank+1) + Nghost
	    jstanum = Njstas(irank+1) - Nghost
          jendnum = Njends(irank+1) + Nghost

	    lenx = iendnum - istanum + 1
	    leny = jendnum - jstanum + 1
	    lenxy = lenx*leny
	    allocate( xx(lenx, leny) )

	    xx = PHIGLOB( istanum:iendnum, jstanum:jendnum )
	    call mpi_isend( xx, lenxy, mpi_sp, irank, 1, mpi_comm_world, ireq, ier )
	    call mpi_wait( ireq, istatus, ier )
          deallocate( xx )

	  elseif( myid == irank ) then
	    
	    lenx = Niend-Nista+1+2*Nghost
	    leny = Njend-Njsta+1+2*Nghost
	    lenxy = lenx*leny

	    call mpi_irecv( PHI, lenxy, mpi_sp, 0, 1, mpi_comm_world, ireq, ier )
	    call mpi_wait( ireq, istatus, ier )

	  endif
     enddo

     deallocate( Nistas, Niends, Njstas, Njends )

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

SUBROUTINE PutFile(FILE_NAME,PHI)
     USE GLOBAL
     USE PARALLEL_FIELD_IO
     IMPLICIT NONE

     CHARACTER(LEN=80) FILE_NAME
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI

     CHARACTER(LEN=80)::TMP_NAME=' '

     SELECT CASE (TRIM(FIELD_IO_TYPE))
      CASE ('ASCII' , 'ascii')
         CALL PutFileASCII(FILE_NAME,PHI)
      CASE ('BINARY' , 'binary' )
         Call PutFileBinary(FILE_NAME,PHI)
      CASE DEFAULT
         !Defaults to ASCII case for non-valid input
         CALL PutFileASCII(FILE_NAME,PHI)
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
SUBROUTINE PutFile(FILE,PHI)
     USE PARAM
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI
     CHARACTER(LEN=80) FILE
     LOGICAL :: FirstCallPutFile = .TRUE.
     SAVE  FirstCallPutFile

! first time call 
     IF(FirstCallPutFile)THEN
        FirstCallPutFile = .FALSE.
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:15),'(A7)') 'E16.6E4'
        write(FORMAT_LEN(16:16),'(A1)') ')'
     ENDIF

        OPEN(1,FILE=TRIM(FILE))
# if defined(DEBUG)
        DO J=1,Nloc
           WRITE(1,100)(real(PHI(I,J)),I=1,Mloc)
        ENDDO
# else
        DO J=Nghost+1,Nloc-Nghost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHI(I,J)),I=Nghost+1,Mloc-Nghost,OUTPUT_RES)
        ENDDO
# endif
100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
END SUBROUTINE PutFile

# endif

# if defined (VESSEL)
!-------------------------------------------------------------------------------------
!
!    OUTPUT_VESSEL is a subroutine to write vessel data
!
! HISTORY: 
!    02/18/2017  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE OUTPUT_VESSEL
     USE GLOBAL
     USE INPUT_READ
     USE VESSEL_MODULE

     IMPLICIT NONE
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallVessel = .TRUE.
     SAVE FirstCallVessel

# if defined (PARALLEL)
     IF(myid.eq.0)THEN
# endif

! initialize stations
     if (FirstCallVessel) then
       FDIR=TRIM(RESULT_FOLDER)
       FirstCallVessel = .FALSE.
       open(99,FILE=TRIM(FDIR)//'Resis.txt')
     endif

! write to stations

       write (99,'(60E16.5)') time,(ResistanceX(i),i=1,NumVessel), &
                                 (ResPosX(i),i=1,NumVessel), &
                                 (ResNegX(i),i=1,NumVessel), &
                                 (ResistanceY(i),i=1,NumVessel), &
                                 (ResPosY(i),i=1,NumVessel), &
                                 (ResNegY(i),i=1,NumVessel)

! close station files
     if (TIME.ge.TOTAL_TIME) then
       CLOSE(99) 
     endif

# if defined (PARALLEL)
      ENDIF
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
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallSediment = .TRUE.
     SAVE FirstCallSediment

# if defined (PARALLEL)
     IF(myid.eq.0)THEN
# endif

! initialize stations
     if (FirstCallSediment) then
       FDIR=TRIM(RESULT_FOLDER)
       FirstCallSediment = .FALSE.
     endif

! time series here

# if defined (PARALLEL)
      ENDIF
# endif

END SUBROUTINE OUTPUT_SEDIMENT

# endif
! end sediment

INTEGER FUNCTION GET_UNIT()

     USE GLOBAL, ONLY : UnitGenerateStart, UnitGenerateEnd
     IMPLICIT NONE
     INTEGER :: new_unit
     LOGICAL :: is_open

     INTEGER, PARAMETER :: MIN_UNIT_NUM = UnitGenerateStart
     INTEGER, PARAMETER :: MAX_UNIT_NUM = UnitGenerateEnd


     DO new_unit = MIN_UNIT_NUM, MAX_UNIT_NUM

        INQUIRE(unit=new_unit, opened = is_open)

        IF( .NOT.is_open) THEN
           GET_UNIT = new_unit
           RETURN
        ENDIF
        
     END DO 


     GET_UNIT=-1

END FUNCTION GET_UNIT
