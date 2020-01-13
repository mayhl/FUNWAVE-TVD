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

# if defined (PARALLEL)
#define SWRITE IF (myid.EQ.0 ) WRITE
# else
#define SWRITE WRITE
# endif


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
    USE LOG_OUTPUT
    USE INPUT_READ
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

    CHARACTER(LEN=100) :: FMT_STR_BUFF

    !>by Zhouteng Ye
    CHARACTER(LEN=80)::INPUT_NAME=''

![ykchoi
    CHARACTER(LEN=80)::FDIR=' '
!ykchoi]

    LOGICAL :: IsDelayed

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

! title
      CALL READ_STRING(TITLE,FILE_NAME,'TITLE',ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No TITLE in ', FILE_NAME, 'use default'
        TITLE='---TEST RUN---'
      ENDIF

      CALL WRITE_LOG( LOG_BANNER( 'LOG FILE' ) )
      CALL WRITE_LOG( TITLE )
      CALL WRITE_LOG( LOG_BANNER ( 'INPUT START' ) )

      CALL INIT_TMP_LOG


# if defined (PARALLEL)
! ---------------------------------------
! -            Parallel Info            -
! ---------------------------------------
      CALL WRITE_LOG( LOG_BANNER('PARALLEL' ) )

      CALL READ_VALUE_CHECK(PX, FILE_NAME, 'PX', DefaultValue=1 )
      CALL READ_VALUE_CHECK(PY, FILE_NAME, 'PY', DefaultValue=1 )

      CALL FLUSH_TMP_LOG

# endif


! ---------------------------------------
! -              Grid Info              -
! ---------------------------------------
      CALL WRITE_LOG( LOG_BANNER( 'GRID INFO' ) )

      CALL READ_VALUE_CHECK(Mglob, FILE_NAME, 'Mglob' )
      CALL READ_VALUE_CHECK(Nglob, FILE_NAME, 'Nglob' )

! grid 
# if defined (CARTESIAN)

      CALL WRITE_ALL( 'Equispaced Cartesian Coordinates Mode' )
      CALL READ_VALUE_CHECK(DX, FILE_NAME, 'DX' )
      CALL READ_VALUE_CHECK(DY, FILE_NAME, 'DY' )

# else

     CALL READ_VALUE_CHECK( StretchGrid, FILE_NAME, 'StretchGrid', DefaultValue=.FALSE. )

     IF ( StretchGrid ) THEN
        CALL WRITE_ALL( 'Stretched Cartesian Coordinates Mode' )

        CALL READ_VALUE_CHECK( DX_FILE      , FILE_NAME, 'DX_FILE'       )  
        CALL READ_VALUE_CHECK( DY_FILE      , FILE_NAME, 'DY_FILE'       )  
        CALL READ_VALUE_CHECK( Coriolis_FILE, FILE_NAME, 'CORIOLIS_FILE' ) 
  
     ELSE 
        CALL WRITE_ALL( 'Spherical Coordinates Mode' )

        CALL READ_VALUE_CHECK( Lon_West , FILE_NAME, 'Lon_West'  )
        CALL READ_VALUE_CHECK( Lat_South, FILE_NAME, 'Lat_South' )
        CALL READ_VALUE_CHECK( Dphi     , FILE_NAME, 'Dphi'      )
        CALL READ_VALUE_CHECK( Dtheta   , FILE_NAME, 'Dtheta'    )

     END IF
# endif

! depth
 
     CALL READ_VALUE_CHECK(DEPTH_TYPE, FILE_NAME, 'DEPTH_TYPE' , DefaultValue='FLAT') 

     IF ( DEPTH_TYPE(1:3).EQ.'DAT' ) THEN
        CALL READ_VALUE_CHECK( DEPTH_FILE, FILE_NAME, 'DEPTH_FILE' )
     END IF  ! end type=data

     IF( DEPTH_TYPE(1:3).EQ.'FLA' ) THEN
        CALL READ_VALUE_CHECK( DEPTH_FLAT, FILE_NAME, 'DEPTH_FLAT' , DefaultValue=10.0_SP , Units='m' ) 
     END IF ! endif type=flat

     IF ( DEPTH_TYPE(1:3).EQ.'SLO' ) THEN
        CALL READ_VALUE_CHECK( DEPTH_FLAT, FILE_NAME, 'DEPTH_FLAT' , DefaultValue=10.0_SP , Units='m' )
        CALL READ_VALUE_CHECK( SLP       , FILE_NAME, 'SLP'        , DefaultValue=0.1_SP              ) 
        CALL READ_VALUE_CHECK( Xslp      ,FILE_NAME , 'Xslp'       , DefaultValue=0.0_SP              ) 
     END IF  ! endif type=slope

! depth correction

     CALL READ_VALUE_CHECK( BATHY_CORRECTION, FILE_NAME, 'BATHY_CORRECTION', DefaultValue=.FALSE. )
     IF (BATHY_CORRECTION) CALL WRITE_ALL( 'Bathymetry has been corrected!' )

     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -           Time Info                 -
! ---------------------------------------

     CALL WRITE_LOG( LOG_BANNER( 'TIME INFO' ) )

     CALL READ_VALUE_CHECK( TOTAL_TIME         , FILE_NAME, 'TOTAL_TIME'                                          )
     CALL READ_VALUE_CHECK( PLOT_START_TIME    , FILE_NAME, 'PLOT_START_TIME'    , DefaultValue=0.0_SP, Units='s' )
     CALL READ_VALUE_CHECK( PLOT_INTV          , FILE_NAME, 'PLOT_INTV'          , DefaultValue=1.0_SP, Units='s' )
     CALL READ_VALUE_CHECK( PLOT_INTV_STATION  , FILE_NAME, 'PLOT_INTV_STATION'  , DefaultValue=1.0_SP, Units='s' )
     CALL READ_VALUE_CHECK( StationOutputBuffer, FILE_NAME, 'StationOutputBuffer', DefaultValue=1000              )
     CALL READ_VALUE_CHECK( SCREEN_INTV        , FILE_NAME, 'SCREEN_INTV'        , DefaultValue=1.0_SP, Units='s' )

     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -          Hot Start Info             -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'HOT START' ) )

     CALL READ_VALUE_CHECK( INI_UVZ, FILE_NAME, 'INI_UVZ', DefaultValue=.FALSE. )

! initial uvz
     IF ( INI_UVZ ) THEN
        CALL READ_VALUE_CHECK( ETA_FILE, FILE_NAME, 'ETA_FILE' )

        CALL READ_VALUE_CHECK_DELAY_DEFAULT( U_FILE, FILE_NAME, 'U_FILE' , IsDefault=NO_UV_FILE )
        IF ( NO_UV_FILE ) THEN
           CALL WRITE_ALL( 'WARNING: U_FILE not found in input file. Defaulting U and V to ZERO.' )
        ELSE
           ! Only check for V_FILE if U_FILE is found
           CALL READ_VALUE_CHECK_DELAY_DEFAULT( V_FILE, FILE_NAME, 'V_FILE' , IsDefault=NO_UV_FILE )
           IF ( NO_UV_FILE ) THEN
              CALL WRITE_ALL( 'WARNING: U_FILE found but V_FILE not found in input file. Defaulting U and V to ZERO.' )
           END IF
        END IF


        CALL READ_VALUE_CHECK_DELAY_DEFAULT( MASK_FILE, FILE_NAME, 'MASK_FILE', IsDefault=NO_MASK_FILE )
        IF ( NO_MASK_FILE ) CALL WRITE_ALL( 'WARNING: MASK_FILE not found in input file. Using NO_MASK option.' )

        CALL READ_VALUE_CHECK( HotStartTime, FILE_NAME, 'HotStartTime'     , DefaultValue=0.0_SP , Units='s' )
        CALL READ_VALUE_CHECK( icount      , FILE_NAME, 'OutputStartNumber', DefaultValue=1                  )
        icount = icount-1
     END IF

     CALL FLUSH_TMP_LOG
! end initial hot start file

! ---------------------------------------
! -         Water Level Info            -
! ---------------------------------------
! add water level 03/29/2016
     CALL WRITE_LOG( LOG_BANNER( 'STILL WATER INFO' ) )
     CALL READ_VALUE_CHECK( WaterLevel, FILE_NAME, 'WaterLevel', DefaultValue=0.0_SP, Units='m' )
     CALL FLUSH_TMP_LOG

! ---------------------------------------
! -          Wavemake Info              -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'WAVEMAKER' ) )
     
     CALL READ_VALUE_CHECK( WaveMaker, FILE_NAME, 'WAVEMAKER', DefaultValue='nothing' )

     IF ( WaveMaker(1:7).EQ.'LEF_SOL' ) THEN
        CALL READ_VALUE_CHECK( AMP_SOLI, FILE_NAME, 'AMP'                                      )
        CALL READ_VALUE_CHECK( DEP_SOLI, FILE_NAME, 'DEP'                                      )
        CALL READ_VALUE_CHECK( LAG_SOLI, FILE_NAME, 'LAGTIME', DefaultValue=0.0_SP , Units='s' )

     END IF

     IF ( WaveMaker(1:7).EQ.'WK_TIME' ) THEN

        CALL READ_VALUE_CHECK( NumWaveComp , FILE_NAME, 'NumWaveComp'                                   )
        CALL READ_VALUE_CHECK( PeakPeriod  , FILE_NAME, 'PeakPeriod'                                    )
        CALL READ_VALUE_CHECK( WaveCompFile, FILE_NAME, 'WaveCompFile'                                  )
        CALL READ_VALUE_CHECK( Xc_WK       , FILE_NAME, 'Xc_WK'                                         )
        CALL READ_VALUE_CHECK( Yc_WK       , FILE_NAME, 'Yc_WK'        , DefaultValue=0.0_SP            )
        CALL READ_VALUE_CHECK( DEP_WK      , FILE_NAME, 'DEP_WK'                                        )
        CALL READ_VALUE_CHECK( Time_ramp   , FILE_NAME, 'Time_ramp'    , DefaultValue=0.0_SP, Units='s' )
        CALL READ_VALUE_CHECK( Delta_WK    , FILE_NAME, 'Delta_WK'     , DefaultValue=0.5_SP            )
        CALL READ_VALUE_CHECK( Ywidth_WK   , FILE_NAME, 'Ywidth_WK'    , DefaultValue=LARGE , Units='m' )

     END IF  ! end WK_TIME

     IF (WaveMaker(1:7).EQ.'INI_SOL' ) THEN

        CALL READ_VALUE_CHECK( SolitaryPositiveDirection,FILE_NAME,  &
                              'SolitaryPositiveDirection', DefaultValue=.TRUE. )

        IF ( SolitaryPositiveDirection ) THEN
           CALL WRITE_ALL( 'Solitary wave propagate in + X direction' )
        ELSE
           CALL WRITE_ALL( 'Solitary wave propagate in - X direction' )
        END IF

        CALL READ_VALUE_CHECK( AMP_SOLI  , FILE_NAME, 'AMP'        )
        CALL READ_VALUE_CHECK( DEP_SOLI  , FILE_NAME,' DEP'        )
        CALL READ_VALUE_CHECK( XWAVEMAKER, FILE_NAME, 'XWAVEMAKER' )

     END IF  ! end initial solitary

     IF (WaveMaker(1:6).EQ.'N_WAVE' ) THEN

        CALL READ_VALUE_CHECK( x1_Nwave   , FILE_NAME, 'x1_Nwave'    )
        CALL READ_VALUE_CHECK( x2_Nwave   , FILE_NAME, 'x2_Nwave'    )
        CALL READ_VALUE_CHECK( a0_Nwave   , FILE_NAME, 'a0_Nwave'    )
        CALL READ_VALUE_CHECK( gamma_Nwave, FILE_NAME, 'gamma_Nwave' )
        CALL READ_VALUE_CHECK( dep_Nwave  , FILE_NAME, 'dep_Nwave'   )
     END IF  ! end N_wave

     IF ( WaveMaker(1:7).EQ.'INI_REC' ) THEN

        CALL READ_VALUE_CHECK( AMP_SOLI, FILE_NAME, 'AMP'                       )
        CALL READ_VALUE_CHECK( Xc      , FILE_NAME,' Xc'                        )
        ! Should this default to 0.0?
        CALL READ_VALUE_CHECK( Yc      , FILE_NAME, 'Yc'  , DefaultValue=0.0_SP )
        CALL READ_VALUE_CHECK( WID     , FILE_NAME, 'WID'                       )
     END IF ! endif rectangular hump

     IF ( WaveMaker(1:7).EQ.'INI_GAU'.OR.&
          WaveMaker(1:7).EQ.'INI_DIP'      )THEN

        CALL READ_VALUE_CHECK( AMP_SOLI, FILE_NAME, 'AMP' )
        CALL READ_VALUE_CHECK( Xc      , FILE_NAME, 'Xc'  )
        CALL READ_VALUE_CHECK( Yc      , FILE_NAME, 'Yc'  )
        CALL READ_VALUE_CHECK( WID     , FILE_NAME, 'WID' )

     END IF ! endif gaussian hump

     IF ( WaveMaker(1:6).EQ.'WK_REG' ) THEN

        CALL READ_VALUE_CHECK( Xc_WK    , FILE_NAME, 'Xc_WK'                                     )
        CALL READ_VALUE_CHECK( Yc_WK    , FILE_NAME, 'Yc_WK'    , DefaultValue=0.0_SP            )
        CALL READ_VALUE_CHECK( Tperiod  , FILE_NAME, 'Tperiod'                                   )
        CALL READ_VALUE_CHECK( AMP_WK   , FILE_NAME,' AMP_WK'                                    )
        CALL READ_VALUE_CHECK( DEP_WK   , FILE_NAME, 'DEP_WK'                                    )
        CALL READ_VALUE_CHECK( Theta_WK , FILE_NAME, 'Theta_WK' , DefaultValue=0.0_SP            )
        CALL READ_VALUE_CHECK( Time_ramp, FILE_NAME, 'Time_ramp', DefaultValue=0.0_SP, Units='s' )
        CALL READ_VALUE_CHECK( Delta_WK , FILE_NAME, 'Delta_WK' , DefaultValue=0.5_SP            )
        CALL READ_VALUE_CHECK( Ywidth_WK, FILE_NAME, 'Ywidth_WK', DefaultValue=LARGE             )

     END IF  ! endif WK_REG

     IF( WaveMaker(1:6).EQ.'WK_IRR'.OR.WaveMaker(1:6).EQ.'TMA_1D'.OR.&
         WaveMaker(1:6).EQ.'JON_1D'.OR.WaveMaker(1:6).EQ.'JON_2D'      ) THEN

        CALL READ_VALUE_CHECK( Xc_WK    , FILE_NAME, 'Xc_WK'                                     )
        CALL READ_VALUE_CHECK( Yc_WK    , FILE_NAME, 'Yc_WK'    , DefaultValue=0.0_SP            )
        CALL READ_VALUE_CHECK( DEP_WK   , FILE_NAME, 'DEP_WK'                                    )
        CALL READ_VALUE_CHECK( Time_ramp, FILE_NAME, 'Time_ramp', DefaultValue=0.0_SP, Units='s' )
        CALL READ_VALUE_CHECK( Delta_WK , FILE_NAME, 'Delta_WK' , DefaultValue=0.5_SP            )
        CALL READ_VALUE_CHECK( FreqPeak , FILE_NAME, 'FreqPeak'                                  )
        CALL READ_VALUE_CHECK( FreqMin  , FILE_NAME, 'FreqMin'                                   )
        CALL READ_VALUE_CHECK( FreqMax  , FILE_NAME, 'FreqMax'                                   )
        CALL READ_VALUE_CHECK( Hmo      , FILE_NAME, 'Hmo'                                       )
        CALL READ_VALUE_CHECK( GammaTMA , FILE_NAME, 'GammaTMA' , DefaultValue=3.3_SP            )


        ! Verify logic has been duplicated successfully 
        IF ( WaveMaker(1:3).EQ.'TMA'.OR. &
             WaveMaker(1:3).EQ.'JON'       ) THEN
           CALL READ_VALUE_CHECK( Nfreq, FILE_NAME, 'Nfreq', DefaultValue=45 )


           IF ( Wavemaker(5:6).EQ.'1D' ) THEN
              Ntheta    = 1
              ThetaPeak = 0.0_SP
           ELSE
              CALL READ_VALUE_CHECK( Ntheta     , FILE_NAME, 'Ntheta'     , DefaultValue=24      )
              CALL READ_VALUE_CHECK( ThetaPeak  , FILE_NAME, 'ThetaPeak'  , DefaultValue=0.0_SP  )
              CALL READ_VALUE_CHECK( Sigma_Theta, FILE_NAME, 'Sigma_Theta', DefaultValue=10.0_SP )
           END IF
        END IF

        CALL READ_VALUE_CHECK( Ywidth_WK, FILE_NAME, 'Ywidth_WK' , DefaultValue=LARGE )
     END IF ! endif wk_irr 

     ! Check this is the correct placement of code
     CALL READ_VALUE_CHECK( ETA_LIMITER,FILE_NAME, 'ETA_LIMITER', DefaultValue=.FALSE. )
     IF( ETA_LIMITER ) THEN
        CALL READ_VALUE_CHECK( CrestLimit , FILE_NAME, 'CrestLimit'  )
        CALL READ_VALUE_CHECK( TroughLimit, FILE_NAME, 'TroughLimit' )
     END IF

     IF( WaveMaker(1:9).EQ.'WK_DATA2D' ) THEN

        CALL READ_VALUE_CHECK( Xc_WK       , FILE_NAME, 'Xc_WK'                                        )
        CALL READ_VALUE_CHECK( Yc_WK       , FILE_NAME, 'Yc_WK'       , DefaultValue=0.0_SP            )
        CALL READ_VALUE_CHECK( DEP_WK      , FILE_NAME, 'DEP_WK'                                       )
        CALL READ_VALUE_CHECK( Time_ramp   , FILE_NAME, 'Time_ramp'   , DefaultValue=0.0_SP, Units='s' )
        CALL READ_VALUE_CHECK( Delta_WK    , FILE_NAME, 'Delta_WK'    , DefaultValue=0.5_SP            )
        CALL READ_VALUE_CHECK( WaveCompFile, FILE_NAME, 'WaveCompFile'                                 )
        CALL READ_VALUE_CHECK( Ywidth_WK   , FILE_NAME, 'Ywidth_WK'   , DefaultValue=LARGE             )

     ENDIF ! end wk_data2D

     CALL READ_VALUE_CHECK( EqualEnergy, FILE_NAME, 'EqualEnergy', DefaultValue=.FALSE. )

! absorbing generating wavemaker, more tests needed 
     IF ( WaveMaker(1:3 ).EQ.'ABS'.OR.&
          WaveMaker(1:11).EQ.'LEFT_BC_IRR') THEN

        ! Orginal code had no error checking, default value?
        CALL READ_VALUE_CHECK( WAVE_DATA_TYPE, FILE_NAME, 'WAVE_DATA_TYPE' )
        
        CALL READ_VALUE_CHECK_DELAY_DEFAULT( DEP_Ser, FILE_NAME, 'DepthWaveMaker', IsDelayed )

        IF ( IsDelayed ) THEN
           CALL WRITE_ALL( 'WARNING: DepthWaveMaker not specified, reading DEP_WK instead.' )
           CALL READ_VALUE_CHECK_DELAY_DEFAULT( DEP_Ser, FILE_NAME, 'DEP_WK', IsDelayed ) 
           IF ( IsDelayed ) THEN
              CALL WRITE_ALL( 'ERROR: DepthWaveMark and DEP_WK not specified, specify either. STOPPING SIMULATION.' )
              CALL TERMINATE
           END IF

        END IF 

        IF( WaveMaker(1:3).EQ.'ABS' ) THEN
           ! NOTE: Original code had no error checks. 
           CALL READ_VALUE_CHECK( WidthWaveMaker    , FILE_NAME, 'WidthWaveMaker'     ) 
!  get R and A for sponge_wavemaker calculation  
           CALL READ_VALUE_CHECK( R_sponge_wavemaker, FILE_NAME, 'R_sponge_wavemaker' )
           CALL READ_VALUE_CHECK( A_sponge_wavemaker, FILE_NAME, 'A_sponge_wavemaker' )
        END IF

        IF ( WAVE_DATA_TYPE(1:4).EQ.'DATA' ) THEN
           CALL READ_VALUE_CHECK( WaveCompFile, FILE_NAME, 'WaveCompFile' )

           OPEN(1,FILE=TRIM(WaveCompFile))
           READ(1,*)NumFreq,NumDir
           ALLOCATE( Amp_Ser(NumFreq,NumDir)   , &
                     Per_Ser(NumFreq)          , &
                     Theta_Ser(NumDir)         , &
                     Phase_LEFT(NumFreq,NumDir) )

           READ(1,*)PeakPeriod  ! useless for this application but should keep for consistency
           DO J=1, NumFreq
             READ(1,*)Per_Ser(J)  ! read in as frequency
!print*,J,Per_Ser(J)
           END DO
           DO I=1, NumDir
             READ(1,*)Theta_Ser(I)
           END DO
           DO I=1, NumDir
              READ(1,*)(Amp_Ser(J,I),J=1,NumFreq)
!print*,I,Amp_Ser(J,I)
           END DO
           DO I=1, NumDir
              READ(1,*,END=991)(Phase_LEFT(J,I),J=1,NumFreq)
           END DO
           CLOSE(1)
! NOTE: Do the below two lines do anything?
881  INPUT_PHASE = .TRUE.
991  CONTINUE

           DO J = 1, NumFreq
              DO I = 1, NumDir

                 IF( INPUT_PHASE ) THEN
                    Phase_LEFT(J,I)=Phase_LEFT(J,I)*3.1415926/180.0_SP
                 ELSE
# if defined (INTEL)
                    Phase_LEFT(J,I)=rand()*2.0_SP*3.1415926
# elif defined (CRAY)
                    Phase_LEFT(J,I)=rand()*2.0_SP*3.1415926
# else
                    Phase_LEFT(J,I)=rand(0)*2.0_SP*3.1415926
# endif
                 END IF

              END DO
           END DO

! to make consistent with cm and sm approach we use phase_ser which is 
! only is random with frequency
           ALLOCATE( Phase_Ser(NumFreq) )
           ALLOCATE( Segma_Ser(NumFreq), Wave_Number_Ser(NumFreq) )
           DO J = 1 ,NumFreq
              Phase_Ser(J)=Phase_LEFT(J,1)
              IF (Per_Ser(J).EQ.ZERO ) THEN
                 WRITE(*,*) 'wave frequency is zero, STOP'
                 STOP
              ELSE
                 Per_Ser(J)=1.0_SP/Per_Ser(J)
              END IF
           END DO

           DO I = 1, NumDir
              Theta_Ser(I)=Theta_Ser(I)*DEG2RAD
           END DO  


! ORIGINAL CODE - NOTE: Inconsistent messages 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A40)')'absorbing generating wave maker'
      if (myid.eq.0) WRITE(3,'(A40)')'use DATA'
# else
      WRITE(3,'(A40)')'abs or left_bc_irr wave maker'
      WRITE(3,'(A40)')'use DATA'
# endif
! END ORIGINAL
        ELSE ! use TMA or JON

           CALL READ_VALUE_CHECK( FreqPeak, FILE_NAME, 'FreqPeak'                       )
           CALL READ_VALUE_CHECK( FreqMin , FILE_NAME, 'FreqMin'                        )
           CALL READ_VALUE_CHECK( FreqMax , FILE_NAME, 'FreqMax'                        )
           CALL READ_VALUE_CHECK( Hmo     , FILE_NAME, 'Hmo'                            )
           CALL READ_VALUE_CHECK( GammaTMA, FILE_NAME, 'GammaTMA' , DefaultValue=3.3_SP )
           CALL READ_VALUE_CHECK( Nfreq    ,FILE_NAME, 'Nfreq'    , DefaultValue=45     )

           IF ( WAVE_DATA_TYPE(5:6).EQ.'1D' ) THEN
              Ntheta = 1
              ThetaPeak = 0.0_SP
           ELSE
              CALL READ_VALUE_CHECK( Ntheta     , FILE_NAME, 'Ntheta'     , DefaultValue=24      )
              CALL READ_VALUE_CHECK( ThetaPeak  , FILE_NAME, 'ThetaPeak'  , DefaultValue=0.0_SP  )
              CALL READ_VALUE_CHECK( Sigma_Theta, FILE_NAME, 'Sigma_Theta', DefaultValue=10.0_SP )
           END IF 

        END IF ! end if data or tma

     ENDIF ! end absorbing-generating or left bc wave maker

     CALL FLUSH_TMP_LOG    


! ---------------------------------------
! -         Perodic BC Info             -
! ---------------------------------------

     CALL WRITE_LOG( LOG_BANNER( 'PERIODIC BC' ) )
# if defined (CARTESIAN)
! south-north periodic boundary condition
     CALL READ_VALUE_CHECK( PERIODIC, FILE_NAME, 'PERIODIC', DefaultValue=.FALSE. )
# endif
     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -           Sponge Info               -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'SPONGE' ) )

     CALL READ_VALUE_CHECK( DIFFUSION_SPONGE, FILE_NAME, 'DIFFUSION_SPONGE', DefaultValue=.FALSE. )
     CALL READ_VALUE_CHECK( DIRECT_SPONGE   , FILE_NAME, 'DIRECT_SPONGE'   , DefaultValue=.FALSE. )
     CALL READ_VALUE_CHECK( FRICTION_SPONGE , FILE_NAME, 'FRICTION_SPONGE' , DefaultValue=.FALSE. )

      IF ( DIRECT_SPONGE ) CALL WRITE_ALL( 'Direct sponge was set.' )

      IF ( DIFFUSION_SPONGE ) THEN
         CALL WRITE_ALL( 'Diffusion sponge was set.' )
         CALL READ_VALUE_CHECK( Csp, FILE_NAME, 'Csp', DefaultValue=0.1_SP )
      END IF ! end diffusion_sponge

      IF ( FRICTION_SPONGE ) THEN
         CALL WRITE_ALL( 'Friction spong was set.' )
         CALL READ_VALUE_CHECK( CDsponge, FILE_NAME, 'CDsponge', DefaultValue=0.5_SP )
      ENDIF  ! endif friction_sponge


      IF( DIFFUSION_SPONGE.OR.DIRECT_SPONGE.OR.FRICTION_SPONGE ) THEN
        CALL READ_VALUE_CHECK( Sponge_west_width , FILE_NAME, 'Sponge_west_width' , DefaultValue=0.0_SP , Units='m' )
        CALL READ_VALUE_CHECK( Sponge_east_width , FILE_NAME, 'Sponge_east_width' , DefaultValue=0.0_SP , Units='m' )
        CALL READ_VALUE_CHECK( Sponge_south_width, FILE_NAME, 'Sponge_south_width', DefaultValue=0.0_SP , Units='m' )
        CALL READ_VALUE_CHECK( Sponge_north_width, FILE_NAME, 'Sponge_north_width', DefaultValue=0.0_SP , Units='m' )
        CALL READ_VALUE_CHECK( R_sponge          , FILE_NAME, 'R_sponge'          , DefaultValue=0.85_SP            )
        CALL READ_VALUE_CHECK( A_sponge          , FILE_NAME, 'A_sponge'          , DefaultValue=5.0_SP             )
      END IF ! endif sponge

! to avoid longshore current caused by extra momentum flux
! we can add bottom friction make momentum balance
! [mayhl] Is this code in the right place?
     CALL READ_VALUE_CHECK_DELAY_DEFAULT(WaveMakerCd,FILE_NAME,'WaveMakerCd', IsDelayed )
        
     IF ( IsDelayed ) THEN
        WaveMakerCurrentBalance=.FALSE.
        CALL WRITE_LOG( 'No Wavemaker current balance.' )
     ELSE
        WaveMakerCurrentBalance=.TRUE.
     END IF

     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -   Obstacle and Breakwater Info      -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'OBSTACLE and BREAKWATER' ) )

! obstacle structures
     CALL READ_VALUE_CHECK_DELAY_DEFAULT( OBSTACLE_FILE, FILE_NAME, 'OBSTACLE_FILE', IsDelayed )
     OBSTACLE=.NOT.IsDelayed
     IF ( .NOT.OBSTACLE ) CALL WRITE_LOG( 'OBSTACLE_FILE: Not specified' )
        
! breakwater
     CALL READ_VALUE_CHECK_DELAY_DEFAULT( BREAKWATER_FILE, FILE_NAME, 'BREAKWATER_FILE', IsDelayed )
     BREAKWATER=.NOT.IsDelayed
     IF (.NOT.BREAKWATER) CALL WRITE_LOG( 'BREAKWATER_FILE: Not specified.' )
 
! breakwater reflection stength
     ! NOTE: Should this be read only when BREAKWATER file is set?
     CALL READ_VALUE_CHECK( BreakWaterAbsorbCoef, FILE_NAME, 'BreakWaterAbsorbCoef', DefaultValue=10.0_SP )

     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -          Physics Info               -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'PHYSICS' ) )

     CALL READ_VALUE_CHECK( DISPERSION, FILE_NAME, 'DISPERSION', DefaultValue=.TRUE.    )
     CALL READ_VALUE_CHECK( Gamma1    , FILE_NAME, 'Gamma1'    , DefaultValue=1.0_SP    ) 
# if defined (CARTESIAN)
     CALL READ_VALUE_CHECK( Gamma2    , FILE_NAME, 'Gamma2'    , DefaultValue=1.0_SP    )
# endif 
# if defined (CARTESIAN) || defined (ZALPHA)
     CALL READ_VALUE_CHECK( Beta_ref  , FILE_NAME, 'Beta_ref'  , DefaultValue=-0.531_SP )
# endif
     CALL READ_VALUE_CHECK( Gamma3    , FILE_NAME, 'Gamma3'    , DefaultValue=1.0_SP    )


     CALL READ_VALUE_CHECK( VISCOSITY_BREAKING, FILE_NAME, 'VISCOSITY_BREAKING', DefaultValue=.TRUE. )

     IF (ROLLER.AND.(VISCOSITY_BREAKING.EQV..FALSE.) ) THEN
        CALL WRITE_ALL( 'WARNING: ROLLER set to TRUE, but VISCOSITY_BREAKING set to FALSE. '//&
                        'Switching to VISCOSITY_BREAKING to TRUE.'                              )
        VISCOSITY_BREAKING=.TRUE.
     END IF

     IF ( VISCOSITY_BREAKING ) CALL READ_VALUE_CHECK( SWE_ETA_DEP, FILE_NAME, 'SWE_ETA_DEP', DefaultValue=0.8_SP )

     CALL READ_VALUE_CHECK( IN_Cd, FILE_NAME, 'FRICTION_MATRIX', DefaultValue=.FALSE. )

     IF ( IN_Cd ) THEN
        CALL READ_VALUE_CHECK( CD_FILE , FILE_NAME, 'FRICTION_FILE'                      )
     ELSE
        CALL READ_VALUE_CHECK( Cd_fixed, FILE_NAME, 'Cd'           , DefaultValue=0.0_SP )
     END IF

     CALL FLUSH_TMP_LOG


! ---------------------------------------
! -          Numerics Info              -
! ---------------------------------------
     CALL WRITE_LOG( LOG_BANNER( 'NUMERICS' ) )

     CALL READ_VALUE_CHECK( Time_Scheme, FILE_NAME, 'Time_Scheme' , DefaultValue='Runge_Kutta' )
     CALL READ_VALUE_CHECK( CONSTR     , FILE_NAME, 'CONSTRUCTION', DefaultValue='HLLC'        )
     CALL READ_VALUE_CHECK( HIGH_ORDER , FILE_NAME, 'HIGH_ORDER'  , DefaultValue='FOURTH'      )
     CALL READ_VALUE_CHECK( CFL        , FILE_NAME, 'CFL'         , DefaultValue=0.5_SP        )

     CALL READ_VALUE_CHECK_DELAY_DEFAULT( DT_fixed, FILE_NAME, 'DT_fixed', IsDelayed )

     ! NOTE: Add message about choosing DT
     FIXED_DT=.NOT.IsDelayed 

     CALL READ_VALUE_CHECK( FroudeCap  , FILE_NAME, 'FroudeCap'  , DefaultValue=3.0_SP            ) 
     CALL READ_VALUE_CHECK( MinDepth   , FILE_NAME, 'MinDepth'   , DefaultValue=0.1_SP, Units='m' )
     CALL READ_VALUE_CHECK( MinDepthFrc, FILE_NAME, 'MinDepthFrc', DefaultValue=0.1_SP, Units='m' )

     CALL FLUSH_TMP_LOG

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


!          write (iunit,'(20E16.5E4)') time, eta_sta,&
!                          u_sta,v_sta
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

!       ALLOCATE(envelope(Mloc,Nloc))
!       envelope = 0.d0
       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2
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
             iunit=100+i
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
             iunit=100+i
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
        call PutFile(TMP_NAME,CH *Sdensity*1000.0_SP)
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
        ENDIF
        IF(OUT_Vmean)THEN
          TMP_NAME = TRIM(FDIR)//'vmean_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,Vmean)
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


