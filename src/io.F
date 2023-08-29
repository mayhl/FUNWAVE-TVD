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
      CALL STATIONS_V2
!# if defined (SPHERICAL_IJ_STATION)
!      ! use IJ for coupling purpose
!      CALL STATIONS_SPHERICAL_IJ
!# else
!      CALL STATIONS
!# endif
    END IF
  END IF
! preview

  IF (TIME >= PLOT_START_TIME) THEN

    PLOT_COUNT = PLOT_COUNT + DT
    IF (PLOT_COUNT >= PLOT_INTV) THEN
      PLOT_COUNT = PLOT_COUNT - PLOT_INTV
      CALL PREVIEW
    ENDIF
# if defined (VESSEL)
    PLOT_COUNT_VESSEL = PLOT_COUNT_VESSEL + DT
    IF (PLOT_COUNT_VESSEL >= PLOT_INTV_VESSEL) THEN
      PLOT_COUNT_VESSEL = PLOT_COUNT_VESSEL - PLOT_INTV_VESSEL
      CALL OUTPUT_VESSEL
    ENDIF
# endif
# if defined (SEDIMENT)
    PLOT_COUNT_SEDIMENT = PLOT_COUNT_SEDIMENT + DT
    IF (PLOT_COUNT_SEDIMENT >= PLOT_INTV_SEDIMENT) THEN
      PLOT_COUNT_SEDIMENT = PLOT_COUNT_SEDIMENT - PLOT_INTV_SEDIMENT
      CALL OUTPUT_SEDIMENT
    ENDIF
# endif
# if defined (TRACKING)
    PLOT_COUNT_TRACKING = PLOT_COUNT_TRACKING + DT
    IF (PLOT_COUNT_TRACKING >= PLOT_INTV_TRACKING) THEN
      PLOT_COUNT_TRACKING = PLOT_COUNT_TRACKING - PLOT_INTV_TRACKING
      CALL OUTPUT_TRACKING
    ENDIF
# endif
# if defined (FOAM)
    PLOT_COUNT_FOAM = PLOT_COUNT_FOAM + DT
    IF (PLOT_COUNT_FOAM >= PLOT_INTV_FOAM) THEN
      PLOT_COUNT_FOAM = PLOT_COUNT_FOAM - PLOT_INTV_FOAM
      CALL OUTPUT_FOAM
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
  USE LOG_IO
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
  ! Flag for bypassing error when input value is not
  ! found and returns if input value is found
  LOGICAL :: found, found2
  ! Dummy variable for storing default float value
  REAL(SP) :: def_flt
  !>by Zhouteng Ye
  CHARACTER(LEN=80)::INPUT_NAME = ''

![ykchoi
  CHARACTER(LEN=80)::FDIR = ' '
!ykchoi]

# if defined (PARALLEL)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ier)   !ykchoi(04/May/2017)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
# endif

  FDIR = TRIM(RESULT_FOLDER)
  !mayhl: Moved to preview 
  !OPEN (10000, FILE='time_dt.out', STATUS='UNKNOWN')
  OPEN(LU, FILE='LOG.txt')

! read everything from input.txt

  !>by Zhouteng Ye
  !> Get the argument from the command.
  !> If no input in command, file name is 'input.txt' (same as before)
  !> If the input comes with other name, read the correponding file
  CALL GETARG(1, INPUT_NAME)
  IF (INPUT_NAME .EQ. '') THEN
    FILE_NAME = 'input.txt'
  ELSE
    FILE_NAME = INPUT_NAME
  ENDIF
  INPUT_FILE_NAME = FILE_NAME

! title
  CALL SET_INPUT_LOGGER(FILE_NAME, logbuff)

  CALL BWRITE('LOG FILE')

  CALL READ_PARAMETER(TITLE, 'TITLE', '---TEST RUN---')
  CALL logbuff%CLEAR()

# if defined (PARALLEL)
! ----------------------------------------------------------------
  CALL BWRITE('PARALLEL')
! ----------------------------------------------------------------
  CALL READ_PARAMETER(PX, 'PX', 1)
  CALL READ_PARAMETER(PY, 'PY', 1)
  CALL logbuff%FLUSH2OUTPUT()
# endif

! ----------------------------------------------------------------
  CALL BWRITE('GRID INFO')
! ----------------------------------------------------------------
  CALL READ_PARAMETER(Mglob, 'Mglob')
  CALL READ_PARAMETER(Nglob, 'Nglob')

# if defined (CARTESIAN)
  CALL READ_PARAMETER(DX, 'DX')
  CALL READ_PARAMETER(DY, 'DY')
#else
  CALL READ_PARAMETER(StretchGrid, 'StretchGrid', .FALSE.)

  IF (StretchGrid) THEN

    CALL LWRITE('Stretch Grid Mode')
    CALL READ_PARAMETER(DX_FILE       ,'DX_FILE'       ,is_file=.TRUE.)
    CALL READ_PARAMETER(DY_FILE       ,'DY_FILE'       ,is_file=.TRUE.)
    CALL READ_PARAMETER(Coriolis_FILE ,'CORIOLIS_FILE' ,is_file=.TRUE.)

  ELSE

    CALL LWRITE('Spherical Grid Mode'
    CALL READ_PARAMETER(Lon_West , 'Lon_West' )
    CALL READ_PARAMETER(Lat_South, 'Lat_South')
    CALL READ_PARAMETER(Dphi     , 'Dphi'     )
    CALL READ_PARAMETER(Dtheta   , 'Dtheta'   )

  END IF
# endif
  CALL logbuff%FLUSH2OUTPUT()

  ! ----------------------------------------------------------------
  CALL BWRITE('BATHYMETRY INFO')
  ! ----------------------------------------------------------------

  CALL READ_PARAMETER(DEPTH_TYPE, 'DEPTH_TYPE', 'FLAT')

  IF (DEPTH_TYPE(1:3) == 'DAT') THEN

    CALL READ_PARAMETER(DEPTH_FILE, 'DEPTH_FILE', is_file=.TRUE.)

  ELSE IF (DEPTH_TYPE(1:3) == 'FLA') THEN

    CALL READ_PARAMETER(DEPTH_FLAT, 'DEPTH_FLAT', 10.0_SP, 'm')

  ELSE IF (DEPTH_TYPE(1:3) == 'SLO') THEN

    CALL READ_PARAMETER(DEPTH_FLAT, 'DEPTH_FLAT', 10.0_SP, 'm')
    CALL READ_PARAMETER(SLP       , 'SLP'       , 0.1_SP      )
    CALL READ_PARAMETER(Xslp      , 'Xslp'      , 0.0_SP , 'm')

  ELSE
    CALL SWRITE("INVALID DEPTH_TYPE '"//TRIM(DEPTH_TYPE)//"'.")
  END IF

  CALL READ_PARAMETER(WaterLevel, 'WaterLevel', 0.0_SP, 'm')

  CALL READ_PARAMETER(BATHY_CORRECTION, 'BATHY_CORRECTION', .FALSE.)
  IF (BATHY_CORRECTION) CALL LWRITE('Bathymetry is being corrected !')

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('TIME INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(TOTAL_TIME     , 'TOTAL_TIME'     , units='s'      )
  CALL READ_PARAMETER(PLOT_START_TIME, 'PLOT_START_TIME', 0.0_SP    , 's')
  CALL READ_PARAMETER(PLOT_INTV      , 'PLOT_INTV'      , 1.0_SP    , 's')
  ! mayhl: Consolidated station related parameters to their own section
  CALL READ_PARAMETER(SCREEN_INTV    , 'SCREEN_INTV'    , 1.0_SP    , 's')

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('HOT START INFO ')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(INI_UVZ, 'INI_UVZ', .FALSE.)

  IF (INI_UVZ) THEN

    CALL READ_PARAMETER(BED_DEFORMATION, 'BED_DEFORMATION', .FALSE.)
    IF (BED_DEFORMATION) CALL LWRITE('Bathymetry is adjusted based on Bed deformation.')

    CALL READ_PARAMETER(ETA_FILE, 'ETA_FILE', is_file=.TRUE.)

    CALL READ_PARAMETER(U_FILE, 'U_FILE', is_file=.TRUE., found=found)
    NO_UV_FILE = .NOT. found
    CALL READ_PARAMETER(V_FILE, 'V_FILE', is_file=.TRUE., found=found)
    IF (NO_UV_FILE .AND. found) THEN
      CALL SWRITE('NO U_FILE SPECIFIED, BUT V_FILE SPECIFIED.')
    END IF

    CALL READ_PARAMETER(MASK_FILE, 'MASK_FILE', is_file=.TRUE., found=found)
    NO_MASK_FILE = .NOT. found

    CALL READ_PARAMETER(HotStartTime, 'HotStartTime'     , 0.0_SP, 's')
    CALL READ_PARAMETER(icount      , 'OutputStartNumber', 1          )

    icount = icount - 1
    CALL logbuff%FLUSH2OUTPUT()
  ELSE
    CALL logbuff%CLEAR()
  END IF

  ! mayhl: Moved still water info to bathy

! mayhl: Consolidated station related parameters to their own section
! ----------------------------------------------------------------
  CALL BWRITE('STATIONS INFO ')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(NumberStations, 'NumberStations', 0)
  IF (NumberStations .GT. 0) THEN
    CALL READ_PARAMETER(STATIONS_FILE      , 'STATIONS_FILE'                   )
    CALL READ_PARAMETER(PLOT_INTV_STATION  , 'PLOT_INTV_STATION'  , 1.0_SP, 's')
    CALL READ_PARAMETER(StationOutputBuffer, 'StationOutputBuffer', 1000       )
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('WAVEMAKER INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(WaveMaker, 'WAVEMAKER', 'nothing')

  IF (WaveMaker(1:7) .EQ. 'nothing') THEN

    CALL LWRITE('No Wavemaker')

  ELSEIF (WaveMaker(1:7) == 'LEF_SOL') THEN

    CALL READ_PARAMETER(AMP_SOLI, 'AMP'                 )
    CALL READ_PARAMETER(DEP_SOLI, 'DEP'                 )
    CALL READ_PARAMETER(LAG_SOLI, 'LAGTIME', 0.0_SP, 's')

  ELSE IF (WaveMaker(1:7) == 'WK_TIME') THEN

    CALL READ_PARAMETER(NumWaveComp , 'NumWaveComp'                       )
    CALL READ_PARAMETER(PeakPeriod  , 'PeakPeriod'                        )
    CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile', is_file=.TRUE.      )
    CALL READ_PARAMETER(Xc_WK       , 'Xc_WK'                             )
    CALL READ_PARAMETER(Yc_WK       , 'Yc_WK'       , 0.0_SP         , 'm')
    CALL READ_PARAMETER(DEP_WK      , 'DEP_WK'                            )
    CALL READ_PARAMETER(Time_ramp   , 'Time_ramp'   , 0.0_SP         , 's')
    CALL READ_PARAMETER(Delta_WK    , 'Delta_WK'    , 0.5_SP              )
    CALL READ_PARAMETER(Ywidth_WK   , 'Ywidth_WK'   , LARGE               )

  ELSE IF (WaveMaker(1:7) == 'INI_SOL') THEN

    CALL READ_PARAMETER(SolitaryPositiveDirection, 'SolitaryPositiveDirection', .TRUE.)

    IF (SolitaryPositiveDirection) THEN
      CALL LWRITE('Solitary wave propagate in + X direction')
    ELSE
      CALL LWRITE('Solitary wave propagate in - X direction')
    END IF

    CALL READ_PARAMETER(AMP_SOLI  , 'AMP'       )
    CALL READ_PARAMETER(DEP_SOLI  , 'DEP'       )
    CALL READ_PARAMETER(XWAVEMAKER, 'XWAVEMAKER')

  ELSE IF (WaveMaker(1:6) == 'N_WAVE') THEN

    CALL READ_PARAMETER(x1_Nwave   , 'x1_Nwave'   )
    CALL READ_PARAMETER(x2_Nwave   , 'x2_Nwave'   )
    CALL READ_PARAMETER(a0_Nwave   , 'a0_Nwave'   )
    CALL READ_PARAMETER(gamma_Nwave, 'gamma_Nwave')
    CALL READ_PARAMETER(dep_Nwave  , 'dep_Nwave'  )

  ELSE IF (WaveMaker(1:7) == 'INI_REC') THEN

    CALL READ_PARAMETER(AMP_SOLI, 'AMP'              )
    CALL READ_PARAMETER(Xc      , 'Xc'               )
    CALL READ_PARAMETER(Yc      , 'Yc'  , 0.0_SP, 'm')
    CALL READ_PARAMETER(WID     , 'WID'              )

  ELSE IF (WaveMaker(1:7) == 'INI_GAU' .OR. &
           WaveMaker(1:7) == 'INI_DIP') THEN

    CALL READ_PARAMETER(AMP_SOLI, 'AMP')
    CALL READ_PARAMETER(Xc      , 'Xc' )
    CALL READ_PARAMETER(Yc      , 'Yc' )
    CALL READ_PARAMETER(WID     , 'WID')

  ELSE IF (WaveMaker(1:6) == 'WK_REG') THEN

    CALL READ_PARAMETER(Xc_WK    , 'Xc_WK'                  )
    CALL READ_PARAMETER(Yc_WK    , 'Yc_WK'    , 0.0_SP , 'm')
    CALL READ_PARAMETER(Tperiod  , 'Tperiod'                )
    CALL READ_PARAMETER(AMP_WK   , 'AMP_WK'                 )
    CALL READ_PARAMETER(DEP_WK   , 'DEP_WK'                 )
    CALL READ_PARAMETER(Theta_WK , 'Theta_WK' , 0.0_SP      )
    CALL READ_PARAMETER(Time_ramp, 'Time_ramp', 0.0_SP , 's')
    CALL READ_PARAMETER(Delta_WK , 'Delta_WK' , 0.5_SP      )
    CALL READ_PARAMETER(Ywidth_WK, 'Ywidth_WK', LARGE       )

  ELSE IF (WaveMaker(1:6) == 'WK_IRR' .OR. WaveMaker(1:6) == 'TMA_1D' .OR. &
           WaveMaker(1:6) == 'JON_1D' .OR. WaveMaker(1:6) == 'JON_2D') THEN

    CALL READ_PARAMETER(Xc_WK    , 'Xc_WK'                  )
    CALL READ_PARAMETER(Yc_WK    , 'Yc_WK'    , 0.0_SP , 'm')
    CALL READ_PARAMETER(DEP_WK   , 'DEP_WK'                 )
    CALL READ_PARAMETER(Time_ramp, 'Time_ramp', 0.0_SP , 's')
    CALL READ_PARAMETER(Delta_WK , 'Delta_WK' , 0.5_SP      )
    CALL READ_PARAMETER(FreqPeak , 'FreqPeak'               )
    CALL READ_PARAMETER(FreqMin  , 'FreqMin'                )
    CALL READ_PARAMETER(FreqMax  , 'FreqMax'                )
    CALL READ_PARAMETER(Hmo      , 'Hmo'                    )
    CALL READ_PARAMETER(GammaTMA , 'GammaTMA' , 3.3_SP      )
    CALL READ_PARAMETER(Nfreq    , 'Nfreq'    , 45          )

    IF (WaveMaker(1:6) == 'TMA_1D' .OR. &
        WaveMaker(1:6) == 'JON_1D') THEN
      Ntheta = 1
      ThetaPeak = 0.0_SP
      Sigma_Theta = 10.0_SP
    ELSE
      CALL READ_PARAMETER(Ntheta     , 'Ntheta'     , 24     )
      CALL READ_PARAMETER(ThetaPeak  , 'ThetaPeak'  , 0.0_SP )
      CALL READ_PARAMETER(Sigma_Theta, 'Sigma_Theta', 10.0_SP)
    END IF

    CALL READ_PARAMETER(Ywidth_WK  , 'Ywidth_WK'  , LARGE  )
    CALL READ_PARAMETER(EqualEnergy, 'EqualEnergy', .FALSE.)

  ELSE IF (WaveMaker(1:13) == 'WK_NEW_DATA2D' .OR. &
           WaveMaker(1:9) == 'WK_DATA2D') THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

    CALL READ_PARAMETER(Xc_WK       , 'Xc_WK'                             )
    CALL READ_PARAMETER(Yc_WK       , 'Yc_WK'       , 0.0_SP         , 'm')
    CALL READ_PARAMETER(DEP_WK      , 'DEP_WK'                            )
    CALL READ_PARAMETER(Time_ramp   , 'Time_ramp'   , 0.0_SP         , 's')
    CALL READ_PARAMETER(Delta_WK    , 'Delta_WK'    , 0.5_SP              )
    CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile', is_file=.TRUE.      )
    CALL READ_PARAMETER(Ywidth_WK   , 'Ywidth_WK'   , LARGE               )
    CALL READ_PARAMETER(EqualEnergy , 'EqualEnergy' , .FALSE.             )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! absorbing generating wavemaker, more tests needed
  ELSE IF (WaveMaker(1:3) == 'ABS' .OR. &
           WaveMaker(1:11) == 'LEFT_BC_IRR') THEN

    CALL READ_PARAMETER(WAVE_DATA_TYPE, 'WAVE_DATA_TYPE'              )
    CALL READ_PARAMETER(DEP_Ser       , 'DepthWaveMaker' , found=found)

    IF (.NOT. found) THEN
      CALL LWRITE("DepthWaveMaker not found, attempting to read DEP_WK")
      CALL READ_PARAMETER(DEP_Ser, 'DEP_WK', found=found)
      IF (.NOT. found) THEN
        CALL SWRITE("Neither DepthWaveMaker or DEP_WK specified.")
      END IF
    END IF

    IF (WAVE_DATA_TYPE(1:3) == 'ABS') THEN

      CALL READ_PARAMETER(WidthWaveMaker    , 'WidthWaveMaker'    )
      CALL READ_PARAMETER(R_sponge_wavemaker, 'R_sponge_wavemaker')
      CALL READ_PARAMETER(A_sponge_wavemaker, 'A_sponge_wavemaker')

    ELSE IF (WAVE_DATA_TYPE(1:4) == 'DATA') THEN

      CALL READ_PARAMETER(WaveCompFile, 'WaveCompFile', is_file=.TRUE.)
      ! mayhl: move WaveCompFile read code to seperate subroutine
      !        to improve readability
      CALL READ_ABS_OR_LEFT_BC_COMPFILE(INPUT_PHASE)

    ELSE IF (WAVE_DATA_TYPE(1:6) == 'WK_IRR' .OR. WAVE_DATA_TYPE(1:6) == 'TMA_1D' .OR. &
             WAVE_DATA_TYPE(1:6) == 'JON_1D' .OR. WAVE_DATA_TYPE(1:6) == 'JON_2D') THEN

      CALL READ_PARAMETER(FreqPeak, 'FreqPeak'         )
      CALL READ_PARAMETER(FreqMin , 'FreqMin'          )
      CALL READ_PARAMETER(FreqMax , 'FreqMax'          )
      CALL READ_PARAMETER(Hmo     , 'Hmo'              )
      CALL READ_PARAMETER(Nfreq   , 'Nfreq'    , 45    )
      CALL READ_PARAMETER(GammaTMA, 'GammaTMA' , 3.3_SP)

      IF (WAVE_DATA_TYPE(1:6) == 'TMA_1D' .OR. &
          WAVE_DATA_TYPE(1:6) == 'JON_1D') THEN
        Ntheta = 1
        ThetaPeak = 0.0_SP
      ELSE
        CALL READ_PARAMETER(Ntheta     , 'Ntheta'     , 24     )
        CALL READ_PARAMETER(ThetaPeak  , 'ThetaPeak'  , 0.0_SP )
        CALL READ_PARAMETER(Sigma_Theta, 'Sigma_Theta', 10.0_SP)
      END IF

    ELSE
      ! Added explicit else case to avoid invalid input
      CALL SWRITE("Invalid WAVE_DATA_TYPE '"//TRIM(WAVE_DATA_TYPE)//".")
    END IF

  ELSE IF (WaveMaker(1:10) == 'WK_NEW_IRR') THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!START!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Oct, 2021
!Salatin, R., Chen, Q., Bak, A. S., Shi, F., & Brandt, S. R. (2021). Effects of
!wave coherence on longshore variability of nearshore wave processes. Journal
!of Geophysical Research: Oceans,126, e2021JC017641.
!https://doi.org/10.1029/2021JC017641

    ! mayhl moved WK_NEW_IRR from end of READ_INPUT to here
    CALL READ_PARAMETER(Xc_WK      ,'Xc_WK'                    )
    CALL READ_PARAMETER(Yc_WK      ,'Yc_WK'      , 0.0_SP , 'm')
    CALL READ_PARAMETER(DEP_WK     ,'DEP_WK'                   )
    CALL READ_PARAMETER(Time_ramp  ,'Time_ramp'  , 0.0_SP , 's')
    CALL READ_PARAMETER(Delta_WK   ,'Delta_WK'   , 0.5_SP      )
    CALL READ_PARAMETER(FreqPeak   ,'FreqPeak'                 )
    CALL READ_PARAMETER(FreqMin    ,'FreqMin'                  )
    CALL READ_PARAMETER(FreqMax    ,'FreqMax'                  )
    CALL READ_PARAMETER(Hmo        ,'Hmo'                      )
    CALL READ_PARAMETER(GammaTMA   ,'GammaTMA'   , 3.3_SP      )
    CALL READ_PARAMETER(Nfreq      ,'Nfreq'      , 1080        )
    CALL READ_PARAMETER(Ntheta     ,'Ntheta'     , 24          )
    CALL READ_PARAMETER(ThetaPeak  ,'ThetaPeak'  , 0.0_SP      )
    CALL READ_PARAMETER(Sigma_Theta,'Sigma_Theta', 10.0_SP     )
    CALL READ_PARAMETER(Ywidth_WK  ,'Ywidth_WK'  , LARGE       )
    CALL READ_PARAMETER(alpha_c    ,'alpha_c'    , 0.0_SP , '%')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELSE
    ! Added explicit else case to avoid invalid input
    CALL SWRITE("Invalid WaveMaker '"//TRIM(WaveMaker)//"'.")
  END IF

  ! mayhl: Fengyan mention values are no longer relevant 
  ! IF (.NOT. (WaveMaker(1:7) .EQ. 'nothing')) THEN
  !
  !   CALL READ_PARAMETER(ETA_LIMITER, 'ETA_LIMITER', .FALSE.)
  !   IF (ETA_LIMITER) THEN
  !     CALL READ_PARAMETER(CrestLimit, 'CrestLimit')
  !     CALL READ_PARAMETER(TroughLimit, 'TroughLimit')
  !   END IF
  !
  !   CALL READ_PARAMETER(WaveMakerCD, 'WaveMakerCd', found=WaveMakerCurrentBalance)
  !
  ! END IF

  CALL logbuff%FLUSH2OUTPUT()

# if defined (CARTESIAN)
! ----------------------------------------------------------------
  CALL BWRITE('PERIODIC BC INFO')
! ----------------------------------------------------------------
  CALL READ_PARAMETER(PERIODIC, 'PERIODIC', .FALSE.)
  CALL logbuff%FLUSH2OUTPUT()
# endif

! ----------------------------------------------------------------
  CALL BWRITE('SPONGE LAYER BC INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(DIRECT_SPONGE, 'DIRECT_SPONGE', .FALSE.)

  IF (DIRECT_SPONGE) THEN
    CALL LWRITE('Direct sponge used')
    CALL READ_PARAMETER(R_sponge, 'R_sponge', 0.85_SP)
    CALL READ_PARAMETER(A_sponge, 'A_sponge', 5.0_SP )
  END IF

  CALL READ_PARAMETER(DIFFUSION_SPONGE, 'DIFFUSION_SPONGE', .FALSE.)

  IF (DIFFUSION_SPONGE) THEN
    CALL LWRITE('Diffusion sponge used')
    CALL READ_PARAMETER(Csp, 'Csp', 0.1_SP)
  END IF

  CALL READ_PARAMETER(FRICTION_SPONGE, 'FRICTION_SPONGE', .FALSE.)

  IF (FRICTION_SPONGE) THEN
    CALL LWRITE('Diffusion sponge used')
    CALL READ_PARAMETER(CDsponge, 'CDsponge', 5.0_SP)
  END IF

  IF (DIFFUSION_SPONGE .OR. DIRECT_SPONGE .OR. FRICTION_SPONGE) THEN
    CALL READ_PARAMETER(Sponge_west_width , 'Sponge_west_width' , 0.0_SP, 'm')
    CALL READ_PARAMETER(Sponge_east_width , 'Sponge_east_width' , 0.0_SP, 'm')
    CALL READ_PARAMETER(Sponge_south_width, 'Sponge_south_width', 0.0_SP, 'm')
    CALL READ_PARAMETER(Sponge_north_width, 'Sponge_north_width', 0.0_SP, 'm')
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('OBSTACLE & BREAKWATER INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(OBSTACLE_FILE, 'OBSTACLE_FILE', found=OBSTACLE)
  IF (.NOT. OBSTACLE) CALL LWRITE('No obstacle file.')

  CALL READ_PARAMETER(BREAKWATER_FILE, 'BREAKWATER_FILE', found=BREAKWATER)
  IF (.NOT. BREAKWATER) THEN
    CALL LWRITE('No breakwater file.')
  ELSE
    CALL READ_PARAMETER(BreakWaterAbsorbCoef, 'BreakWaterAbsorbCoef', 10.0_SP)
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('PHYSICS INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(DISPERSION, 'DISPERSION', .TRUE.)
  CALL READ_PARAMETER(Gamma1, 'Gamma1', 1.0_SP)
# if defined (CARTESIAN)
  CALL READ_PARAMETER(Gamma2  , 'Gamma2'  , 1.0_SP   )
  CALL READ_PARAMETER(Beta_ref, 'Beta_ref', -0.531_SP)
  CALL READ_PARAMETER(Gamma3  , 'Gamma3'  , 1.0_SP   )
# else
# if define (ZALPHA)
  ! mayhl: Is ZAPLPHA a dead preprocessor flag?
  CALL READ_PARAMETER(Beta_ref, 'Beta_ref', -0.531_SP)
# endif
# endif

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('WAVE BREAKING INFO')
! ----------------------------------------------------------------

! mayhl: ROLLER was read after this check.
  !        Moved wave breaking parameters to new section
  CALL READ_PARAMETER(ROLLER, 'ROLLER_EFFECT', .FALSE.)

  IF (ROLLER) THEN
    CALL LWRITE('Roller effects included, switching on VISCOSITY_BREAKING.')
    ROLLER_SWITCH = 1.0_SP
    VISCOSITY_BREAKING = .TRUE.
    CALL logbuf%ADD('VISCOSITY_BREAKING', FORMAT_LOGICAL(VISCOSITY_BREAKING))

  ELSE
    ROLLER_SWITCH = 0.0_SP
    CALL LWRITE('Roller effects not included.')
    CALL READ_PARAMETER(VISCOSITY_BREAKING, 'VISCOSITY_BREAKING', .TRUE.)
  END IF

  IF (VISCOSITY_BREAKING) THEN
    CALL LWRITE('Viscosity breaking included, switching on SHOW_BREAKING.')
    SHOW_BREAKING = .TRUE.
    CALL logbuf%ADD('SHOW_BREAKING', FORMAT_LOGICAL(SHOW_BREAKING))
  ELSE
    CALL READ_PARAMETER(SHOW_BREAKING, 'SHOW_BREAKING', .TRUE.)
    CALL READ_PARAMETER(SWE_ETA_DEP  , 'SWE_ETA_DEP'  , 0.8_SP)
  END IF

  ! mayhl: Check Cbrk1 and Cbrk2 are needed for no VISCOSITY_BREAKING
  !        Changed defaults from Cbrk1=0.65, Cbrk2=0.45 based on latest
  !        Wiki documentation Choi et al. (2018).
  IF (SHOW_BREAKING) THEN
    CALL READ_PARAMETER(Cbrk1, 'Cbrk1', 0.45_SP)
    CALL READ_PARAMETER(Cbrk2, 'Cbrk2', 0.35_SP)
  END IF

  ! mayhl: Default to Cbrk1?
  CALL READ_PARAMETER(WAVEMAKER_Cbrk, "WAVEMAKER_Cbrk", 1.0_SP)

  ![ykchoi(08.18.2015) : for viscosity of wavemaker
  CALL READ_PARAMETER(WAVEMAKER_VIS, 'WAVEMAKER_VIS', .FALSE.)

  IF (WAVEMAKER_VIS .AND. VISCOSITY_BREAKING) THEN
    CALL SWRITE('VISCOSITY_BREAKING AND WAVEMAKER_VIS CAN NOT BE BOTH INCLUDED.')
  END IF

  !  suggest dont use wavemaker_vis 04/30
  IF (WAVEMAKER_VIS) THEN
    CALL READ_PARAMETER(visbrk, 'visbrk')
    CALL READ_PARAMETER(WAVEMAKER_visbrk, 'WAVEMAKER_visbrk')
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('BOTTOM FRICTION INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(In_Cd, 'FRICTION_MATRIX', .FALSE.)

  IF (IN_Cd) THEN
    CALL READ_PARAMETER(CD_FILE, 'FRICTION_FILE', is_file=.TRUE.)
  ELSE
    CALL READ_PARAMETER(Cd_fixed, 'Cd', 0.0_SP)
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! ----------------------------------------------------------------
  CALL BWRITE('NUMERICS INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(Time_Scheme, 'Time_Scheme', 'Runge_Kutta')

  IF ((Time_Scheme .NE. 'Predictor_Corrector') .AND. &
      (Time_Scheme .NE. 'Runge_Kutta')) THEN
    CALL SWRITE("INVALID TIME SCHEME OPTION '"//TRIM(Time_Scheme)//"'")
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

  CALL READ_PARAMETER(DT_fixed, 'DT_fixed', found=FIXED_DT)
  IF (FIXED_DT) THEN
    CALL logbuf%ADD('FIXED_DT', FORMAT_LOGICAL(FIXED_DT))
    CALL LWRITE('Used fixed DT, but judged by CFL. IF not satisfy CLF, DT/2...')
  END IF

  CALL READ_PARAMETER(FroudeCap, 'FroudeCap', 3.0_SP)

  ! mayhl: Some what of a hacking solution. First check if
  !        parameters are in input file
  CALL READ_PARAMETER(MinDepth   , 'MinDepth'   , found=found )
  CALL READ_PARAMETER(MinDepthFrc, 'MinDepthFrc', found=found2)

  IF (found .AND. found2) THEN
    ! If both are in input file check if values are the same
    IF (.NOT. ALMOST_EQUAL(MinDepthFrc, MinDepth)) THEN
      MinDepthFrc = MAX(MinDepthFrc, MinDepth)
      MinDepth = MinDepthFrc
      CALL LWRITE('MinDepth and MinDepthFrc are not the same, using the maximum of two values.')
      ! Cleaning up log buffer
      CALL logbuff%DPOP(2)
      CALL logbuff%ADD('MinDepth', FORMAT_FLOAT(MinDepth))
      CALL logbuff%ADD('MinDepthFrc', FORMAT_FLOAT(MinDepthFrc))
    END IF

  ELSEIF (found) THEN
    ! If MinDepth is found, use value as default value for MinDepthFrc
    CALL READ_PARAMETER(MinDepthFrc, 'MinDepthFrc', MinDepth, 'm')
  ELSEIF (found2) THEN
    ! If MinDepthFrc is found, use value as default value for MinDepth
    CALL READ_PARAMETER(MinDepth, 'MinDepth', MinDepthFrc, 'm')
  ELSE
    ! If neither is found, use default value
    CALL READ_PARAMETER(MinDepth   , 'MinDepth'   , 0.1_SP, 'm')
    CALL READ_PARAMETER(MinDepthFrc, 'MinDepthFrc', 0.1_SP, 'm')
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! Lauren - Arrival Time - Wave height threshold (in m) to pick up arrival time
! ----------------------------------------------------------------
  CALL BWRITE('ARRIVAL TIME INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(OUT_Time, 'OUT_Time', .FALSE.)

  IF (.NOT. OUT_Time) THEN
    CALL LWRITE("Don't record wave arrival time.")
  ELSE
    CALL LWRITE("Record wave arrival time.")
    CALL READ_PARAMETER(ArrTimeMin, 'ArrTimeMin', 0.001_SP, 'm')
  END IF

  CALL logbuff%FLUSH2OUTPUT()

! end Laurens modification

! ----------------------------------------------------------------
  CALL BWRITE('WAVE-AVERAGED PROPERTIES INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(T_INTV_mean, 'T_INTV_mean', LARGE  , 's')
  CALL READ_PARAMETER(STEADY_TIME, 'STEADY_TIME', LARGE  , 's')
  CALL READ_PARAMETER(C_smg      , 'C_smg'      , 0.0_SP      )
  CALL READ_PARAMETER(nu_bkg     , 'nu_bkg'     , 0.0_SP      )

  CALL logbuff%FLUSH2OUTPUT()

# if defined (COUPLING)
! -----------------------------------------------------
  CALL BWRITE('COUPLING INFO')
! ----------------------------------------------------------------
  CALL READ_PARAMETER(COUPLING_FILE, 'COUPLING_FILE', is_file=.TRUE.)
  CALL logbuff%FLUSH2OUTPUT()
# endif

! ----------------------------------------------------------------
  CALL BWRITE('OUTPUT INFO')
! ----------------------------------------------------------------

  CALL READ_PARAMETER(RESULT_FOLDER, 'RESULT_FOLDER', './output/')

  MKFOLDER = "mkdir -p "//TRIM(RESULT_FOLDER)
# if defined (PARALLEL)
  IF (myid .eq. 0) THEN
# endif
# if defined (INTEL)
    RES = SYSTEM(TRIM(MKFOLDER))
# else
    CALL SYSTEM(TRIM(MKFOLDER))
# endif
#if defined(PARALLEL)
  END IF
#endif

! mayhl 17/06/12
! Note: Serial code only outputs in ASCII
# if defined (PARALLEL)
  CALL READ_PARAMETER(FIELD_IO_TYPE, 'FIELD_IO_TYPE', 'ASCII')
# else
  FIELD_IO_TYPE = "ASCII"
# endif
! mayhl]

  CALL READ_PARAMETER(OUTPUT_RES, 'OUTPUT_RES', 1)

  CALL READ_PARAMETER(OUT_DEPTH     , 'DEPTH_OUT' , .FALSE.)
  CALL READ_PARAMETER(OUT_U         , 'U'         , .FALSE.)
  CALL READ_PARAMETER(OUT_V         , 'V'         , .FALSE.)
  CALL READ_PARAMETER(OUT_ETA       , 'ETA'       , .FALSE.)
# if defined(UseEtaScreen)
  CALL READ_PARAMETER(OUT_EtaScreen , 'ETAscreen' , .FALSE.)
# endif
  CALL READ_PARAMETER(OUT_Hmax      , 'Hmax'      , .FALSE.)
  CALL READ_PARAMETER(OUT_Hmin      , 'Hmin'      , .FALSE.)
  CALL READ_PARAMETER(OUT_Umax      , 'Umax'      , .FALSE.)
  CALL READ_PARAMETER(OUT_MFmax     , 'MFmax'     , .FALSE.)
  CALL READ_PARAMETER(OUT_VORmax    , 'VORmax'    , .FALSE.)
  CALL READ_PARAMETER(OUT_MASK      , 'MASK'      , .FALSE.)
  CALL READ_PARAMETER(OUT_MASK9     , 'MASK9'     , .FALSE.)
  CALL READ_PARAMETER(OUT_Umean     , 'Umean'     , .FALSE.)
  CALL READ_PARAMETER(OUT_Vmean     , 'Vmean'     , .FALSE.)
  CALL READ_PARAMETER(OUT_ETAmean   , 'ETAmean'   , .FALSE.)
  CALL READ_PARAMETER(OUT_WaveHeight, 'WaveHeight', .FALSE.)
  CALL READ_PARAMETER(OUT_SXL       , 'SXL'       , .FALSE.)
  CALL READ_PARAMETER(OUT_SXR       , 'SXR'       , .FALSE.)
  CALL READ_PARAMETER(OUT_SYL       , 'SYL'       , .FALSE.)
  CALL READ_PARAMETER(OUT_SYR       , 'SYR'       , .FALSE.)
  CALL READ_PARAMETER(OUT_SourceX   , 'SourceX'   , .FALSE.)
  CALL READ_PARAMETER(OUT_SourceY   , 'SourceY'   , .FALSE.)
  CALL READ_PARAMETER(OUT_FrcX      , 'FrcX'      , .FALSE.)
  CALL READ_PARAMETER(OUT_FrcY      , 'FrcY'      , .FALSE.)
  CALL READ_PARAMETER(OUT_BrkdisX   , 'BrkdisX'   , .FALSE.)
  CALL READ_PARAMETER(OUT_BrkdisY   , 'BrkdisY'   , .FALSE.)
  CALL READ_PARAMETER(OUT_P         , 'P'         , .FALSE.)
  CALL READ_PARAMETER(OUT_Q         , 'Q'         , .FALSE.)
  CALL READ_PARAMETER(OUT_Fx        , 'Fx'        , .FALSE.)
  CALL READ_PARAMETER(OUT_Fy        , 'Fy'        , .FALSE.)
  CALL READ_PARAMETER(OUT_Gx        , 'Gx'        , .FALSE.)
  CALL READ_PARAMETER(OUT_Gy        , 'Gy'        , .FALSE.)
  CALL READ_PARAMETER(OUT_AGE       , 'AGE'       , .FALSE.)
  CALL READ_PARAMETER(OUT_ROLLER    , 'ROLLER'    , .FALSE.)
  CALL READ_PARAMETER(OUT_UNDERTOW  , 'UNDERTOW'  , .FALSE.)
  CALL READ_PARAMETER(OUT_NU        , 'OUT_NU'    , .FALSE.)
  CALL READ_PARAMETER(OUT_TMP       , 'TMP'       , .FALSE.)
  CALL READ_PARAMETER(OUT_Radiation , 'Radiation' , .FALSE.)

  CALL logbuff%FLUSH2OUTPUT()
!ykchoi
!  CALL READ_FLOAT(EtaBlowVal,FILE_NAME,'EtaBlowVal',ierr)
!  fyshi set blowup value is 100xmax_depth in init.F

  CALL CLEAR_INPUT_LOGGER

ENDSUBROUTINE READ_INPUT

SUBROUTINE READ_ABS_OR_LEFT_BC_COMPFILE(INPUT_PHASE)

  USE GLOBAL
  USE LOG_IO
#if defined(INTEL)
  USE IFPORT
#endif
  IMPLICIT NONE

  LOGICAL, INTENT(INOUT):: INPUT_PHASE

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

881 INPUT_PHASE = .TRUE.
991 CONTINUE

  DO J = 1, NumFreq
    DO I = 1, NumDir
      IF (INPUT_PHASE) THEN
        Phase_LEFT(J, I) = Phase_LEFT(J, I)*3.1415926/180.0_SP
      ELSE
# if defined (INTEL)
        Phase_LEFT(J, I) = rand()*2.0_SP*3.1415926
# elif defined (CRAY)
        Phase_LEFT(J, I) = rand()*2.0_SP*3.1415926
# else
        Phase_LEFT(J, I) = rand(0)*2.0_SP*3.1415926
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
      CALL SWRITE('Wave frequency is zero in WaveCompFile')
    ELSE
      Per_Ser(J) = 1.0_SP/Per_Ser(J)
    END IF
  END DO
  DO I = 1, NumDir
    Theta_Ser(I) = Theta_Ser(I)*DEG2RAD
  END DO

ENDSUBROUTINE

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
  INTEGER, DIMENSION(:), ALLOCATABLE :: BufferCount
  REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
  REAL(SP) :: dum1, dum2
  REAL(SP) :: eta_sta, u_sta, v_sta
  CHARACTER(LEN=80)::FILE_NAME = ' '
  CHARACTER(LEN=80)::TMP_NAME = ' '
  CHARACTER(LEN=80)::FDIR = ' '
  LOGICAL :: FirstCallStation = .TRUE.
  SAVE FirstCallStation, BufferCount, ZUV_Buffer

! initialize stations
  FDIR = TRIM(RESULT_FOLDER)
  if (FirstCallStation) then
    FirstCallStation = .FALSE.
    ALLOCATE (ista(NumberStations), &
              jsta(NumberStations), &
              nsta(NumberStations))
    ALLOCATE (BufferCount(NumberStations), &
              ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
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
      ELSE  ! to avoid topography on nested water surface
        eta_sta = eta(ista(i), jsta(i))
        u_sta = u(ista(i), jsta(i))
        v_sta = v(ista(i), jsta(i))
      END IF

      IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
        BufferCount(i) = BufferCount(i) + 1
        ZUV_BUFFER(BufferCount(i), i, 1) = time
        ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
        ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
        ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
      ELSE
        DO j = 1, StationOutputBuffer
          write (iunit, '(4E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
        END DO
        BufferCount(i) = 0
        ZUV_BUFFER(:, i, 1) = TOTAL_TIME
        ZUV_BUFFER(:, i, 2:4) = ZERO

# if defined (PARALLEL)
        if (myid .eq. 0) WRITE (*, *) 'WRITE OUT STATAION ...'
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

ENDSUBROUTINE STATIONS

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
  INTEGER, DIMENSION(:), ALLOCATABLE :: BufferCount
  REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
  REAL(SP) :: dum1, dum2
  REAL(SP) :: eta_sta, u_sta, v_sta
  CHARACTER(LEN=80)::FILE_NAME = ' '
  CHARACTER(LEN=80)::TMP_NAME = ' '
  CHARACTER(LEN=80)::FDIR = ' '
  LOGICAL :: FirstCallStation = .TRUE.
  SAVE FirstCallStation, BufferCount, ZUV_Buffer

! initialize stations
  FDIR = TRIM(RESULT_FOLDER)
  if (FirstCallStation) then
    FirstCallStation = .FALSE.
    ALLOCATE (ista(NumberStations), &
              jsta(NumberStations), &
              nsta(NumberStations))
    ALLOCATE (BufferCount(NumberStations), &
              ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
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
      ELSE  ! to avoid topography on nested water surface
        eta_sta = eta(ista(i), jsta(i))
        u_sta = u(ista(i), jsta(i))
        v_sta = v(ista(i), jsta(i))
      END IF

      IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
        BufferCount(i) = BufferCount(i) + 1
        ZUV_BUFFER(BufferCount(i), i, 1) = time
        ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
        ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
        ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
      ELSE
        DO j = 1, StationOutputBuffer
          write (iunit, '(4E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
        END DO
        BufferCount(i) = 0
        ZUV_BUFFER(:, i, 1) = TOTAL_TIME
        ZUV_BUFFER(:, i, 2:4) = ZERO

# if defined (PARALLEL)
        if (myid .eq. 0) WRITE (*, *) 'WRITE OUT STATAION ...'
# else
        WRITE (*, *) 'WRITE OUT STATIONS ...'
# endif

      END IF ! end buffer

!          write (iunit,'(20E16.5E4)') time, eta_sta,&
!                          u_sta,v_sta
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

! initialize stations
  FDIR = TRIM(RESULT_FOLDER)
  if (FirstCallStationSP) then
    FirstCallStationSP = .FALSE.
    ALLOCATE (ista(NumberStations), &
              jsta(NumberStations), &
              nsta(NumberStations))
    ALLOCATE (BufferCount(NumberStations), &
              ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
    BufferCount = 0

!       ALLOCATE(envelope(Mloc,Nloc))
!       envelope = 0.d0
    open (sunit, FILE=TRIM(STATIONS_FILE))
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
        iunit = sunit + i
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
        iunit = sunit + i
        open (iunit, FILE=TMP_NAME)
      else
        nsta(i) = 0
      end if
# endif
    end do
  end if

! write to stations

  sunit = fhandle%GET('stations')

  do i = 1, NumberStations
    if (nsta(i) .eq. 1) then
      iunit = 100 + i

! for coupling, make sure dry point
      IF (mask(ista(i), jsta(i)) < 1) THEN
        eta_sta = ZERO
        u_sta = ZERO
        v_sta = ZERO
      ELSE  ! to avoid topography on nested water surface
        eta_sta = eta(ista(i), jsta(i))
        u_sta = u(ista(i), jsta(i))
        v_sta = v(ista(i), jsta(i))
      END IF

      IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN
        BufferCount(i) = BufferCount(i) + 1
        ZUV_BUFFER(BufferCount(i), i, 1) = time
        ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
        ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
        ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
      ELSE
        DO j = 1, StationOutputBuffer
          write (iunit, '(4E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
        END DO
        BufferCount(i) = 0
        ZUV_BUFFER(:, i, 1) = TOTAL_TIME
        ZUV_BUFFER(:, i, 2:4) = ZERO

        CALL LWRITE('WRITE OUT STATIONS ...')

      END IF ! end buffer

!          write (iunit,'(20E16.5E4)') time, eta_sta,&
!                          u_sta,v_sta

!          write (iunit,*) time, eta(ista(i),jsta(i)),u(ista(i),jsta(i)),&
!                                v(ista(i),jsta(i))

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
!    STATIONS_V2 is a subroutine to write station data. Unified CARTESIAN, SPHERICAL
!       and SPHERICAL_IJ versions of STATIONS subroutine which only differed in 
!       ista and jsta
!
!    HISTORY:
!   08/20/2023  Michael-Angelo Lam
!   09/16/2011  Fengyan Shi
!   04/05/2011  Jeff Harris
!
!-------------------------------------------------------------------------------------
SUBROUTINE STATIONS_V2
  USE GLOBAL
  USE LOG_IO
  USE MOD_FUNITS, ONLY: fhandle
  IMPLICIT NONE

  INTEGER                                   :: iunit, sunit
  INTEGER , DIMENSION(:)      , ALLOCATABLE :: BufferCount
  REAL(SP), DIMENSION(:, :, :), ALLOCATABLE :: ZUV_Buffer
  REAL(SP)                                  :: dum1, dum2
  REAL(SP)                                  :: eta_sta, u_sta, v_sta
  CHARACTER(LEN=80)                         :: FILE_NAME = ' '
  CHARACTER(LEN=80)                         :: TMP_NAME = ' '
  CHARACTER(LEN=80)                         :: FDIR = ' '
  LOGICAL                                   :: FirstCallStation2 = .TRUE.
  LOGICAL                                   :: BufferWrote
  SAVE FirstCallStation2, BufferCount, ZUV_Buffer, sunit


  CALL DWRITE("HERE")
! initialize stations
  FDIR = TRIM(RESULT_FOLDER)
  IF (FirstCallStation2) THEN
    FirstCallStation2 = .FALSE.
    ALLOCATE (ista(NumberStations), &
              jsta(NumberStations), &
              nsta(NumberStations))
    ALLOCATE (BufferCount(NumberStations), &
              ZUV_Buffer(StationOutputBuffer, NumberStations, 4))
    BufferCount = 0

    ! Reserving file units for stations + stations file
    sunit = fhandle%ADD_RANGE('stations', NumberStations + 1)

    OPEN (sunit, FILE=TRIM(STATIONS_FILE))
    DO i = 1, NumberStations
      READ (sunit, *) dum1, dum2

      ! mayhl: Refactored STATION subroutines for CARTESIAN, SPHERICAL, and 
      !        SPHERICAL_IJ_STATION. Differences between subroutines only in
      !        parsing STATIONS_FILE values to ista and jsta values. 
# if defined (CARTESIAN)
      ista(i) = Nghost + dum1
      jsta(i) = Nghost + dum2
# else
# if defined (SPHERICAL_IJ_STATION)
      ista(i) = Nghost + dum1
      jsta(i) = Nghost + dum2
# else
      ista(i) = Nghost + 1 + nint((dum2 - Lon_West)/Dphi)
      jsta(i) = Nghost + 1 + nint((dum1 - Lat_South)/Dtheta) 
# endif
# endif 
      ! mayhl: Refactored PARALLEL flag logic as only difference was in index offset 
# if defined (PARALLEL)
      ista(i) = ista(i) - (iista - 1)
      jsta(i) = jsta(i) - (jjsta - 1)
# endif 

      IF ((ista(i).GE.Ibeg).AND.(ista(i).LE.Iend).AND. &
          (jsta(i).GE.Jbeg).AND.(jsta(i).LE.Jend)) THEN
        nsta(i) = 1
        WRITE (file_name(1:4), '(I4.4)') i
        TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
        iunit = sunit + i
        OPEN (iunit, FILE=TMP_NAME)
      ELSE
        nsta(i) = 0
      ENDIF
    ENDDO
  ENDIF ! end FirstCallStation 

  BufferWrote = .FALSE.
  DO i = 1, NumberStations

    ! Go to next iteration skipping remaining loop code
    IF (nsta(i).EQ.0) CYCLE 

    iunit = sunit + i

    ! for coupling, make sure dry point
    IF (mask(ista(i), jsta(i)).LT.1) THEN
      eta_sta = ZERO
      u_sta = ZERO
      v_sta = ZERO
    ELSE  ! to avoid topography on nested water surface
      eta_sta = eta(ista(i), jsta(i))
      u_sta = u(ista(i), jsta(i))
      v_sta = v(ista(i), jsta(i))
    END IF

    IF (BufferCount(i) < StationOutputBuffer .AND. TIME < TOTAL_TIME) THEN

      BufferCount(i) = BufferCount(i) + 1
      ZUV_BUFFER(BufferCount(i), i, 1) = time
      ZUV_BUFFER(BufferCount(i), i, 2) = eta_sta
      ZUV_BUFFER(BufferCount(i), i, 3) = u_sta
      ZUV_BUFFER(BufferCount(i), i, 4) = v_sta
    ELSE

      DO j = 1, BufferCount(i)
        WRITE (iunit, '(4E16.5E4)') (ZUV_BUFFER(j, i, k), k=1, 4)
      END DO

      BufferCount(i) = 0
      ZUV_BUFFER(:, i, 1) = TOTAL_TIME
      ZUV_BUFFER(:, i, 2:4) = ZERO

      !IF (.NOT.BufferWrote) BufferWrote = .TRUE.

    END IF ! end buffer    
  ENDDO

! mayhl: Is communication non-negligible?
!#if defined (PARALLEL)
!  CALL MPI_AllReduce(BufferWrote, BufferWrote, 1, MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, ier)
!#endif
!  IF (BufferWrote) CALL LWRITE('Write stations buffer...')

  IF (TIME.GE.TOTAL_TIME) THEN
    DO i = 1, NumberStations
      IF (nsta(i).EQ.1) THEN
        iunit = sunit + i
        CLOSE(iunit)
      ENDIF
    ENDDO
  ENDIF

END SUBROUTINE

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
  USE LOG_IO
  USE FIELD_IO
  USE MOD_FUNITS, ONLY: fhandle
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
  CHARACTER(LEN=5) :: FILE_NUM = ' '
  INTEGER, SAVE    :: tunit = -1

  ICOUNT = ICOUNT + 1

  WRITE(msg_buff, 102) 'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME, '/', Total_Time
102  FORMAT(A20, I6, A14, F12.3, A2, F12.3)
  CALL LWRITE(msg_buff)

  ! mayhl: replaced explcit digit extraction 
  WRITE(FILE_NUM, '(I0.5)') icount

  IF (ICOUNT == 1) THEN
    IF (OUT_DEPTH .OR. BREAKWATER) THEN
      CALL PutOutput('dep.out', DEPTH)
      CALL PutOutput('cd_breakwater.out', CD_breakwater)
    END IF
  END IF

  IF (tunit.LT.0 ) THEN
    tunit = fhandle%ADD('time_dt')
    OPEN(tunit, FILE='time_dt.out', STATUS='UNKNOWN')
  ENDIF
![ykchoi
  WRITE(msg_buff, *) time, dt
  CALL UWRITE(tunit, msg_buff)
!ykchoi]

  IF (OUT_ETA) CALL PutOutputNum('eta', FILE_NUM, Eta)

# if defined (UseEtaScreen)
  IF (OUT_ETAscreen) CALL PutOutputNum('etasrn', FILE_NUM, EtaScreen)
# endif

  IF (OUT_Hmax  ) CALL PutOutputNum('hmax'  , FILE_NUM, HeightMax      )
  IF (OUT_Hmin  ) CALL PutOutputNum('hmin'  , FILE_NUM, HeightMin      )
  IF (OUT_Umax  ) CALL PutOutputNum('umax'  , FILE_NUM, VelocityMax    )
  IF (OUT_MFmax ) CALL PutOutputNum('MFmax' , FILE_NUM, MomentumFluxMax)
  IF (OUT_VORmax) CALL PutOutputNum('VORmax', FILE_NUM, VorticityMax   )
  IF (OUT_U     ) CALL PutOutputNum('u'     , FILE_NUM, U              )
  IF (OUT_V     ) CALL PutOutputNum('v'     , FILE_NUM, V              )

  IF (OUT_MASK) THEN
    Int2Flo = MASK
    CALL PutOutputNum('mask', FILE_NUM, Int2Flo)
  END IF

  IF (OUT_MASK9) THEN
    Int2Flo = MASK9
    CALL PutOutputNum('mask9', FILE_NUM, Int2Flo)
  END IF

  IF (OUT_P) CALL PutOutputNum('p', FILE_NUM, P)
  IF (OUT_Q) CALL PutOutputNum('q', FILE_NUM, Q)

  IF (OUT_AGE.AND.SHOW_BREAKING) THEN
    CALL PutOutputNum('age', FILE_NUM, AGE_BREAKING)
  END IF

  IF (OUT_ROLLER) CALL PutOutputNum('roller', FILE_NUM, ROLLER_FLUX)

  IF (OUT_UNDERTOW) THEN
    CALL PutOutputNum('U_undertow', FILE_NUM, UNDERTOW_U)
    CALL PutOutputNum('V_undertow', FILE_NUM, UNDERTOW_V)
  END IF

  IF (VISCOSITY_BREAKING.AND.OUT_NU) THEN
   CALL PutOutputNum('nubrk', FILE_NUM, nu_break)
!  TMP_NAME = TRIM(FDIR)//'etat_'//TRIM(FILE_NUM)
!  call PutFile(TMP_NAME,etat)
  END IF

  IF (OUT_FrcX   ) CALL PutOutputNum('FrcInsX', FILE_NUM, FrcInsX)
  IF (OUT_FrcY   ) CALL PutOutputNum('FrcInsY', FILE_NUM, FrcInsY)
  IF (OUT_BrkdisX) CALL PutOutputNum('BrkSrcX', FILE_NUM, BreakSourceX)
  IF (OUT_BrkdisY) CALL PutOutputNum('BrkSrcY', FILE_NUM, BreakSourceY)
  IF (OUT_Time   ) CALL PutOutputNum('time'   , FILE_NUM, ARRTIME)

# if defined (METEO)
  IF (OUT_METEO) THEN

    IF (WindHollandModel) THEN
      CALL PutOutputNum('Pstorm', FILE_NUM, StormPressureTotal)
      CALL PutOutputNum('Ustorm', FILE_NUM, WindU2D)
      CALL PutOutputNum('Vstorm', FILE_NUM, WindV2D)
    END IF

    IF (MeteoGausian) CALL PutOutputNum('Pstorm', FILE_NUM, StormPressureTotal)
    IF (SlideModel)   CALL PutOutputNum('Pstorm', FILE_NUM, StormPressureTotal)

  END IF
# endif

# if defined (VESSEL)
  IF (OUT_VESSEL) THEN
# if defined (VESSEL_PANEL_SOURCE)
    CALL PutOutputNum('Fves', FILE_NUM, VesselFluxGradient)
# else
    CALL PutOutputNum('Pves', FILE_NUM, VesselPressureTotal)
# endif
  ENDIF
# endif

! sediment
# if defined (SEDIMENT)

  ! change to non-dimensional 
  !CALL PutOutputNum('C'     , FILE_NUM, CH*Sdensity*RHO_WATER)
  CALL PutOutputNum('C'     , FILE_NUM, CH                                  )
  CALL PutOutputNum('Pick'  , FILE_NUM, Pickup                              )
  CALL PutOutputNum('Depo'  , FILE_NUM, D                                   )
  CALL PutOutputNum('P_avg' , FILE_NUM, P_ave                               )
  CALL PutOutputNum('D_avg' , FILE_NUM, D_ave                               )
  CALL PutOutputNum('DchgS' , FILE_NUM, TotalSuspendLoad/(1.0_SP-n_porosity))
  CALL PutOutputNum('DchgB' , FILE_NUM, TotalBedLoad/(1.0_SP-n_porosity)    )
  CALL PutOutputNum('BedFx' , FILE_NUM, BedFluxX                            )
  CALL PutOutputNum('BedFy' , FILE_NUM, BedFluxY                            )
  CALL PutOutputNum('dep'   , FILE_NUM, Depth                               )
  CALL PutOutputNum('BedStr', FILE_NUM, Tau_xy                              )
  CALL PutOutputNum('Aval'  , FILE_NUM, Zb_avalanching                      )
  CALL PutOutputNum('AvalAc', FILE_NUM, Aval_accum                          )

#if defined (SEDIMENT_DEBUG)
  CALL PutOutputNum('Cb'   , FILE_NUM, Cb_debug    )
  CALL PutOutputNum('Ca'   , FILE_NUM, Ca_debug    )
  CALL PutOutputNum('Redu' , FILE_NUM, Reduct_debug)
  CALL PutOutputNum('TauEx', FILE_NUM, Tau_extra   )
  CALL PutOutputNum('Hpo'  , FILE_NUM, Hpo         )
# endif 
# endif
! end sediment

! foam
# if defined (FOAM)
  CALL PutOutputNum('FoamEta', FILE_NUM, EtaFoam)
# endif
! end foam

  IF (OUT_TMP) CALL PutOutputNum('tmp', FILE_NUM, tmp4preview)

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
  USE LOG_IO
  USE FIELD_IO
  IMPLICIT NONE
  REAL(SP), DIMENSION(Mloc, Nloc) :: tmpout
  CHARACTER(LEN=5)::FILE_NUM = ' '

  ICOUNT_MEAN = ICOUNT_MEAN + 1

  WRITE (msg_buff, '(A20, I6)') 'PRINTING MEAN FILE', icount_mean
  CALL LWRITE(msg_buff)  

  ! mayhl: replaced explicit digit extraction
  WRITE(FILE_NUM, '(I0.5)') ICOUNT_MEAN

  IF (OUT_Umean) THEN
    CALL PutOutputNum('umean', FILE_NUM, Umean)
    tmpout = P_mean/Max(Depth + ETAmean, MinDepthFrc)
    CALL PutOutputNum('ulagm', FILE_NUM, tmpout)
  END IF

  IF (OUT_Vmean) THEN
    CALL PutOutputNum('vmean', FILE_NUM, Vmean)
    tmpout = Q_mean/Max(Depth + ETAmean, MinDepthFrc)
    CALL PutOutputNum('vlagm', FILE_NUM, tmpout)
  END IF

  IF (OUT_ETAmean) CALL PutOutputNum('etamean', FILE_NUM, ETAmean)

  IF (OUT_WaveHeight) THEN
    CALL PutOutputNum('Hrms', FILE_NUM, WaveHeightRMS)
    CALL PutOutputNum('Havg', FILE_NUM, WaveHeightAve)
    CALL PutOutputNum('Hsig', FILE_NUM, SigWaveHeight)
  END IF

    ! output FRCYsum,FRCYmean
    !    DxSxx,DySxy,DySyy,DxSxy,PgrdX,PgrdY,DxUUH,DyUVH,DyVVH,DxUVH

  IF (OUT_Radiation) THEN

   tmpout = UUmean - WWmean + 0.5*9.8*ETA2mean
   CALL PutOutputNum('Sxx', FILE_NUM, tmpout)
   CALL PutOutputNum('Sxy', FILE_NUM, UVmean)
   tmpout = VVmean - WWmean + 0.5*9.8*ETA2mean
   CALL PutOutputNum('Syy', FILE_NUM, tmpout)

   CALL PutOutputNum('DxSxx'   , FILE_NUM, DxSxx)
   CALL PutOutputNum('DySxy'   , FILE_NUM, DySxy)
   CALL PutOutputNum('DySyy'   , FILE_NUM, DySyy)
   CALL PutOutputNum('DxSxy'   , FILE_NUM, DxSxy)
   CALL PutOutputNum('PgrdX'   , FILE_NUM, PgrdX)
   CALL PutOutputNum('PgrdY'   , FILE_NUM, PgrdY)
   CALL PutOutputNum('DxUUH'   , FILE_NUM, DxUUH)
   CALL PutOutputNum('DyUVH'   , FILE_NUM, DyUVH)
   CALL PutOutputNum('DyVVH'   , FILE_NUM, DyVVH)
   CALL PutOutputNum('DxUVH'   , FILE_NUM, DxUVH)
   CALL PutOutputNum('FRCX'    , FILE_NUM, FRCXmean)
   CALL PutOutputNum('FRCY'    , FILE_NUM, FRCYmean)
   CALL PutOutputNum('BrkDissX', FILE_NUM, BreakDissX)
   CALL PutOutputNum('BrkDissY', FILE_NUM, BreakDissY)

  END IF

END SUBROUTINE PREVIEW_MEAN

! # if defined (PARALLEL)
! !-------------------------------------------------------------------------------------
! !
! !    GetFile is subroutine for reading field data
! !
! !    HISTORY:
! !    05/01/2010  Fengyan Shi
! !    05/08/2017  Young-Kwang Choi
! !-------------------------------------------------------------------------------------
!
!       SUBROUTINE GetFile(FILE, PHI)
!         USE GLOBAL
!         IMPLICIT NONE
!
!         REAL(SP), DIMENSION(MGlob + 2*Nghost, NGlob + 2*Nghost) :: PHIGLOB
!         CHARACTER(LEN=80) FILE
!         REAL(SP), DIMENSION(Mloc, Nloc), INTENT(OUT) :: PHI
!
! ![-------ykchoi (08/May/2017)
!         INTEGER :: irank, lenx, leny, lenxy, ireq
!         INTEGER :: Nista, Niend, Njsta, Njend
!         INTEGER :: istanum, iendnum, jstanum, jendnum
!         INTEGER, ALLOCATABLE :: Nistas(:), Niends(:), Njstas(:), Njends(:)
!         INTEGER :: istatus(mpi_status_size)
!         REAL(SP), ALLOCATABLE :: xx(:, :)
! ! -------ykchoi (08/May/2017) ]
!
! ! TEMP
!
!         if (myid .eq. 0) then
!           OPEN (1, FILE=TRIM(FILE))
!           DO J = Nghost + 1, NGlob + NGhost
!             READ (1, *) (PHIGLOB(I, J), I=Nghost + 1, MGlob + Nghost)
!           END DO
!           CLOSE (1)
! ! ghost cells
!           DO I = Nghost + 1, MGlob + Nghost
!             DO J = 1, Nghost
!               PHIGLOB(I, J) = PHIGLOB(I, Nghost + 1)
!             END DO
!             DO J = NGlob + Nghost + 1, NGlob + 2*Nghost
!               PHIGLOB(I, J) = PHIGLOB(I, NGlob + Nghost)
!             END DO
!           END DO
!           DO J = 1, NGlob + 2*Nghost
!             DO I = 1, Nghost
!               PHIGLOB(I, J) = PHIGLOB(Nghost + 1, J)
!             END DO
!             DO I = MGlob + Nghost + 1, MGlob + 2*Nghost
!               PHIGLOB(I, J) = PHIGLOB(MGlob + Nghost, J)
!             END DO
!           END DO
!         end if
!
! ![-------ykchoi (08/May/2017)
!         Nista = iista + Nghost; 
!         Niend = iiend + Nghost; 
!         Njsta = jjsta + Nghost; 
!         Njend = jjend + Nghost; 
!         allocate (Nistas(nprocs), Niends(nprocs), Njstas(nprocs), Njends(nprocs))
!
!         call MPI_Gather(Nista, 1, MPI_INTEGER, Nistas, 1, MPI_INTEGER, &
!                         0, MPI_COMM_WORLD, ier)
!         call MPI_Gather(Niend, 1, MPI_INTEGER, Niends, 1, MPI_INTEGER, &
!                         0, MPI_COMM_WORLD, ier)
!         call MPI_Gather(Njsta, 1, MPI_INTEGER, Njstas, 1, MPI_INTEGER, &
!                         0, MPI_COMM_WORLD, ier)
!         call MPI_Gather(Njend, 1, MPI_INTEGER, Njends, 1, MPI_INTEGER, &
!                         0, MPI_COMM_WORLD, ier)
!
!         if (myid == 0) then
!           PHI = PHIGLOB(1:Mloc, 1:Nloc)
!         end if
!
!         do irank = 1, px*py - 1
!           if (myid == 0) then
!             istanum = Nistas(irank + 1) - Nghost
!             iendnum = Niends(irank + 1) + Nghost
!             jstanum = Njstas(irank + 1) - Nghost
!             jendnum = Njends(irank + 1) + Nghost
!
!             lenx = iendnum - istanum + 1
!             leny = jendnum - jstanum + 1
!             lenxy = lenx*leny
!             allocate (xx(lenx, leny))
!
!             xx = PHIGLOB(istanum:iendnum, jstanum:jendnum)
!             call mpi_isend(xx, lenxy, mpi_sp, irank, 1, mpi_comm_world, ireq, ier)
!             call mpi_wait(ireq, istatus, ier)
!             deallocate (xx)
!
!           elseif (myid == irank) then
!
!             lenx = Niend - Nista + 1 + 2*Nghost
!             leny = Njend - Njsta + 1 + 2*Nghost
!             lenxy = lenx*leny
!
!             call mpi_irecv(PHI, lenxy, mpi_sp, 0, 1, mpi_comm_world, ireq, ier)
!             call mpi_wait(ireq, istatus, ier)
!
!           end if
!         end do
!
!         deallocate (Nistas, Niends, Njstas, Njends)
!
! ! -------ykchoi (08/May/2017) ]
!
!       END SUBROUTINE Getfile
!
! # endif
!
! SUBROUTINE PutOutput(name, phi)
!
!   USE GLOBAL
!   IMPLICIT NONE
!   
!   CHARACTER(LEN=*)               , INTENT(IN) :: name
!   REAL(SP), DIMENSION(Mloc, Nloc), INTENT(IN) :: phi
!   CHARACTER(LEN=100)             :: fpath          
!
!   fpath = TRIM(RESULT_FOLDER)//TRIM(ADJUSTL(name))
!   CALL PutFile(fpath, phi)
!
! END SUBROUTINE
!
! SUBROUTINE PutOutputNum(name, num, phi)
!
!   USE GLOBAL
!   IMPLICIT NONE
!   
!   CHARACTER(LEN=*)               , INTENT(IN) :: name, num
!   REAL(SP), DIMENSION(Mloc ,Nloc), INTENT(IN) :: phi
!   CHARACTER(LEN=100)             :: fpath          
!
!   fpath = TRIM(RESULT_FOLDER)//TRIM(ADJUSTL(name))//"_"//TRIM(num)
!   CALL PutFile(fpath, phi)
!
! END SUBROUTINE 
!
!
! # if defined (PARALLEL)
! !-------------------------------------------------------------------------------------
! !
! !    PutFile is subroutine for print-out of field data
! !
! !    HISTORY:
! !      05/01/2010  Fengyan Shi
! !      05/06/2017  Young-Kwang Choi
! !-------------------------------------------------------------------------------------
!
! SUBROUTINE PutFile(FILE_NAME, PHI)
!   USE GLOBAL
!   USE PARALLEL_FIELD_IO
!   IMPLICIT NONE
!
!   CHARACTER(LEN=80) FILE_NAME
!   REAL(SP), DIMENSION(Mloc, Nloc), INTENT(IN) :: PHI
!
!   SELECT CASE (TRIM(FIELD_IO_TYPE))
!   CASE ('ASCII', 'ascii')
!     CALL PutFileASCII(FILE_NAME, PHI)
!   CASE ('BINARY', 'binary')
!     Call PutFileBinary(FILE_NAME, PHI)
!   CASE DEFAULT
!     !Defaults to ASCII case for non-valid input
!     CALL PutFileASCII(FILE_NAME, PHI)
!   END SELECT
!
! END SUBROUTINE Putfile
!
! # else
! !-------------------------------------------------------------------------------------
! !
! !    PutFile is subroutine for print-out of field data
! !
! !    HISTORY:
! !      05/01/2010  Fengyan Shi
! !
! !-------------------------------------------------------------------------------------
!       SUBROUTINE PutFile(FILE, PHI)
!         USE PARAM
!         USE GLOBAL
!         IMPLICIT NONE
!         REAL(SP), DIMENSION(Mloc, Nloc), INTENT(IN) :: PHI
!         CHARACTER(LEN=80) FILE
!         LOGICAL :: FirstCallPutFile = .TRUE.
!         SAVE FirstCallPutFile
!
! ! first time call
!         IF (FirstCallPutFile) THEN
!           FirstCallPutFile = .FALSE.
! ! format length
!           write (FORMAT_LEN(1:1), '(A1)') '('
!           write (FORMAT_LEN(2:8), '(I7)') Mglob
!           write (FORMAT_LEN(9:15), '(A7)') 'E16.6E4'
!           write (FORMAT_LEN(16:16), '(A1)') ')'
!         END IF
!
!         OPEN (1, FILE=TRIM(FILE))
! # if defined(DEBUG)
!         DO J = 1, Nloc
!           WRITE (1, 100) (real(PHI(I, J)), I=1, Mloc)
!         END DO
! # else
!         DO J = Nghost + 1, Nloc - Nghost, OUTPUT_RES
!           WRITE (1, FORMAT_LEN) (real(PHI(I, J)), I=Nghost + 1, Mloc - Nghost, OUTPUT_RES)
!         END DO
! # endif
! 100     FORMAT(5000E16.6)
! !100   FORMAT(FORMAT_LEN)
!         CLOSE (1)
!       END SUBROUTINE PutFile
!
! # endif

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
  USE LOG_IO
  USE MOD_FUNITS, ONLY : fhandle
  IMPLICIT NONE
  CHARACTER (LEN=80) :: FDIR = ' '
  INTEGER, SAVE      :: runit = -1
  LOGICAL, SAVE     :: FirstCallVessel = .TRUE.
  SAVE FirstCallVessel, runit

  ! mayhl: Outside myid.EQ.0 to ensure all threads 
  !        have the same units 
  IF (runit.LT.0) runit = fhandle % ADD('Resis')

# if defined (PARALLEL)
  IF (myid.EQ.0) THEN
# endif

! initialize stations
    IF (FirstCallVessel) THEN
      FDIR = TRIM(RESULT_FOLDER)
      FirstCallVessel = .FALSE.
      OPEN (runit, FILE=TRIM(FDIR)//'Resis.txt')
    ENDIF

! write to stations

    WRITE(runit, '(60E16.5)') time, (ResistanceX(i), i=1, NumVessel), &
      (ResPosX(i), i=1, NumVessel), &
      (ResNegX(i), i=1, NumVessel), &
      (ResistanceY(i), i=1, NumVessel), &
      (ResPosY(i), i=1, NumVessel), &
      (ResNegY(i), i=1, NumVessel)

  ! close station files
    IF (TIME .ge. TOTAL_TIME) CLOSE (runit)

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
  IMPLICIT NONE
  CHARACTER(LEN=80)::FDIR = ' '
  LOGICAL, SAVE :: FirstCallSediment = .TRUE.

# if defined (PARALLEL)
  IF (myid.EQ.0) THEN
# endif

    IF (FirstCallSediment) THEN
      FDIR = TRIM(RESULT_FOLDER)
      FirstCallSediment = .FALSE.
    ENDIF

# if defined (PARALLEL)
  ENDIF
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
  IMPLICIT NONE
  CHARACTER(LEN=80)::FDIR = ' '
  LOGICAL :: FirstCallFoam = .TRUE.
  SAVE FirstCallFoam

# if defined (PARALLEL)
  IF (myid.EQ.0) THEN
# endif

    IF (FirstCallFoam) THEN
      FDIR = TRIM(RESULT_FOLDER)
      FirstCallFoam = .FALSE.
    ENDIF

! time series here

# if defined (PARALLEL)
  END IF
# endif

END SUBROUTINE OUTPUT_FOAM
# endif
! end foam

