!------------------------------------------------------------------------------------
!
!      FILE main.F
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
Program FUNWAVE_TVD
!-------------------------------------------------------------------------------------
!    VERSION 3.0
!
!    MAIN - READ_INPUT
!         - INDEX
!         - ALLOCATE_VARIABLES
!         - INITIALIZATION 
!         DO LOOP
!             - VARIABLE_UPDATING
!             - EXCHANGE
!             - ESTIMATE_DT
!             - RUNGE-KUTTA
!             - DISPERSION
!             - FLUXES          
!             - SourceTerms      
!             - ESTIMATE_HUV
!             - EXCHANGE 
!             - FLUXES                 
!             - SourceTerms again
!
!             - STATISTICS
!             - PREVIEW
!          ENDDO LOOP
!-------------------------------------------------------------------------------------
! ** OPEN FILES **
!  (1): read input, (2): output, (3): log, (4): !write/read hotstart
!-------------------------------------------------------------------------------------
! ** HOT START DATA **
!   NOTE: read input.txt first, if HOT_START, then read  
        ! -- dimension
! Mloc,Nloc,Mloc1,Nloc1
! Nghost
! Ibeg,Iend,Jbeg,Jend,Iend1,Jend1
!   NOTE: need to confirm if the saved data is consistent with input.txt
        ! -- time
! TIME
! TOTAL_TIME
! PLOT_INTV
! PLOT_COUNT
! SCREEN_INTV
! SCREEN_COUNT
! HOTSTART_INTV
! ICOUNT
        ! spacing
! DX,DY
        ! -- physics
! DISPERSION
! Gamma1
! a1,a2,b1,b2
! SWE_ETA_DEP
        ! -- numerics
! Time_Scheme
! HIGH_ORDER
! CONSTR
! CFL
! FroudeCap
! DISP_TIME_LEFT
        ! -- wet-dry
! MinDepth,MinDepthfrc

        ! -- depth
! DEPTH
! DEPTHx
! DEPTHy
        ! variables
! U
! V
! if (.NOT.DISP_TIME_LEFT)THEN
! U0
! V0
! endif
! Ubar
! Vbar
! ETA 
! H
! MASK
! MASK9
! MAST_STRUC
!
       ! -- wavemaker
! if (WAVEMAKER is WK_IRR)
! turns out the data for Cm Sm too large, calculate it when hotstart
!
! if (WAVEMAKER is WK_REG)
! D_gen
! Beta_gen
! rlamda
! 
!
!-------------------------------------------------------------------------------------
  USE GLOBAL
  USE LOG_IO
# if defined (VESSEL)
  USE VESSEL_MODULE
# endif
# if defined (SEDIMENT)
  USE SEDIMENT_MODULE
# endif
# if defined (FOAM)
  USE MOD_FOAM
# endif
# if defined (METEO)
  USE METEO_MODULE
# endif
# if defined (TRACKING)
   USE TRACER
# endif
# if defined (PRECIPITATION)
     USE PRECIPITATION_MODULE
# endif

     USE TIDE_MODULE
     USE TIME_SPECTRA_MODULE

     IMPLICIT NONE

!     INTEGER::ISTAGE ! moved to mod_global 09/12/2017
!     REAL(SP) :: tbegin,tend  ! moved to mod_global 07/29/2016

# if defined (PARALLEL)
  CALL MPI_INIT ( ier )
  CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ier)
  CALL DWRITE('MPI_INIT')
# endif

  CALL READ_INPUT
  CALL DWRITE('READ_INPUT')

  ![ykchoi(14.12.24.)
  IF (INI_UVZ) THEN
    TIME=HotStartTime
    CALL DWRITE('Set HotStartTime')
  ENDIF
  !ykchoi(14.12.24.)]

  CALL INDEX
  CALL DWRITE('INDEX')

! allocate variables
  CALL ALLOCATE_VARIABLES
  CALL DWRITE('ALLOCATE_VARIABLES')

  CALL INITIALIZATION
  CALL DWRITE('INITIALIZATION')

   !CALL TIME_SPECTRA_PROTECTION
  IF(WaveMaker(1:12)=='TIME_SPECTRA') THEN
    CALL TIME_SPECTRA_INITIAL
    CALL DWRITE('TIME_SPECTRA_INITIAL')
  ENDIF

  CALL TIDE_INITIAL
  CALL DWRITE('TIDE_INITIAL')
  
# if defined (COUPLING)
  TIME=TIME_COUPLING_1
  CALL DWRITE('Set TIME_COUPLING_1')
# endif

# if defined (VESSEL)
  CALL VESSEL_INITIAL
  CALL DWRITE('VESSEL_INITIAL')
# endif

# if defined (METEO)
  CALL METEO_INITIAL
  CALL DWRITE('METEO_INITIAL')
# endif

# if defined (SEDIMENT)
  CALL SEDIMENT_INITIAL
  CALL DWRITE('SEDIMENT_INITIAL')
# endif

# if defined (FOAM)
  CALL ALLOCATE_FOAM
  CALL DWRITE('ALLOCATE_FOAM')
  CALL INITIALIZATION_FOAM
  CALL DWRITE('INITIALIZATION_FOAM')
# endif

#if defined (PRECIPITATION)
     CALL PRECIPITATION_INITIAL
# endif

#if defined (TRACKING)
  CALL TRACER_INITIAL
  CALL DWRITE('TRACER_INITIAL')
# endif

  CALL GET_TIME(tbegin)

! time integration

  DO WHILE (TIME<TOTAL_TIME)

!     move output here to get the initial condition 11/27/2018

    CALL OUTPUT
    CALL DWRITE('OUTPUT')

    IF  (WaveMaker(1:7)=='LEF_SOL') THEN
      CALL SOLITARY_WAVE_LEFT_BOUNDARY
      CALL DWRITE('SOLITARY_WAVE_LEFT_BOUNDARY')
    ENDIF   

! update three variables
    Eta0=Eta
    Ubar0=Ubar
    Vbar0=Vbar
    ! previous update_mask
    CALL DWRITE('UPDATE Eta, Ubar, & Vbar')

# if defined (SEDIMENT)
    CHH0=CHH
    CALL DWRITE('UPDATE CHH') 
# endif

    CALL EXCHANGE
    CALL DWRITE('EXCHANGE')

# if defined (COUPLING)
    CALL OneWayCoupling
    CALL DWRITE('OneWayCoupling')
# endif

    CALL ESTIMATE_DT(Mloc,Nloc,DX,DY,U,V,H,MinDepthFrc,DT,CFL,TIME)
    CALL DWRITE('ESTIMATE_DT')
  
! U0, V0 are moved to following part due to computation of Ut, Vt.
    U0=U   !ykchoi(15. 08. 06.)
    V0=V   !ykchoi

# if defined (VESSEL)
    CALL VESSEL_FORCING
    CALL DWRITE('VESSEL_FORCING')
# endif

# if defined (METEO)
    CALL METEO_FORCING
    CALL DWRITE('METEO_FORCING')
# endif

    IF (TideBcType(1:4)=='DATA') THEN
      CALL TIDE_DATA
      CALL DWRITE('TIDE_DATA')
    END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'PASS CALL TIDE_DATA'     
#    else   
       WRITE(*,*) 'PASS CALL TIDE_DATA'
#    endif
# endif

       IF(WaveMaker(1:12) == 'TIME_SPECTRA') THEN
         CALL TIME_SPECTRA_INTERPOLATION
       ENDIF

# if defined (DEBUG)
#    if defined (PARALLEL)
       IF(myid==0) WRITE(*,*) 'PASS CALL TIME_SPECTRA_INTERPOLATION'     
#    else   
       WRITE(*,*) 'PASS CALL TIME_SPECTRA_INTERPOLATION'
#    endif
# endif


#if defined (PRECIPITATION)
     CALL PRECIPITATION_DISTRIBUTION
# endif

     ! 3-ORDER RUNGE-KUTTA TIME STEPPING
    DO ISTAGE=1,3

      IF (DISPERSION) THEN
        CALL Cal_Dispersion
        CALL DWRITE('Cal_Dispersion')
      ENDIF 

      CALL FLUXES
      CALL DWRITE('FLUXES')

      ! put sourceterms after fluxes in order to get eta_t
      CALL SourceTerms   
      CALL DWRITE('SourceTerms')

      CALL ESTIMATE_HUV(ISTAGE) 
      CALL DWRITE('ESTIMATE_HUV')

      ! etascreen was added, update_mask was moved here from outside RK 
      CALL UPDATE_MASK
      CALL DWRITE('UPDATE_MASK')

      IF (TIDAL_BC_ABS) THEN
         CALL TIDE_BC
         CALL DWRITE('TIDE_BC')
      END IF

# if defined (SEDIMENT)
      CALL SEDIMENT_ADVECTION_DIFFUSION(ISTAGE) 
      CALL DWRITE('SEDIMENT_ADVECTION_DIFFUSION')
# endif

      CALL WAVE_BREAKING
      CALL DWRITE('WAVE_BREAKING')

# if defined (FOAM)
      CALL FOAM_FLUX
      CALL DWRITE('FOAM_FLUX')       
      CALL FOAM_UPDATE
      CALL DWRITE('FOAM_UPDATE')
      CALL FOAM_BC
      CALL DWRITE('FOAM_BC')
# endif
       
      CALL EXCHANGE
      CALL DWRITE('EXCHANGE')
       
# if defined (COUPLING)
      CALL OneWayCoupling
      CALL DWRITE('OneWayCoupling')
# endif         

      !IF (WaveMaker(1:3)=='ABS') THEN
      !  CALL ABSORBING_GENERATING_BC
      !  CALL DWRITE('ABSORBING_GENERATING_BC')
      !ENDIF

      !IF (WaveMaker(1:11)=='LEFT_BC_IRR') THEN
      !  CALL IRREGULAR_LEFT_BC
      !  CALL DWRITE('IRREGULAR_LEFT_BC')
      !ENDIF
      IF(WaveMaker(1:3)=='ABS' .OR. WaveMaker(1:12)=='TIME_SPECTRA') THEN
        CALL ABSORBING_GENERATING_BC
        CALL DWRITE('ABSORBING_GENERATING_BC')
      ENDIF

      IF (DIRECT_SPONGE) THEN
        CALL SPONGE_DAMPING
        CALL DWRITE('SPONGE_DAMPING')
      ENDIF

    ! End RK Loop
    END DO

# if defined (SEDIMENT)
    ! moved out of the RK loop 07/02/2019
    IF (Bed_Change) THEN
      CALL MORPHOLOGICAL_CHANGE
      CALL DWRITE('MORPHOLOGICAL_CHANGE')
    END IF
# endif

    CALL MIXING_STUFF
    CALL DWRITE('MIXING_STUFF')


# if defined (TRACKING)
    CALL TRACK_XY
    CALL DWRITE('TRACK_XY')
# endif

    ! find maximum eta velocity
    IF (OUT_Hmax.OR.OUT_Hmin.OR.OUT_Umax.OR.OUT_MFmax.OR.OUT_VORmax.OR.OUT_Time)THEN
      CALL MAX_MIN_PROPERTY
      CALL DWRITE('MAX_MIN_PROPERTY')
    ENDIF        

    CALL CHECK_BLOWUP 
    CALL DWRITE('CHECK_BLOWUP')

  ! End Time Loop
  END DO

  IF (NumberStations>0 ) THEN
    CALL STATIONS_V2
    CALL DWRITE('STATIONS_V2')
!# if defined (SPHERICAL_IJ_STATION)
!    ! use IJ for coupling purpose
!    CALL STATIONS_SPHERICAL_IJ
!    CALL DWRITE('STATIONS_SPHERICAL_IJ')
!# else
!    CALL STATIONS
!    CALL DWRITE('STATIONS')
!# endif
  END IF

# if defined (COUPLING)
  CLOSE(11)
  DWRITE('CLOSE COUPLING FILE')
# endif

# if defined (VESSEL)
  DO Kves = vunit+1,vunit+NumVessel
    CLOSE(Kves)
  ENDDO
  CALL DWRITE('CLOSE Vessel files')
# endif

  ! record wall time at the end
  CALL GET_TIME(tend)
  CALL FORMAT_ELAPSED(tend-tbegin, msg_buff)
  CALL LWRITE("")
  CALL BWRITE('SUMMARY')
  CALL LWRITE('Simulation took '//TRIM(msg_buff))
  CALL LWRITE('Normal Termination!')

# if defined (PARALLEL)
  CALL MPI_FINALIZE(ier)
# endif

END PROGRAM FUNWAVE_TVD


!-------------------------------------------------------------------------------------
! This part is not subroutines
!  DEFINITIONS OF VARIABLES
! 
!    Last Update: 02/18/2016 Fengyan Shi
!-------------------------------------------------------------------------------------
!
! Depth(): still water depth at element point
! DepthNode(): still water depth at node
! DepthX(): still water depth at x-interface
! DepthY(): still water depth at y-interface
! Eta():   surface elevation
! Eta0(): Eta at previous time level
!  for dry point, Eta() = MinDepth+Z()
! MASK(): 1 - wet
!         0 - dry
! MASK_STRUC(): 0 - permanent dry point
! MASK9: mask for itself and 8 elements around
! 
! U():  depth-averaged u or u at the reference level (u_alpha) at element
! V():  depth-averaged v or v at the reference level (v_alpha) at element
! HU(): (dep+eta)*u at element
! HV(): (dep+eta)*v at element
! P(): HU + dispersion at x-interface
! Q(): HV + dispersion at y-interface
! Fx(): F at x-interface
! Fy(): F at y-interface
! Gx(): G at x-interface
! Gy(): G at y-interface
! Ubar(:,:,:): Ubar
! Vbar(:,:,:): Vbar

! dispersion
! U1p(:,:): x-component of V1p
! V1p(:,:): y-component of V1p

! 
! EtaRxL(): Eta Left value at x-interface
! EtaRxR(): Eta Right value at x-interface
! EtaRyL(): Eta Left value at y-interface
! EtaRyR(): Eta Right value at y-interface
! HxL():   total depth  Left value at x-interface
! HxR():   total depth  Right value at x-interface
! HyL():   total depth  Left value at y-interface
! HyR():   total depth  Right value at y-interface

! HUxL(): HU Left value at x-interface
! HUxR(): HU Right value at x-interface
! HUyL(): HV Left value at y-interface
! HUyR(): HV Right value at y-interface

! PL(): HU + dispersion, Left value at x-interface
! PR(): HU + dispersion, Right value at x-interface
! QL(): HV + dispersion, Left value at y-interface
! QR(): HV + dispersion, Right value at y-interface

! FxL = HUxL*UxL + 1/2*g*(EtaRxL^2 + 2*EtaRxL*Depthx)
! FxR = HUxR*UxR + 1/2*g*(EtaRxR^2 + 2*EtaRxR*Depthx)
! FyL = HyL*UyL*VyL
! FyR = HyR*UyR*VyR

! GxL = HxL*UxL*VxL
! GxR = HxR*UxR*VxR
! GyL = HVyL*VyL + 1/2*g*(EtaRyL^2 + 2*EtaRyL*Depthy)
! GyR = HVyR*VyR + 1/2*g*(EtaRyR^2 + 2*EtaRyR*Depthy) 
