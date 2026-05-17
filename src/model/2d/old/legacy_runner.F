!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  run_legacy_2d — callable wrapper around the legacy simulation loop.
!
!  Extracted from main.F so the launcher can invoke the full simulation
!  after model%init_from_env() has already read YAML config.
!  READ_INPUT (inside this subroutine) re-reads config via model%init()
!  to populate MODULE GLOBAL variables used by the legacy kernels.
!  MPI_FINALIZE is intentionally omitted — the launcher handles it.
!
!  HISTORY :
!    05/28/2026  Michael-Angelo Y.H. Lam
!
!--------------------------------------------------

   subroutine run_legacy_2d()

   USE GLOBAL

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

   CALL READ_INPUT

# if defined (DEBUG)
#    if defined (PARALLEL)
   IF (myid == 0) WRITE (*, *) 'READ_INPUT'
#    else
   WRITE (*, *) 'READ_INPUT'
#    endif
# endif

   ![ykchoi(14.12.24.)
   IF (INI_UVZ) THEN
      TIME = HotStartTime
   END IF
   !ykchoi(14.12.24.)]

   CALL INDEX

# if defined (DEBUG)
#    if defined (PARALLEL)
   IF (myid == 0) WRITE (*, *) 'INDEX'
#    else
   WRITE (*, *) 'INDEX'
#    endif
# endif

! allocate variables
   CALL ALLOCATE_VARIABLES

# if defined (DEBUG)
#    if defined (PARALLEL)
   IF (myid == 0) WRITE (*, *) 'ALLOCATION'
#    else
   WRITE (*, *) 'ALLOCATION'
#    endif
# endif

   CALL INITIALIZATION

! time dependent wave spectra

   IF (WaveMaker(1:12) == 'TIME_SPECTRA') THEN
      CALL TIME_SPECTRA_INITIAL
   END IF

   CALL TIDE_INITIAL

   IF (WaveMaker(1:12) == 'TIME_SPECTRA') THEN
      CALL TIME_SPECTRA_PROTECTION
   END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
   IF (myid == 0) WRITE (*, *) 'INITIALIZATION'
#    else
   WRITE (*, *) 'INITIALIZATION'
#    endif
# endif

# if defined (COUPLING)
   TIME = TIME_COUPLING_1
# endif

# if defined (VESSEL)
   CALL VESSEL_INITIAL
# endif

# if defined (METEO)
   CALL METEO_INITIAL
# endif

# if defined (SEDIMENT)
   CALL SEDIMENT_INITIAL
# endif

# if defined (FOAM)
   CALL ALLOCATE_FOAM
   CALL INITIALIZATION_FOAM
# endif

#if defined (PRECIPITATION)
   CALL PRECIPITATION_INITIAL
# endif

#if defined (TRACKING)
   CALL TRACER_INITIAL
# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
   IF (myid == 0) WRITE (*, *) 'INITIAL VESSEL, METEO, OR SEDIMENT if included'
#    else
   WRITE (*, *) 'INITIAL VESSEL, METEO, OR SEDIMENT if included'
#    endif
# endif

! time integration

   ! record wall time
# if defined (PARALLEL)
   if (myid == 0) tbegin = MPI_Wtime()
# else
   CALL CPU_TIME(tbegin)
# endif

   DO WHILE (TIME < TOTAL_TIME)

!     move output here to get the initial condition 11/27/2018

      CALL OUTPUT

      IF (WaveMaker(1:7) == 'LEF_SOL') THEN
         CALL SOLITARY_WAVE_LEFT_BOUNDARY
      END IF

! update three variables
      Eta0 = Eta
      Ubar0 = Ubar
      Vbar0 = Vbar
# if defined (SEDIMENT)
      CHH0 = CHH
# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'UPDATE Eta Ubar Vbar, MASK'
#    else
      WRITE (*, *) 'UPDATE Eta Ubar Vbar, MASK'
#    endif
# endif

      CALL EXCHANGE

# if defined (COUPLING)
      CALL OneWayCoupling
# endif

      CALL ESTIMATE_DT(Mloc, Nloc, DX, DY, U, V, H, MinDepthFrc, DT, CFL, TIME)

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'ESTIMATE_DT'
#    else
      WRITE (*, *) 'ESTIMATE_DT'
#    endif
# endif

! U0, V0 are moved to following part due to computation of Ut, Vt.
      U0 = U   !ykchoi(15. 08. 06.)
      V0 = V   !ykchoi

# if defined (VESSEL)
      CALL VESSEL_FORCING
# endif

# if defined (METEO)
      CALL METEO_FORCING
# endif

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'vessel, meteo forcing, if included'
#    else
      WRITE (*, *) 'vessel, meteo forcing, if included'
#    endif
# endif

      IF (TideBcType(1:4) == 'DATA') THEN
         CALL TIDE_DATA
      END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'PASS CALL TIDE_DATA'
#    else
      WRITE (*, *) 'PASS CALL TIDE_DATA'
#    endif
# endif

      IF (WaveMaker(1:12) == 'TIME_SPECTRA') THEN
         CALL TIME_SPECTRA_INTERPOLATION
      END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'PASS CALL TIME_SPECTRA_INTERPOLATION'
#    else
      WRITE (*, *) 'PASS CALL TIME_SPECTRA_INTERPOLATION'
#    endif
# endif

#if defined (PRECIPITATION)
      CALL PRECIPITATION_DISTRIBUTION
# endif

      ! 3-ORDER RUNGE-KUTTA TIME STEPPING
      DO ISTAGE = 1, 3

         IF (DISPERSION) THEN
            CALL Cal_Dispersion
         END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'Cal_Dispersion'
#    else
         WRITE (*, *) 'Cal_Dispersion'
#    endif
# endif

         CALL FLUXES

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'FLUXES'
#    else
         WRITE (*, *) 'FLUXES'
#    endif
# endif

         CALL SourceTerms   ! put sourceterms after fluxes in order to get eta_t

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'SourceTerms'
#    else
         WRITE (*, *) 'SourceTerms'
#    endif
# endif

         CALL ESTIMATE_HUV(ISTAGE)

! etascreen was added, update_mask was moved here from outside RK
         CALL UPDATE_MASK

         IF (TIDAL_BC_ABS) THEN
            CALL TIDE_BC
         END IF

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'ESTIMATE_HUV'
#    else
         WRITE (*, *) 'ESTIMATE_HUV'
#    endif
# endif

# if defined (SEDIMENT)
         CALL SEDIMENT_ADVECTION_DIFFUSION(ISTAGE)
# endif

         CALL WAVE_BREAKING

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'WAVE_BREAKING'
#    else
         WRITE (*, *) 'WAVE_BREAKING'
#    endif
# endif

# if defined (FOAM)
         CALL FOAM_FLUX
         CALL FOAM_UPDATE
         CALL FOAM_BC
# endif

         CALL EXCHANGE

# if defined (DEBUG)
#    if defined (PARALLEL)
         IF (myid == 0) WRITE (*, *) 'EXCHANGE'
#    else
         WRITE (*, *) 'EXCHANGE'
#    endif
# endif

# if defined (COUPLING)
         CALL OneWayCoupling
# endif

         IF (WaveMaker(1:3) == 'ABS' .OR. WaveMaker(1:12) == 'TIME_SPECTRA') THEN
            CALL ABSORBING_GENERATING_BC

# if defined (DEBUG)
#    if defined (PARALLEL)
            IF (myid == 0) WRITE (*, *) 'ABS or TIME_SPECTRA wavemaker'
#    else
            WRITE (*, *) 'ABS or TIME_SPECTRA wavemakerwavemaker'
#    endif
# endif

         END IF

         IF (WaveMaker(1:11) == 'LEFT_BC_IRR') THEN
            CALL IRREGULAR_LEFT_BC
         END IF

         IF (DIRECT_SPONGE) THEN
            CALL SPONGE_DAMPING
         END IF

      END DO

# if defined (SEDIMENT)
!   moved out of the RK loop 07/02/2019
      IF (Bed_Change) THEN
         CALL MORPHOLOGICAL_CHANGE
      END IF
# endif

      CALL MIXING_STUFF

# if defined (DEBUG)
#    if defined (PARALLEL)
      IF (myid == 0) WRITE (*, *) 'MIXING_STUFF'
#    else
      WRITE (*, *) 'MIXING_STUFF'
#    endif
# endif

# if defined (TRACKING)
      CALL TRACK_XY
# endif

!  find maximum eta velocity

      IF (OUT_Hmax .OR. OUT_Hmin .OR. OUT_Umax .OR. OUT_MFmax .OR. OUT_VORmax .OR. OUT_Time) THEN
         CALL MAX_MIN_PROPERTY
      END IF

      CALL CHECK_BLOWUP

   END DO

   IF (NumberStations > 0) THEN
# if defined (SPHERICAL_IJ_STATION)
      CALL STATIONS_SPHERICAL_IJ
# else
      CALL STATIONS
# endif
   END IF

# if defined (COUPLING)
   CLOSE (11)
# endif

# if defined (VESSEL)
   DO Kves = 201, 200 + NumVessel
      CLOSE (Kves)
   END DO
# endif

   ! record wall time at the end
# if defined (PARALLEL)
   if (myid .eq. 0) tend = MPI_Wtime()
# else
   CALL CPU_TIME(tend)
# endif

# if defined (PARALLEL)
   if (myid .eq. 0) write (*, *) 'Simulation takes', tend - tbegin, 'seconds'
   if (myid .eq. 0) write (3, *) 'Simulation takes', tend - tbegin, 'seconds'
   if (myid .eq. 0) WRITE (*, *) 'Normal Termination!'
   if (myid .eq. 0) WRITE (3, *) 'Normal Termination!'
# else
   write (*, *) 'Simulation takes', tend - tbegin, 'seconds'
   write (3, *) 'Simulation takes', tend - tbegin, 'seconds'
   WRITE (*, *) 'Normal Termination!'
   WRITE (3, *) 'Normal Termination!'
# endif

   end subroutine run_legacy_2d
