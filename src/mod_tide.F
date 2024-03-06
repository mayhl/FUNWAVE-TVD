!------------------------------------------------------------------------------------
!
!      FILE mod_tide.F
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
!  TIDE_MODULE is a module to add tide/surge boundary conditions into wave simulation    
!
!  HISTORY :
!    11/26/2019 Fengyan Shi
!-------------------------------------------------------------------------------------

MODULE TIDE_MODULE
  USE PARAM
  USE GLOBAL,ONLY : Mloc,Nloc,Mloc1,Nloc1,Nghost,Ibeg,Iend,Jbeg,Jend,DX,DY, &
                    H,ETA,U,V,P,Q,MinDepth,MASK,DT,Gamma3,Depth,tmp4preview, &
                    ALPHA,BETA,MASK9,DepthX,DepthY,PERIODIC, &
                    UNDERTOW_U, UNDERTOW_V, ROLLER_SWITCH,ROLLER,Mglob,Nglob, &
                    TIME,Sponge_west_width,Sponge_east_width,Sponge_south_width, &
                    Sponge_north_width
  USE INPUT_READ
  USE LOG_IO
#if defined (PARALLEL)
  USE GLOBAL,ONLY : myid,ier, npx,npy,PX,PY
  USE MPI
# endif
  IMPLICIT NONE
  SAVE

  CHARACTER(LEN=80) TideBcType
  CHARACTER(LEN=80) TideDataFileName
  CHARACTER(LEN=80) TideWestFileName,TideEastFileName,TideNorthFileName,TideSouthFileName
  CHARACTER(LEN=80) tmp_name
  LOGICAL :: TIDAL_BC_ABS=.FALSE.
  LOGICAL :: TIDAL_BC_GEN_ABS=.FALSE.
  LOGICAL :: TideEast=.TRUE.
  LOGICAL :: TideWest=.TRUE.
  LOGICAL :: TideNorth=.TRUE.
  LOGICAL :: TideSouth=.TRUE.
  REAL(SP) :: TideEast_U,TideEast_V,TideEast_ETA
  REAL(SP) :: TideWest_U,TideWest_V,TideWest_ETA
  REAL(SP) :: TideNorth_U,TideNorth_V,TideNorth_ETA
  REAL(SP) :: TideSouth_U,TideSouth_V,TideSouth_ETA
  REAL(SP),DIMENSION(:,:),ALLOCATABLE :: SPONGE_TIDE_WEST, &
              SPONGE_TIDE_EAST,SPONGE_TIDE_SOUTH,SPONGE_TIDE_NORTH
  INTEGER :: Iwidth,Ifile,Iwidth_tide
  REAL(SP) :: Time_tide_west_1,Eta_tide_west_1,U_tide_west_1,V_tide_west_1, &
              Time_tide_west_2,Eta_tide_west_2,U_tide_west_2,V_tide_west_2, &
              Time_tide_east_1,Eta_tide_east_1,U_tide_east_1,V_tide_east_1, &
              Time_tide_east_2,Eta_tide_east_2,U_tide_east_2,V_tide_east_2, &
              Time_tide_south_1,Eta_tide_south_1,U_tide_south_1,V_tide_south_1, &
              Time_tide_south_2,Eta_tide_south_2,U_tide_south_2,V_tide_south_2, &
              Time_tide_north_1,Eta_tide_north_1,U_tide_north_1,V_tide_north_1, &
              Time_tide_north_2,Eta_tide_north_2,U_tide_north_2,V_tide_north_2  

  INTEGER :: tdunit 
  TYPE(LOG_BUFFER), TARGET :: logbuff
  ! Flag for bypassing error when input value is not
  ! found and returns if input value is found
  LOGICAL :: found

CONTAINS
  
SUBROUTINE TIDE_INITIAL
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL,INPUT_FILE_NAME,WaveMaker
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif

  USE INPUT_READ
  IMPLICIT NONE

  CHARACTER(LEN=80)::FILE_NAME=' '
  CHARACTER(LEN=80)::TMP_NAME=' '
  INTEGER :: Ifile,ierr

  ! read  from input.txt
  FILE_NAME=INPUT_FILE_NAME
  CALL SET_INPUT_LOGGER(FILE_NAME, logbuff)

  CALL BWRITE('TIDAL INFO')


! width of sponge
  CALL READ_PARAMETER(Iwidth_tide, 'WaveMakerPointNum', 30, 'points')

  CALL READ_PARAMETER(TIDAL_BC_ABS    , 'TIDAL_BC_ABS'    , .FALSE.)
  CALL READ_PARAMETER(TIDAL_BC_GEN_ABS, 'TIDAL_BC_GEN_ABS', .FALSE.)
!   tidal bc ---------
   IF(TIDAL_BC_GEN_ABS.OR.TIDAL_BC_ABS)THEN
  
    CALL READ_PARAMETER(TideBcType      , 'TideBcType'      , 'CONSTANT')

    IF(TideBcType(1:4)=='CONS')THEN
 
      CALL LWRITE("Using CONSTANT Tidal BC.")
      CALL Tide_READ_CONSTANT

    ELSEIF(TideBcType(1:4)=='DATA')THEN
    
      CALL LWRITE("Using DATA Tidal BC.")
      CALL Tide_READ_DATA_INIT

    ELSE
      CALL SWRITE("INVALID VALUE FOR 'TideBcType'.")
    ENDIF

    ALLOCATE(SPONGE_TIDE_WEST(Mloc,Nloc) ,SPONGE_TIDE_EAST(Mloc,Nloc), &
             SPONGE_TIDE_SOUTH(Mloc,Nloc),SPONGE_TIDE_NORTH(Mloc,Nloc))

    CALL TIDE_SPONGE
  ENDIF

  IF (TIDAL_BC_ABS) THEN
    CALL LWRITE("TIDAL_BC_ABS is defined.")
    CALL REMOVE_SPONGE
  ENDIF ! end tide abs

  CALL logbuff % FLUSH2OUTPUT

  CALL CLEAR_INPUT_LOGGER

END SUBROUTINE TIDE_INITIAL


SUBROUTINE TIDE_SPONGE
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
                    
  IMPLICIT NONE
  REAL(SP) :: ri,lim,Lstart,Lend,R_sp,A_sp
  
  Iwidth = Iwidth_tide ! specified in read or default = 30
  lim = 1.0_SP
  R_sp = 0.85_SP
  A_sp = 10.0_SP

! west

       do j = 1,Nloc
       do i = 1,Mloc
# if defined(PARALLEL)
         ri = R_sp**(50*(i+npx*Mglob/px-1)/(Iwidth-1))
# else
         ri = R_sp**(50*(i-1)/(Iwidth-1))
# endif
         SPONGE_TIDE_WEST(i,j) = max(A_sp**ri,lim)
       enddo
       enddo

! east
       do j = 1,Nloc
       do i = 1,Mloc
# if defined(PARALLEL)
         ri = R_sp**(50*(Mloc-i+(px-npx-1)*Mglob/px)/(Iwidth-1))
# else
         ri = R_sp**(50*(Mloc-i)/(Iwidth-1))
# endif
         SPONGE_TIDE_EAST(i,j) = max(A_sp**ri,lim)
       enddo
       enddo

! south

       do j = 1,Nloc
       do i = 1,Mloc
# if defined(PARALLEL)
         ri = R_sp**(50*(j+npy*Nglob/py-1)/(Iwidth-1))
# else
         ri = R_sp**(50*(j-1)/(Iwidth-1))
# endif
         SPONGE_TIDE_SOUTH(i,j) = max(A_sp**ri,lim)
       enddo
       enddo

! north
       do j = 1,Nloc
       do i = 1,Mloc
# if defined(PARALLEL)
         ri = R_sp**(50*(Nloc-j+(py-npy-1)*Nglob/py)/(Iwidth-1))
# else
         ri = R_sp**(50*(Nloc-i)/(Iwidth-1))
# endif
         SPONGE_TIDE_NORTH(i,j) = max(A_sp**ri,lim)
       enddo
       enddo

       SPONGE_TIDE_WEST = 1.0_SP/SPONGE_TIDE_WEST
       SPONGE_TIDE_EAST = 1.0_SP/SPONGE_TIDE_EAST
       SPONGE_TIDE_SOUTH = 1.0_SP/SPONGE_TIDE_SOUTH
       SPONGE_TIDE_NORTH = 1.0_SP/SPONGE_TIDE_NORTH



END SUBROUTINE TIDE_SPONGE

SUBROUTINE TIDE_BC
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
                    
  IMPLICIT NONE

   IF(TideWest)THEN
     DO J=1,Nloc
     DO I=1,Iwidth
       IF(MASK(I,J)==1)THEN
         ETA(I,J)=TideWest_ETA +(ETA(I,J)-TideWest_ETA)*SPONGE_TIDE_WEST(I,J)
         U(I,J)=TideWest_U +(U(I,J)-TideWest_U)*SPONGE_TIDE_WEST(I,J)
         V(I,J)=TideWest_V +(V(I,J)-TideWest_V)*SPONGE_TIDE_WEST(I,J)
       ENDIF
     ENDDO
     ENDDO
   ENDIF

   IF(TideEast)THEN
     DO J=1,Nloc
     DO I=Mloc-Iwidth+1,Mloc
       IF(MASK(I,J)==1)THEN
         ETA(I,J)=TideEast_ETA +(ETA(I,J)-TideEast_ETA)*SPONGE_TIDE_EAST(I,J)
         U(I,J)=TideEast_U +(U(I,J)-TideEast_U)*SPONGE_TIDE_EAST(I,J)
         V(I,J)=TideEast_V +(V(I,J)-TideEast_V)*SPONGE_TIDE_EAST(I,J)
       ENDIF
     ENDDO
     ENDDO
   ENDIF

   IF(TideSouth)THEN
     DO J=1,Iwidth
     DO I=1,Mloc
       IF(MASK(I,J)==1)THEN
         ETA(I,J)=TideSouth_ETA +(ETA(I,J)-TideSouth_ETA)*SPONGE_TIDE_SOUTH(I,J)
         U(I,J)=TideSouth_U +(U(I,J)-TideSouth_U)*SPONGE_TIDE_SOUTH(I,J)
         V(I,J)=TideSouth_V +(V(I,J)-TideSouth_V)*SPONGE_TIDE_SOUTH(I,J)
       ENDIF
     ENDDO
     ENDDO
   ENDIF

   IF(TideNorth)THEN
     DO J=Nloc-Iwidth+1,Nloc
     DO I=1,Mloc
       IF(MASK(I,J)==1)THEN
         ETA(I,J)=TideNorth_ETA +(ETA(I,J)-TideNorth_ETA)*SPONGE_TIDE_NORTH(I,J)
         U(I,J)=TideNorth_U +(U(I,J)-TideNorth_U)*SPONGE_TIDE_NORTH(I,J)
         V(I,J)=TideNorth_V +(V(I,J)-TideNorth_V)*SPONGE_TIDE_NORTH(I,J)
       ENDIF
     ENDDO
     ENDDO
   ENDIF

END SUBROUTINE TIDE_BC

SUBROUTINE Tide_READ_CONSTANT
                    
  USE INPUT_READ
  USE LOG_IO
  IMPLICIT NONE


  CALL READ_PARAMETER(TideWest_ETA, 'TideWest_ETA', found=TideWest)
  IF (.NOT.TideWest) THEN
    CALL LWRITE("TideWest_ETA not specified, no West boundary tide.")
    TideWest_U = 0.0_SP
    TideWest_V = 0.0_SP
  ELSE
    CALL READ_PARAMETER(TideWest_U  , 'TideWest_U'  , 0.0_SP)
    CALL READ_PARAMETER(TideWest_V  , 'TideWest_V'  , 0.0_SP)
  ENDIF

  CALL READ_PARAMETER(TideEast_ETA, 'TideEast_ETA', found=TideEast)
  IF (.NOT.TideEast) THEN
    CALL LWRITE("TideEast_ETA not specified, no East boundary tide.")
    TideEast_U = 0.0_SP
    TideEast_V = 0.0_SP
  ELSE
    CALL READ_PARAMETER(TideEast_U  , 'TideEast_U'  , 0.0_SP)
    CALL READ_PARAMETER(TideEast_V  , 'TideEast_V'  , 0.0_SP)
  ENDIF

  CALL READ_PARAMETER(TideSouth_ETA, 'TideSouth_ETA', found=TideSouth)
  IF (.NOT.TideSouth) THEN
    CALL LWRITE("TideSouth_ETA not specified, no South boundary tide.")
    TideSouth_U = 0.0_SP
    TideSouth_V = 0.0_SP
  ELSE
    CALL READ_PARAMETER(TideSouth_U  , 'TideSouth_U'  , 0.0_SP)
    CALL READ_PARAMETER(TideSouth_V  , 'TideSouth_V'  , 0.0_SP)
  ENDIF

  CALL READ_PARAMETER(TideNorth_ETA, 'TideNorth_ETA', found=TideNorth)
  IF (.NOT.TideNorth) THEN
    CALL LWRITE("TideNorth_ETA not specified, no North boundary tide.")
    TideNorth_U = 0.0_SP
    TideNorth_V = 0.0_SP
  ELSE
    CALL READ_PARAMETER(TideNorth_U  , 'TideNorth_U'  , 0.0_SP)
    CALL READ_PARAMETER(TideNorth_V  , 'TideNorth_V'  , 0.0_SP)
  ENDIF

END SUBROUTINE Tide_READ_CONSTANT


SUBROUTINE REMOVE_SPONGE
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL,INPUT_FILE_NAME,WaveMaker
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
 
  IMPLICIT NONE

  IF (TideWest.AND..NOT.ALMOST_EQUAL(Sponge_west_width, 0.0_SP)) THEN
    CALL LWRITE("Both west boundary tide and sponge layer set. Removing sponge layer.")
    Sponge_west_width = 0.0_SP
  ENDIF 

  IF (TideEast.AND..NOT.ALMOST_EQUAL(Sponge_east_width, 0.0_SP)) THEN
    CALL LWRITE("Both east boundary tide and sponge layer set. Removing sponge layer.")
    Sponge_east_width = 0.0_SP
  ENDIF

  IF (TideSouth.AND..NOT.ALMOST_EQUAL(Sponge_south_width, 0.0_SP)) THEN
    CALL LWRITE("Both south boundary tide and sponge layer set. Removing sponge layer.")
    Sponge_south_width = 0.0_SP
  ENDIF 

  IF (TideNorth.AND..NOT.ALMOST_EQUAL(Sponge_north_width, 0.0_SP)) THEN
    CALL LWRITE("Both north boundary tide and sponge layer set. Removing sponge layer.")
    Sponge_north_width = 0.0_SP
  ENDIF 

  IF (.NOT.TideWest.AND. & !.NOT.TideNorth.AND. &
      .NOT.TideEast) THEN !.AND..NOT.TideSouth ) THEN

    TIDAL_BC_ABS = .FALSE.
    CALL LWRITE('Tides specified, but not BCs, setting tides as false.')

  END IF

END SUBROUTINE REMOVE_SPONGE

SUBROUTINE Tide_READ_DATA_INIT
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL,INPUT_FILE_NAME,WaveMaker
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
                    
  USE INPUT_READ
  USE LOG_IO
  USE MOD_FUNITS, ONLY: fhandle 
  IMPLICIT NONE

  INTEGER :: Ifile

  tdunit = fhandle % ADD_RANGE('tide_data', 4)

  ! -------------------------------------
  !        West Tide
  ! -------------------------------------
  CALL READ_PARAMETER(TideWestFileName, 'TideWestFileName', is_file=.TRUE., found=TideWest)

  IF (TideWest) THEN
    
    Ifile = tdunit
    OPEN(Ifile, FILE=TideWestFileName)

    READ(Ifile,'(A80)')  tmp_name 
    READ(Ifile,*) Time_tide_west_2, Eta_tide_west_2, U_tide_west_2, V_tide_west_2

    Time_tide_west_1 = Time_tide_west_2 
    Eta_tide_west_1 = Eta_tide_west_2 
    U_tide_west_1 = U_tide_west_2 
    V_tide_west_1 = V_tide_west_2

    CALL LWRITE('Read West tide ------')
    CALL LWRITE('Time, Eta, U, V = '//&
                 FORMAT_FLOAT(Time_tide_west_2)//", "//&
                 FORMAT_FLOAT(Eta_tide_west_2)//", "//&
                 FORMAT_FLOAT(U_tide_west_2)//", "//&
                 FORMAT_FLOAT(V_tide_west_2))

  END IF

  ! -------------------------------------
  !        East Tide
  ! -------------------------------------
  CALL READ_PARAMETER(TideEastFileName, 'TideEastFileName', is_file=.TRUE., found=TideEast)

  IF (TideEast) THEN
    
    Ifile = tdunit + 1
    OPEN(Ifile, FILE=TideEastFileName)

    READ(Ifile,'(A80)')  tmp_name 
    READ(Ifile,*) Time_tide_east_2, Eta_tide_east_2, U_tide_east_2, V_tide_east_2

    Time_tide_east_1 = Time_tide_east_2 
    Eta_tide_east_1 = Eta_tide_east_2 
    U_tide_east_1 = U_tide_east_2 
    V_tide_east_1 = V_tide_east_2

    CALL LWRITE('Read East tide ------')
    CALL LWRITE('Time, Eta, U, V = '//&
                 FORMAT_FLOAT(Time_tide_east_2)//", "//&
                 FORMAT_FLOAT(Eta_tide_east_2)//", "//&
                 FORMAT_FLOAT(U_tide_east_2)//", "//&
                 FORMAT_FLOAT(V_tide_east_2))

  ENDIF

  ! -------------------------------------
  !        South Tide
  ! -------------------------------------
  CALL READ_PARAMETER(TideSouthFileName, 'TideSouthFileName', is_file=.TRUE., found=TideSouth)

  IF (TideSouth) THEN
    
    Ifile = tdunit + 2
    OPEN(Ifile, FILE=TideSouthFileName)

    READ(Ifile,'(A80)')  tmp_name 
    READ(Ifile,*) Time_tide_south_2, Eta_tide_south_2, U_tide_south_2, V_tide_south_2

    Time_tide_south_1 = Time_tide_south_2 
    Eta_tide_south_1 = Eta_tide_south_2 
    U_tide_south_1 = U_tide_south_2 
    V_tide_south_1 = V_tide_south_2

    CALL LWRITE('Read South tide ------')
    CALL LWRITE('Time, Eta, U, V = '//&
                 FORMAT_FLOAT(Time_tide_south_2)//", "//&
                 FORMAT_FLOAT(Eta_tide_south_2)//", "//&
                 FORMAT_FLOAT(U_tide_south_2)//", "//&
                 FORMAT_FLOAT(V_tide_south_2))

  ENDIF

  ! -------------------------------------
  !        North Tide
  ! -------------------------------------
  CALL READ_PARAMETER(TideNorthFileName, 'TideNorthFileName', is_file=.TRUE., found=TideNorth)

  IF (TideNorth) THEN
    
    Ifile = tdunit + 3
    OPEN(Ifile, FILE=TideNorthFileName)

    READ(Ifile,'(A80)')  tmp_name 
    READ(Ifile,*) Time_tide_north_2, Eta_tide_north_2, U_tide_north_2, V_tide_north_2

    Time_tide_north_1 = Time_tide_north_2 
    Eta_tide_north_1 = Eta_tide_north_2 
    U_tide_north_1 = U_tide_north_2 
    V_tide_north_1 = V_tide_north_2

    CALL LWRITE('Read North tide ------')
    CALL LWRITE('Time, Eta, U, V = '//&
                 FORMAT_FLOAT(Time_tide_north_2)//", "//&
                 FORMAT_FLOAT(Eta_tide_north_2)//", "//&
                 FORMAT_FLOAT(U_tide_north_2)//", "//&
                 FORMAT_FLOAT(V_tide_north_2))

  ENDIF

END SUBROUTINE Tide_READ_DATA_INIT

SUBROUTINE TIDE_DATA
  USE GLOBAL,ONLY : tmp1,tmp2,SMALL,TIME,ZERO,DT
  INTEGER :: Ifile
 
  ! -------------------------------------
  !        West Tide
  ! -------------------------------------
  IF (TideWest) THEN
  
    Ifile = tdunit

    IF (TIME.GT.Time_tide_west_1.AND.&
        TIME.GT.Time_tide_west_2) THEN

      Time_tide_west_1 = Time_tide_west_2 
      Eta_tide_west_1 = Eta_tide_west_2 
      U_tide_west_1 = U_tide_west_2 
      V_tide_west_1 = V_tide_west_2

      DO WHILE (Time_tide_west_2.LT.TIME+DT)
        READ(Ifile,*,END=120)  Time_tide_west_2, Eta_tide_west_2, U_tide_west_2, V_tide_west_2
      END DO

      CALL LWRITE('Time, Eta, U, V = '//&
                   FORMAT_FLOAT(Time_tide_west_2)//", "//&
                   FORMAT_FLOAT(Eta_tide_west_2)//", "//&
                   FORMAT_FLOAT(U_tide_west_2)//", "//&
                   FORMAT_FLOAT(V_tide_west_2))

    ENDIF ! end read time

! interpolate
    tmp2=ZERO
    tmp1=ZERO

    IF (TIME.GT.Time_tide_west_1) THEN
      IF(Time_tide_west_1.EQ.Time_tide_west_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
        tmp2=(Time_tide_west_2-TIME) &
            /MAX(SMALL, ABS(Time_tide_west_2-Time_tide_west_1))
        tmp1=1.0_SP - tmp2
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    TideWEST_U = U_tide_west_2*tmp1 +U_tide_west_1*tmp2
    TideWEST_V = V_tide_west_2*tmp1 +V_tide_west_1*tmp2
    TideWEST_ETA = ETA_tide_west_2*tmp1 +ETA_tide_west_1*tmp2

  ENDIF

  ! -------------------------------------
  !        East Tide
  ! -------------------------------------
  IF (TideEast) THEN
  
    Ifile = tdunit + 1

    IF (TIME.GT.Time_tide_east_1.AND.&
        TIME.GT.Time_tide_east_2) THEN

      Time_tide_east_1 = Time_tide_east_2 
      Eta_tide_east_1 = Eta_tide_east_2 
      U_tide_east_1 = U_tide_east_2 
      V_tide_east_1 = V_tide_east_2

      DO WHILE (Time_tide_east_2.LT.TIME+DT)
        READ(Ifile,*,END=120)  Time_tide_east_2, Eta_tide_east_2, U_tide_east_2, V_tide_east_2
      END DO

      CALL LWRITE('Time, Eta, U, V = '//&
                   FORMAT_FLOAT(Time_tide_east_2)//", "//&
                   FORMAT_FLOAT(Eta_tide_east_2)//", "//&
                   FORMAT_FLOAT(U_tide_east_2)//", "//&
                   FORMAT_FLOAT(V_tide_east_2))

    ENDIF ! end read time

! interpolate
    tmp2=ZERO
    tmp1=ZERO

    IF (TIME.GT.Time_tide_east_1) THEN
      IF(Time_tide_east_1.EQ.Time_tide_east_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
        tmp2=(Time_tide_east_2-TIME) &
            /MAX(SMALL, ABS(Time_tide_east_2-Time_tide_east_1))
        tmp1=1.0_SP - tmp2
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    TideEAST_U = U_tide_east_2*tmp1 +U_tide_east_1*tmp2
    TideEAST_V = V_tide_east_2*tmp1 +V_tide_east_1*tmp2
    TideEAST_ETA = ETA_tide_east_2*tmp1 +ETA_tide_east_1*tmp2

  ENDIF

  ! -------------------------------------
  !        South Tide
  ! -------------------------------------
  IF (TideSouth) THEN
  
    Ifile = tdunit + 2

    IF (TIME.GT.Time_tide_south_1.AND.&
        TIME.GT.Time_tide_south_2) THEN

      Time_tide_south_1 = Time_tide_south_2 
      Eta_tide_south_1 = Eta_tide_south_2 
      U_tide_south_1 = U_tide_south_2 
      V_tide_south_1 = V_tide_south_2

      DO WHILE (Time_tide_south_2.LT.TIME+DT)
        READ(Ifile,*,END=120)  Time_tide_south_2, Eta_tide_south_2, U_tide_south_2, V_tide_south_2
      END DO

      CALL LWRITE('Time, Eta, U, V = '//&
                   FORMAT_FLOAT(Time_tide_south_2)//", "//&
                   FORMAT_FLOAT(Eta_tide_south_2)//", "//&
                   FORMAT_FLOAT(U_tide_south_2)//", "//&
                   FORMAT_FLOAT(V_tide_south_2))

    ENDIF ! end read time

! interpolate
    tmp2=ZERO
    tmp1=ZERO

    IF (TIME.GT.Time_tide_south_1) THEN
      IF(Time_tide_south_1.EQ.Time_tide_south_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
        tmp2=(Time_tide_south_2-TIME) &
            /MAX(SMALL, ABS(Time_tide_south_2-Time_tide_south_1))
        tmp1=1.0_SP - tmp2
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    TideSOUTH_U = U_tide_south_2*tmp1 +U_tide_south_1*tmp2
    TideSOUTH_V = V_tide_south_2*tmp1 +V_tide_south_1*tmp2
    TideSOUTH_ETA = ETA_tide_south_2*tmp1 +ETA_tide_south_1*tmp2

  ENDIF

  ! -------------------------------------
  !        North Tide
  ! -------------------------------------
  IF (TideNorth) THEN
  
    Ifile = tdunit + 3

    IF (TIME.GT.Time_tide_north_1.AND. &
        TIME.GT.Time_tide_north_2) THEN

      Time_tide_north_1 = Time_tide_north_2 
      Eta_tide_north_1 = Eta_tide_north_2 
      U_tide_north_1 = U_tide_north_2 
      V_tide_north_1 = V_tide_north_2

      DO WHILE (Time_tide_north_2.LT.TIME+DT)
        READ(Ifile,*,END=120)  Time_tide_north_2, Eta_tide_north_2, U_tide_north_2, V_tide_north_2
      END DO

      CALL LWRITE('Time, Eta, U, V = '//&
                   FORMAT_FLOAT(Time_tide_north_2)//", "//&
                   FORMAT_FLOAT(Eta_tide_north_2)//", "//&
                   FORMAT_FLOAT(U_tide_north_2)//", "//&
                   FORMAT_FLOAT(V_tide_north_2))

    ENDIF ! end read time

! interpolate
    tmp2=ZERO
    tmp1=ZERO

    IF (TIME.GT.Time_tide_north_1) THEN
      IF(Time_tide_north_1.EQ.Time_tide_north_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
        tmp2=(Time_tide_north_2-TIME) &
            /MAX(SMALL, ABS(Time_tide_north_2-Time_tide_north_1))
        tmp1=1.0_SP - tmp2
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    TideNORTH_U = U_tide_north_2*tmp1 +U_tide_north_1*tmp2
    TideNORTH_V = V_tide_north_2*tmp1 +V_tide_north_1*tmp2
    TideNORTH_ETA = ETA_tide_north_2*tmp1 +ETA_tide_north_1*tmp2

  ENDIF

120 CONTINUE  ! no more data 

END SUBROUTINE TIDE_DATA

 
END MODULE TIDE_MODULE



    



