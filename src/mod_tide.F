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
#if defined (PARALLEL)
  USE GLOBAL,ONLY : myid,ier, npx,npy,PX,PY
  USE MPI
# endif
  IMPLICIT NONE
  SAVE

  CHARACTER(LEN=80) TideBcType
  LOGICAL :: TIDAL_BC=.FALSE.
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
  INTEGER :: Iwidth
  

CONTAINS
  
SUBROUTINE TIDE_INITIAL
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL,INPUT_FILE_NAME, SPONGE_ON
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
                    
  USE INPUT_READ !, ONLY: READ_VALUE_CHECK
  USE LOG_OUTPUT
  IMPLICIT NONE

  CHARACTER(LEN=80)::FILE_NAME=' '
  CHARACTER(LEN=80)::TMP_NAME=' '
  INTEGER :: Ifile,ierr
  LOGICAL :: IsNotRead

! read  from input.txt
      FILE_NAME=INPUT_FILE_NAME

      CALL WRITE_LOG( LOG_BANNER( 'TIDE INFO' ) )
      CALL READ_VALUE_CHECK( TIDAL_BC, FILE_NAME, 'TIDAL_BC', DefaultValue=.FALSE. )

      IF ( TIDAL_BC ) THEN

         ALLOCATE(SPONGE_TIDE_WEST(Mloc,Nloc) , SPONGE_TIDE_EAST(Mloc,Nloc) , &
                  SPONGE_TIDE_SOUTH(Mloc,Nloc), SPONGE_TIDE_NORTH(Mloc,Nloc)    )

         CALL TIDE_SPONGE

! input tide details
         CALL READ_VALUE_CHECK( TideBcType, FILE_NAME, 'TideBcType', DefaultValue='CONSTANT' )

         IF( TideBcType(1:4) == 'CONS' ) THEN
!  west ---                 
            CALL READ_VALUE_CHECK_DELAY_DEFAULT( TideWest_ETA, FILE_NAME, 'TideWest_ETA', IsDefault=IsNotRead )
            TideWest = .NOT.IsNotRead 

            IF ( TideWest ) THEN
               CALL READ_VALUE_CHECK( TideWest_U, FILE_NAME, 'TideWest_U' , DefaultValue=0.0_SP )
               CALL READ_VALUE_CHECK( TideWest_V, FILE_NAME, 'TideWest_V' , DefaultValue=0.0_SP )
            ELSE
               CALL WRITE_ALL ( 'WARNING: TideWest_ETA not found in input file, Defaulting TideWest to F.' )
            END IF

!  east ---
            CALL READ_VALUE_CHECK_DELAY_DEFAULT( TideEast_ETA, FILE_NAME, 'TideEast_ETA', IsDefault=IsNotRead )
            TideEast = .NOT.IsNotRead

            IF ( TideEast ) THEN
               CALL READ_VALUE_CHECK( TideEast_U, FILE_NAME, 'TideEast_U', DefaultValue=0.0_SP )
               CALL READ_VALUE_CHECK( TideEast_V, FILE_NAME, 'TideEast_V', DefaultValue=0.0_SP )
            ELSE
               CALL WRITE_ALL ( 'WARNING: TideEast_ETA not found in input file, Defaulting TideEast to F.' )
            END IF

!  south ---
            CALL READ_VALUE_CHECK_DELAY_DEFAULT( TideSouth_ETA, FILE_NAME, 'TideSouth_ETA', IsDefault=IsNotRead )
            TideSouth = .NOT.IsNotRead

            IF ( TideSouth ) THEN
               CALL READ_VALUE_CHECK( TideSouth_U, FILE_NAME, 'TideSouth_U', DefaultValue=0.0_SP )
               CALL READ_VALUE_CHECK( TideSouth_V, FILE_NAME, 'TideSouth_V', DefaultValue=0.0_SP )
            ELSE
               CALL WRITE_ALL ( 'WARNING: TideSouth_ETA not found in input file, Defaulting TideSouth to F.' )
            END IF

!  north ---
            CALL READ_VALUE_CHECK_DELAY_DEFAULT( TideNorth_ETA, FILE_NAME, 'TideNorth_ETA', IsDefault=IsNotRead )
            TideNorth = .NOT.IsNotRead

            IF ( TideNorth ) THEN
               CALL READ_VALUE_CHECK( TideNorth_U, FILE_NAME, 'TideNorth_U', DefaultValue=0.0_SP )
               CALL READ_VALUE_CHECK( TideNorth_V, FILE_NAME, 'TideNorth_V', DefaultValue=0.0_SP )
            ELSE
               CALL WRITE_ALL ( 'WARNING: TideNorth_ETA not found in input file, Defaulting TideNorth to F.' )
            END IF


            ! Checking if sponge layers have been set on any of the boundries 
            IF ( SPONGE_ON ) THEN

               IF( TideWest.AND.(Sponge_west_width > ErrTol) ) THEN
                  CALL WRITE_LOG( 'WARNING: Invalid options, TideWest BC set and Sponge_west_width '//&
                                  'is not zero. Setting Sponge_west_width=0.0.' )
                  Sponge_west_width = 0.0_SP 
               END IF

               IF( TideEast.AND.(Sponge_east_width > ErrTol) ) THEN
                  CALL WRITE_LOG( 'WARNING: Invalid options, TideEast BC set and Sponge_east_width '//&
                                  'is not zero. Setting Sponge_east_width=0.0.' )
                  Sponge_east_width = 0.0_SP
               END IF

               IF( TideSouth.AND.(Sponge_south_width > ErrTol) ) THEN
                  CALL WRITE_LOG( 'WARNING: Invalid options, TideSouth BC set and Sponge_south_width '//&
                                  'is not zero. Setting Sponge_south_width=0.0.' )
                  Sponge_south_width = 0.0_SP
               END IF

               IF( TideNorth.AND.(Sponge_north_width > ErrTol) ) THEN
                  CALL WRITE_LOG( 'WARNING: Invalid options, TideNorth BC set and Sponge_north_width '//&
                                  'is not zero. Setting Sponge_north_width=0.0.' )
                  Sponge_north_width = 0.0_SP
               END IF

            END IF
                     
            
            IF ( (.NOT.TideWest ).AND.&
                 (.NOT.TideEast ).AND.&
                 (.NOT.TideSouth).AND.& 
                 (.NOT.TideNorth)       ) THEN
                TIDAL_BC = .FALSE.
                CALL CLEAR_TMP_LOG
                CALL FORMAT_VALUE(TIDAL_BC, TMP_NAME )
                CALL WRITE_VARIABLE_VALUE( 'TIDAL_BC' , TMP_NAME )
                CALL WRITE_ALL( 'WARNING: TIDAL_BC set, but no BC values set. Setting TIDAL_BC to FALSE. ' )

            ENDIF ! end all false
       
         ENDIF ! end tide constant

      ENDIF ! end tide
    
      CALL FLUSH_TMP_LOG
      CALL CLOSE_TMP_LOG

END SUBROUTINE TIDE_INITIAL

SUBROUTINE TIDE_SPONGE
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   
# endif
                    
  IMPLICIT NONE
  REAL(SP) :: ri,lim,Lstart,Lend,R_sp,A_sp
  
  Iwidth = 30
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

END MODULE TIDE_MODULE



    



