!------------------------------------------------------------------------------------
!
!      FILE vessle.F
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
!  VESSEL is a module to model ship-wakes    
!
!  HISTORY :
!    10/07/2016  Fengyan Shi
!
!-------------------------------------------------------------------------------------

# if defined (VESSEL)

MODULE VESSEL_MODULE
  USE PARAM
  USE GLOBAL,ONLY : Mloc,Nloc,Nghost,Ibeg,Iend,Jbeg,Jend,DX,DY, &
                    H,ETA,ETA0,Xco,Yco

  USE INPUT_READ
#if defined (PARALLEL)
  USE GLOBAL,ONLY : myid,ier, npx,npy,PX,PY
  USE MPI
# endif
  IMPLICIT NONE
  SAVE

    INTEGER :: NumVessel,Kves,II,JJ
    INTEGER,DIMENSION(:),ALLOCATABLE :: VesselType
    CHARACTER(LEN=80),DIMENSION(:),ALLOCATABLE:: SourceType
    REAL(SP),DIMENSION(:),ALLOCATABLE :: Xvessel1,Yvessel1,Xvessel2,Yvessel2, &
                                       LengthVessel,WidthVessel, &
                                       AlphaVessel1,AlphaVessel2,BetaVessel,Pvessel, &
                                       TimeVessel1,TimeVessel2,ThetaVessel
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VesselPressureTotal,VesselPressureEach, &
                                       VesselPressureX,VesselPressureY
    LOGICAL :: OUT_VESSEL = .TRUE.
    REAL(SP),DIMENSION(:),ALLOCATABLE :: ResistanceX,ResistanceY,ResPosX,ResNegX, &
                                         ResPosY,ResNegY
    REAL(SP):: PLOT_INTV_VESSEL,PLOT_COUNT_VESSEL

    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VesselFluxGradient,VesselFluxGradientEach, &
                                           Gz
    REAL(SP),DIMENSION(:),ALLOCATABLE :: Uvel,Vvel

    REAL(SP) :: Xves,Yves,Lves,Wves,P_x,P_y,DetaX,DetaY

!   deep draft vessel

# if defined (DEEP_DRAFT_VESSEL)
    LOGICAL :: MaskMethod = .TRUE.
    LOGICAL :: FrictionMethod = .TRUE.
    LOGICAL :: ViscosityMethod = .FALSE.    
    REAL(SP) :: CdDeepDraft
    REAL(SP) :: VisDeepDraft
    REAL(SP) :: Clearance
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: CdVessel_2D,VisVessel_2D
     INTEGER,DIMENSION(:,:),ALLOCATABLE :: MaskVessel
# endif
  
    LOGICAL :: MakeVesselDraft = .TRUE.

    INTEGER :: vunit 


!INTERFACE READ_FOUR_TYPE_VALUES
!  Module Procedure VESSEL_INITIAL
!  Module Procedure VESSEL_FORCING
!END INTERFACE

CONTAINS
  
! READ VESSEL

SUBROUTINE VESSEL_INITIAL
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL, INPUT_FILE_NAME
# if defined (PARALLEL)
  USE GLOBAL,ONLY : iista,jjsta   !ykchoi Jan/23/2018
# endif
  USE LOG_IO                  
  USE INPUT_READ
  USE MOD_FUNITS, ONLY: fhandle 
  IMPLICIT NONE
  CHARACTER(LEN=80)::FILE_NAME=' '
  CHARACTER(LEN=80)::FILE_VESSEL=' '
  CHARACTER(LEN=80) :: VESSEL_FOLDER=' '
  CHARACTER(LEN=80)::TMP_NAME=' '
  CHARACTER(LEN=80)::VesselName = ' '
  INTEGER :: Ifile,ierr
  TYPE(LOG_BUFFER), TARGET :: logbuff

! read vessel number and folder from input.txt
  FILE_NAME=INPUT_FILE_NAME
  CALL SET_INPUT_LOGGER(FILE_NAME, logbuff)

  CALL BWRITE('VESSEL INFO')

  CALL READ_PARAMETER(VESSEL_FOLDER, 'VESSEL_FOLDER', './')
  CALL READ_PARAMETER(NumVessel    , 'NumVessel'    , 0   )

  CALL READ_PARAMETER(PLOT_INTV_VESSEL, 'PLOT_INTV_VESSEL', SMALL)

  IF (NumVessel.GT.0 ) THEN
    CALL READ_PARAMETER(OUT_VESSEL, 'OUT_VESSEL', .TRUE.)
  ENDIF

  CALL logbuff % FLUSH2OUTPUT

! deep draft vessel
# if defined (DEEP_DRAFT_VESSEL)
  ! ---------

  CALL BWRITE("DEEP DRAFT INFO")

  CALL READ_PARAMETER(MaskMethod, 'MaskMethod', .TRUE.      )
  CALL READ_PARAMETER(CLEARANCE , 'CLEARANCE' , 1.0    , 'm')
      
  CALL READ_PARAMETER(FrictionMethod, 'FrictionMethod', .TRUE.)      
  IF (FrictionMethod) CALL READ_PARAMETER(CdDeepDraft, 'CdDeepDraft', 0.1_SP) 

  CALL READ_PARAMETER(ViscosityMethod, 'ViscosityMethod', .FALSE.)      
  IF (ViscosityMethod) CALL READ_PARAMETER(VisDeepDraft, 'VisDeepDraft',  0.1_SP)     

  CALL logbuff % FLUSH2OUTPUT
  ! -----------
# endif
! end deep draft vessel


      ALLOCATE (Xvessel1(NumVessel),Yvessel1(NumVessel),  &
                Xvessel2(NumVessel),Yvessel2(NumVessel),  &
                TimeVessel1(NumVessel),TimeVessel2(NumVessel), &
                LengthVessel(NumVessel),WidthVessel(NumVessel), &
                AlphaVessel1(NumVessel),AlphaVessel2(NumVessel), &
                BetaVessel(NumVessel),  &
                Pvessel(NumVessel),ThetaVessel(NumVessel),&
                ResistanceX(NumVessel),ResistanceY(NumVessel), &
                ResPosX(NumVessel),ResNegX(NumVessel), &
                ResPosY(NumVessel),ResNegY(NumVessel) )


      ALLOCATE (VesselFluxGradient(Mloc,Nloc),VesselFluxGradientEach(Mloc,Nloc), &
                Gz(Mloc,Nloc))
      ALLOCATE (Uvel(NumVessel),Vvel(NumVessel))
      ALLOCATE (SourceType(NumVessel),VesselType(NumVessel))
      
      Gz = ZERO
      Uvel = ZERO
      Vvel = ZERO

      ALLOCATE (VesselPressureTotal(Mloc,Nloc), VesselPressureEach(Mloc,Nloc),&
                VesselPressureX(Mloc,Nloc), &
                 VesselPressureY(Mloc,Nloc) )

# if defined (DEEP_DRAFT_VESSEL)
     ALLOCATE (CdVessel_2D(Mloc,Nloc),VisVessel_2D(Mloc,Nloc),MaskVessel(Mloc,Nloc))
# endif

  IF (NumVessel.GT.0) vunit = fhandle % ADD_RANGE('vessels', NumVessel+1)

  DO Kves = 1, NumVessel

!  file name
    WRITE(FILE_VESSEL(1:5), '(I0.5)') Kves
    TMP_NAME = TRIM(VESSEL_FOLDER)//'vessel_'//TRIM(FILE_VESSEL)

! check existing

    INQUIRE(FILE=TRIM(TMP_NAME),EXIST=FILE_EXIST)

    IF (.NOT.FILE_EXIST) THEN
      CALL SWRITE("VESSEL FILE 'vessel_"//TRIM(FILE_VESSEL)//"' EXPECTED IN FOLDER'"//\
                  TRIM(VESSEL_FOLDER)//"', BUT CANNOT BE FOUND.")
    ENDIF

! open file
    Ifile=Kves+vunit
    OPEN(Ifile,FILE=TRIM(TMP_NAME))

! read file
    READ(Ifile,'(A80)')  VesselName 
    READ(Ifile,*)  SourceType(Kves), VesselType(Kves)

    READ(Ifile,*)  ! length, width, etc
    IF (SourceType(Kves)(1:2)=='PR') THEN
      READ(Ifile,*) LengthVessel(Kves), WidthVessel(Kves) , &
                    AlphaVessel1(Kves), AlphaVessel2(Kves), &
                    BetaVessel(Kves)  , Pvessel(Kves)
!                for type 2 divid and volker 2017, alpha1 is cl,alpha2 is cb
!                beta is a

!   I remove the following protection, but will figure out how to avoid 
!   some crazy parameters
!          IF(VesselType(Kves)==2)THEN
!            IF(AlphaVessel1(Kves)<16.0_SP)AlphaVessel1(Kves)=16.0_SP
!            IF(AlphaVessel2(Kves)<2.0_SP)AlphaVessel2(Kves)=2.0_SP
!            IF(BetaVessel(Kves)<16.0_SP)BetaVessel(Kves) = 16.0_SP
!          ENDIF

      IF(VesselType(Kves)==1)THEN
        AlphaVessel1(Kves) = Max(SMALL, AlphaVessel1(Kves))
        BetaVessel(Kves) = Max(SMALL, BetaVessel(Kves))
        AlphaVessel1(Kves) = Min(1.0_SP, AlphaVessel1(Kves))
        BetaVessel(Kves) = Min(1.0_SP, BetaVessel(Kves))
      ENDIF


      IF ((VesselType(Kves).NE.1).AND.\
          (VesselType(Kves).NE.2)) THEN

        CALL SWRITE("UKNOWN VESSEL TYPE '"//TRIM(FORMAT_INTEGER(VesselType(Kves)))//\
                    "' FOR SOURCE TYPE '"//TRIM(SourceType(Kves))//"' IN '"//\
                    "vessel_"//TRIM(FILE_VESSEL)//"'.")
      ENDIF 

    ELSEIF (SourceType(Kves)(1:2)=='SL') THEN
      READ(Ifile,*) LengthVessel(Kves), WidthVessel(Kves), &
                    AlphaVessel1(Kves), AlphaVessel2(Kves), &
                    BetaVessel(Kves)  , Pvessel(Kves)

      IF ((VesselType(Kves).NE.1).AND.\
          (VesselType(Kves).NE.2)) THEN

        CALL SWRITE("UKNOWN VESSEL TYPE '"//TRIM(FORMAT_INTEGER(VesselType(Kves)))//\
                    "' FOR SOURCE TYPE '"//TRIM(SourceType(Kves))//"' IN '"//\
                    "vessel_"//TRIM(FILE_VESSEL)//"'.")
      ENDIF 

    ELSE
      CALL SWRITE("UKNOWN VESSEL SOURCE TYPE '"//TRIM(SourceType(Kves))//"' IN '"//\
                  "vessel_"//TRIM(FILE_VESSEL)//"'.")
    ENDIF


    READ(Ifile,*)  ! t, x, y
    READ(Ifile,*)  TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)

    TimeVessel1(Kves) = TimeVessel2(Kves)
    Xvessel1(Kves) = Xvessel2(Kves)
    Yvessel1(Kves) = Yvessel2(Kves)

    CALL BWRITE("VESSEL "//TRIM(FILE_VESSEL))

    CALL logbuff % ADD("Name"       , TRIM(VesselName)                )
    CALL logbuff % ADD("Source Type", TRIM(SourceType(Kves))          )
    CALL logbuff % ADD("Type"       , FORMAT_INTEGER(VesselType(Kves)))
    CALL logbuff % ADD("Length"     , FORMAT_FLOAT(LengthVessel(Kves)))
    CALL logbuff % ADD("Width"      , FORMAT_FLOAT(WidthVessel(Kves)) )

    IF (SourceType(Kves)(1:2).EQ."PR") THEN
      IF (VesselType(Kves).EQ.1) THEN
        CALL logbuff % ADD("Alpha 1", FORMAT_FLOAT(AlphaVessel1(Kves)))
        CALL logbuff % ADD("Alpha 2", FORMAT_FLOAT(AlphaVessel2(Kves)))
        CALL logbuff % ADD("Beta"   , FORMAT_FLOAT(BetaVessel(Kves))  )
      ELSEIF (VesselType(Kves).EQ.2) THEN
        CALL logbuff % ADD("a", FORMAT_FLOAT(AlphaVessel1(Kves)))
        CALL logbuff % ADD("b", FORMAT_FLOAT(AlphaVessel2(Kves)))
        CALL logbuff % ADD("c", FORMAT_FLOAT(BetaVessel(Kves))  )
      ENDIF
    ELSEIF (SourceType(Kves)(1:2).EQ."SL") THEN
      IF (VesselType(Kves).EQ.1) THEN
        ! No additional parameters to read
      ELSEIF (VesselType(Kves).EQ.2) THEN
        CALL logbuff % ADD("Alpha 1", FORMAT_FLOAT(AlphaVessel1(Kves)))
        CALL logbuff % ADD("Alpha 2", FORMAT_FLOAT(AlphaVessel2(Kves)))
      ENDIF
    ENDIF 

    CALL logbuff % ADD("P"           , FORMAT_FLOAT(PVessel(Kves))    )
    CALL logbuff % ADD("Initial Time", FORMAT_FLOAT(TimeVessel2(Kves)))
    CALL logbuff % ADD("Initial X"   , FORMAT_FLOAT(XVessel2(Kves))   )
    CALL logbuff % ADD("Initial Y"   , FORMAT_FLOAT(YVessel2(Kves))   )

    CALL logbuff % FLUSH2OUTPUT

  ENDDO  ! end Kves

  CALL CLEAR_INPUT_LOGGER

ENDSUBROUTINE VESSEL_INITIAL

SUBROUTINE VESSEL_FORCING
  USE GLOBAL,ONLY : Mloc,Nloc,tmp1,tmp2,SMALL,TIME,ZERO,DT,DEPTH
  USE LOG_IO, ONLY : LWRITE
  USE INPUT_READ, ONLY: FORMAT_INTEGER, FORMAT_FLOAT
  IMPLICIT NONE

  INTEGER :: Ifile,ierr,I,J

  VesselFluxGradient = ZERO
  VesselPressureTotal = ZERO

  DO Kves = 1,NumVessel

    IF(TIME>TimeVessel1(Kves).AND.TIME>TimeVessel2(Kves)) THEN

      TimeVessel1(Kves)=TimeVessel2(Kves)
      Xvessel1(Kves) = Xvessel2(Kves)
      Yvessel1(Kves) = Yvessel2(Kves)

      Ifile = vunit + Kves

! I add while to avoid dt is smaller than the time interval of vessel path 08/05/2019
      DO WHILE (TimeVessel2(Kves).LT.TIME+DT)
        READ(Ifile,*,END=120)  TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)
      END DO

      CALL LWRITE("Read Vessel # "//TRIM(FORMAT_INTEGER(Kves)))
      CALL LWRITE("T, X, T = "//\
                  TRIM(FORMAT_FLOAT(TimeVessel2(Kves)))//", "//\
                  TRIM(FORMAT_FLOAT(Xvessel2(Kves)))//", "//\
                  TRIM(FORMAT_FLOAT(Yvessel2(Kves))))

      ThetaVessel(Kves) = ATAN2(Yvessel2(Kves)-Yvessel1(Kves),  &
                                Xvessel2(Kves)-Xvessel1(Kves))


! for panel source
      IF((TimeVessel2(Kves)-TimeVessel1(Kves))> ZERO)THEN
        Uvel(Kves) = (Xvessel2(Kves)-Xvessel1(Kves))/(TimeVessel2(Kves)-TimeVessel1(Kves))
        Vvel(Kves) = (Yvessel2(Kves)-Yvessel1(Kves))/(TimeVessel2(Kves)-TimeVessel1(Kves))
      ENDIF
! end panel source

    ENDIF ! end time > timevessel2

! calculate force
    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TimeVessel1(Kves))THEN
      IF(TimeVessel1(Kves).EQ.TimeVessel2(Kves))THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TimeVessel2(Kves)-TIME) &
            /MAX(SMALL, ABS(TimeVessel2(Kves)-TimeVessel1(Kves)))
      tmp1=1.0_SP - tmp2
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    Xves = Xvessel2(Kves)*tmp1 +Xvessel1(Kves)*tmp2
    Yves = Yvessel2(Kves)*tmp1 +Yvessel1(Kves)*tmp2


   IF(SourceType(Kves)(1:2)=='PR') THEN
   ! pressure source

     CALL PRESSURE_SOURCE (Xves,Yves)
    VesselPressureTotal = VesselPressureTotal+VesselPressureEach

   ENDIF

   IF(SourceType(Kves)(1:2)=='SL') THEN
   
     CALL SLENDER_BODY_SOURCE (Xves,Yves)
     VesselFluxGradient = VesselFluxGradient + VesselFluxGradientEach

   ENDIF

   IF(SourceType(Kves)(1:2)=='PA') THEN
   
     CALL GREEN_FUNCTION_SOURCE (Xves,Yves)
     VesselFluxGradient = VesselFluxGradient + VesselFluxGradientEach

   ENDIF   


120 CONTINUE  ! no more data for vessel Kves


  ENDDO  ! end Kves

! sourceX and sourceY


!   IF(SourceType(Kves)(1:2)=='PR') THEN   

# if defined (DEEP_DRAFT_VESSEL)
    CdVessel_2D = ZERO
    VisVessel_2D = ZERO
    MaskVessel = 1
# endif

    DO J=Jbeg,Jend
    DO I=Ibeg,Iend

!   I modified the term to negative 11/22/2016 fyshi

       VesselPressureX(I,J) = -Grav*H(I,J)*  &
               (VesselPressureTotal(I+1,J)-VesselPressureTotal(I-1,J))/2.0_SP  &
               /DX
       VesselPressureY(I,J) = -Grav*H(I,J)*  &
               (VesselPressureTotal(I,J+1)-VesselPressureTotal(I,J-1))/2.0_SP  &
               /DY
# if defined (DEEP_DRAFT_VESSEL)
! use mask9 for SWE, in the vessel module mask9 uses 25 points rather than 9 points
! for breakers 
       IF(VesselPressureTotal(I,J).GT.ZERO) THEN
        IF(Depth(I,J)-VesselPressureTotal(I,J) < CLEARANCE) THEN
         DO II=I-Nghost,I+Nghost
         DO JJ=J-Nghost,J+Nghost
          IF(MaskMethod) THEN
            MaskVessel(II,JJ) = 0
          ENDIF
          IF(FrictionMethod) THEN
            CdVessel_2D(II,JJ)=CdDeepDraft
          ENDIF
          IF(ViscosityMethod) THEN
            VisVessel_2D(II,JJ) = VisDeepDraft
          ENDIF
         ENDDO
         ENDDO
        ENDIF ! clearance
       ENDIF
# endif

    ENDDO
    ENDDO

!  make initial draft 09/12/2018
    IF(MakeVesselDraft)THEN
       MakeVesselDraft = .FALSE.
        DO J=1,Nloc
        DO I=1,Mloc
          IF(ABS(VesselPressureTotal(I,J)).GT.SMALL)THEN      
            Eta(I,J) = - VesselPressureTotal(I,J)
            ETA0(I,J) = ETA(I,J)
          ENDIF
        ENDDO
        ENDDO
    ENDIF
!   ENDIF  ! end pressure source type 1 and 2

END SUBROUTINE VESSEL_FORCING

SUBROUTINE PRESSURE_SOURCE (Xves,Yves)
  USE GLOBAL,ONLY : Mloc,Nloc,tmp1,tmp2,SMALL,TIME,ZERO, H,U,V,  &
                    DX,DY,Ibeg,Iend,Jbeg,Jend,DEPTH
  IMPLICIT NONE
  INTEGER :: ierr,I,J
  REAL(SP) :: Xves,Yves,Lves,Wves
#if defined (PARALLEL)
    REAL(SP) :: myvar
# endif  

! rectangular
    VesselPressureEach = ZERO
    ResistanceX(Kves)=ZERO
    ResistanceY(Kves)=ZERO
    ResPosX(Kves) = ZERO
    ResNegX(Kves) = ZERO
    ResPosY(Kves) = ZERO
    ResNegY(Kves) = ZERO

    DO J=1,Nloc
    DO I=1,Mloc
      Lves=(Xco(I)-Xves)*COS(ThetaVessel(Kves)) + (Yco(J)-Yves)*SIN(ThetaVessel(Kves))
      Wves=-(Xco(I)-Xves)*SIN(ThetaVessel(Kves)) + (Yco(J)-Yves)*COS(ThetaVessel(Kves))

   ! Ertekin et al. JFM 1986

    IF(ABS(Lves)<=0.5_SP*LengthVessel(Kves).AND. &
         ABS(Wves)<=0.5_SP*WidthVessel(Kves)) THEN


     IF(VesselType(Kves) == 1) THEN
      P_x = ZERO
      P_y = ZERO

      IF(Lves>0.5_SP*LengthVessel(Kves)*AlphaVessel1(Kves).AND. &
         Lves<0.5_SP*LengthVessel(Kves))THEN
          
          P_x = COS(PI*(Lves-0.5_SP*AlphaVessel1(Kves)*LengthVessel(Kves))  &
                   /((1.0_SP-AlphaVessel1(Kves))*LengthVessel(Kves)))**2
      ELSEIF(Lves<-0.5_SP*LengthVessel(Kves)*AlphaVessel2(Kves).AND. &
         Lves>-0.5_SP*LengthVessel(Kves))THEN
          P_x = COS(PI*(ABS(Lves)-0.5_SP*AlphaVessel2(Kves)*LengthVessel(Kves))  &
                   /((1.0_SP-AlphaVessel2(Kves))*LengthVessel(Kves)))**2

      ELSEIF(Lves<=0.5_SP*LengthVessel(Kves)*AlphaVessel1(Kves).AND. &
         Lves>=-0.5_SP*LengthVessel(Kves)*AlphaVessel2(Kves))THEN
          P_x = 1.0_SP
      ENDIF

      IF(ABS(Wves)>0.5_SP*WidthVessel(Kves)*BetaVessel(Kves).AND. &
         ABS(Wves)<0.5_SP*WidthVessel(Kves)) THEN
          P_y = COS(PI*(ABS(Wves)-0.5_SP*BetaVessel(Kves)*WidthVessel(Kves))  &
                  /((1.0_SP-BetaVessel(Kves))*WidthVessel(Kves)))**2
      ELSEIF(ABS(Wves)<=0.5_SP*WidthVessel(Kves)*BetaVessel(Kves)) THEN
          P_y = 1.0_SP
      ENDIF

         VesselPressureEach(I,J) = Pvessel(Kves)*P_x*P_y
     ENDIF  ! type 1

     IF(VesselType(Kves) == 2) THEN

        VesselPressureEach(I,J) = Pvessel(Kves)*  &
              (1.0_SP - AlphaVessel1(Kves)*(Lves/LengthVessel(Kves))**4)* &
              (1.0_SP - AlphaVessel2(Kves)*(Wves/WidthVessel(Kves))**2)* &
              EXP(-BetaVessel(Kves)*(Wves/WidthVessel(Kves))**2)

     ENDIF ! end type 2     

    ENDIF  ! end inside ship rectangule
     
    ENDDO
    ENDDO  ! end grid

! calculate resistance. In the initial code, the resistance was calculated
! inside the last loop. It turns out that resistance was re-counted 
! because of including ghost cells (thanks to Jeff Harris)    

    DO J=Jbeg,Jend
    DO I=Ibeg,Iend

         DetaX = (ETA(I+1,J)-ETA(I-1,J))/2.0_SP/DX
         DetaY = (ETA(I,J+1)-ETA(I,J-1))/2.0_SP/DY

         IF(DetaX>=0)THEN
           ResPosX(Kves) = ResPosX(Kves) &
                  +VesselPressureEach(I,J)*RHO_WATER*GRAV  &
                  *DetaX*DY
         ELSE
           ResNegX(Kves) = ResNegX(Kves) &
                  +VesselPressureEach(I,J)*RHO_WATER*GRAV  &
                  *DetaX*DY           
         ENDIF

         IF(DetaY>=0)THEN
           ResPosY(Kves) = ResPosY(Kves) &
                  +VesselPressureEach(I,J)*RHO_WATER*GRAV  &
                  *DetaY*DX
         ELSE
           ResNegY(Kves) = ResNegY(Kves) &
                  +VesselPressureEach(I,J)*RHO_WATER*GRAV  &
                  *DetaY*DX          
         ENDIF

         ResistanceX(Kves) = ResPosX(Kves) + ResNegX(Kves)
         ResistanceY(Kves) = ResPosY(Kves) + ResNegY(Kves)
      
    ENDDO
    ENDDO

  ! pressure source

# if defined (PARALLEL)
     call MPI_ALLREDUCE(ResistanceX(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResistanceX(Kves) = myvar
     call MPI_ALLREDUCE(ResistanceY(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResistanceY(Kves) = myvar
     call MPI_ALLREDUCE(ResPosX(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResPosX(Kves) = myvar
     call MPI_ALLREDUCE(ResPosY(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResPosY(Kves) = myvar
     call MPI_ALLREDUCE(ResNegX(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResNegX(Kves) = myvar
     call MPI_ALLREDUCE(ResNegY(Kves),myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResNegY(Kves) = myvar
# endif


END SUBROUTINE PRESSURE_SOURCE

SUBROUTINE GREEN_FUNCTION_SOURCE (Xves,Yves)
  USE GLOBAL,ONLY : Mloc,Nloc,tmp1,tmp2,SMALL,TIME,ZERO, H,U,V,  &
                    DX,DY,Ibeg,Iend,Jbeg,Jend,DEPTH
  IMPLICIT NONE
  INTEGER :: ierr,I,J
  REAL(SP) :: Xves,Yves,Lves,Wves


  VesselFluxGradientEach = ZERO

# if defined (REALISTIC_VESSEL_BODY)
  REAL, DIMENSION(Mloc,Nloc) :: W_farfield,Z_prime
                                ! Z_prime defined 0 at surface and positive below
  INTEGER :: m_trunc, km
  REAL :: r3,r3_2m,r3_4m

  m_trunc = 5
  Gz = ZERO
     
  DO J=Jbeg,Jend
  DO I=Ibeg,Iend 

      Lves=(Xco(I)-Xves)*COS(ThetaVessel(Kves)) + (Yco(J)-Yves)*SIN(ThetaVessel(Kves))
      Wves=-(Xco(I)-Xves)*SIN(ThetaVessel(Kves)) + (Yco(J)-Yves)*COS(ThetaVessel(Kves))

!     the 3D nearfield is solved inside -L to L and -W to W
!     note that this length doubles the vessel length
!     the far field is outside this rectangule 
      
      IF(ABS(Lves)>=LengthVessel(Kves).AND. &
         ABS(Wves)>=WidthVessel(Kves)) THEN

        Z_prime(I,J) = Pvessel(Kves)
        W_farfield = - U(I,J)*(Depth(I+1,J)-Depth(I-1,J))/DX*0.5_SP &
                     - V(I,J)*(Depth(I,J+1)-Depth(I,J-1))/DY*0.5_SP &
                     - (Depth(I,J)-Z_prime(I,J)) &
                     * (U(I+1,J)-U(I-1,J))/DX*0.5_SP &
                      + V(I,J+1)-V(I,J-1))/DY*0.5_SP)

          r3=((Xco(I,J)-Xves)**2+(Yco(I,J)-Yves)**2+Z_prime**2)**(3.0_SP/2.0_SP)
          Gz(I,J) = -2.0_SP*Z_prime(I,J)/MAX(SMALL,r3)
                               ! actually r3 should larger than zero
        DO km = 1, m_trunc

          r3_2m=((Xco(I,J)-Xves)**2+(Yco(I,J)-Yves)**2  &
                 +(Z_prime+2.0_SP*km*H(I,J))**2)**(3.0_SP/2.0_SP)
          r3_4m=((Xco(I,J)-Xves)**2+(Yco(I,J)-Yves)**2  &
                 +(Z_prime-2.0_SP*km*H(I,J))**2)**(3.0_SP/2.0_SP)
          Gz(I,J) = Gz(I,J) -2.0_SP * (-1.0_SP)**km &
                    * ( (Z_prime(I,J) + 2.0_SP*km*Depth(I,J))/r3_2m &
                       +(Z_prime(I,J) + 2.0_SP*km*Depth(I,J))/r3_4m )
        ENDDO

         VesselFluxGradientEach(I,J) = -1.0_SP /2.0_SP /PI *( (Uvel(Kves)-U(I,J))/DX &
                               (Uvel(Kves)-U(I,J))/DY -W_farfield/Depth(I,J) )
                               
     ENDIF  ! end inside vessel body

  ENDDO
  ENDDO

# else
  

! assume single value of submerged vessel body
! we only record Gz at z_prime

    DO J=1,Nloc
    DO I=1,Mloc
      Lves=(Xco(I)-Xves)*COS(ThetaVessel(Kves)) + (Yco(J)-Yves)*SIN(ThetaVessel(Kves))
      Wves=-(Xco(I)-Xves)*SIN(ThetaVessel(Kves)) + (Yco(J)-Yves)*COS(ThetaVessel(Kves))

      
      IF(ABS(Lves)<=0.5_SP*LengthVessel(Kves).AND. &
         ABS(Wves)<=0.5_SP*WidthVessel(Kves)) THEN
         VesselFluxGradientEach(I,J) = Pvessel(Kves)  &
                  *SIN(2.0*PI*Lves/(LengthVessel(Kves))) &
                  *COS(PI*Wves/(WidthVessel(Kves)))**2   
!   print*,i,j,VesselFluxGradientEach(I,J), SIN(2.0*PI*Lves/(LengthVessel(Kves))), &
!          COS(PI*Wves/(WidthVessel(Kves)))**2
      ENDIF
    ENDDO
    ENDDO

# endif
  !  end realistic vessel body, otherwise slender

END SUBROUTINE GREEN_FUNCTION_SOURCE


SUBROUTINE SLENDER_BODY_SOURCE (Xves,Yves)
  USE GLOBAL,ONLY : Mloc,Nloc,tmp1,tmp2,SMALL,TIME,ZERO, H,U,V,  &
                    DX,DY,Ibeg,Iend,Jbeg,Jend,DEPTH
  IMPLICIT NONE
  INTEGER :: ierr,I,J
  REAL(SP) :: Xves,Yves,Lves,Wves
  REAL(SP) :: x_right_1, x_right_2, x_left_1, x_left_2


  VesselFluxGradientEach = ZERO

! initial parabolic function by Tanimoto et al. 2000
  
    DO J=1,Nloc
    DO I=1,Mloc
      Lves=(Xco(I)-Xves)*COS(ThetaVessel(Kves)) + (Yco(J)-Yves)*SIN(ThetaVessel(Kves))
      Wves=-(Xco(I)-Xves)*SIN(ThetaVessel(Kves)) + (Yco(J)-Yves)*COS(ThetaVessel(Kves))

      
      IF(ABS(Lves)<=0.5_SP*LengthVessel(Kves).AND. &
         ABS(Wves)<=0.5_SP*WidthVessel(Kves)) THEN

        IF(VesselType(kves)==1)THEN
         VesselFluxGradientEach(I,J) = Pvessel(Kves)  &
                  *SIN(2.0*PI*Lves/(LengthVessel(Kves))) &
                  *COS(PI*Wves/(WidthVessel(Kves)))**2   
        ENDIF  ! type 1

        IF(VesselType(kves)==2)THEN
         
         x_right_1=0.5_SP*LengthVessel(Kves)*AlphaVessel1(Kves)
         x_right_2=0.5_SP*LengthVessel(Kves)
         x_left_1=-0.5_SP*LengthVessel(Kves)*AlphaVessel2(Kves)
         x_left_2=-0.5_SP*LengthVessel(Kves)

         IF(Lves>x_right_1.AND. &
            Lves<x_right_2)THEN
          
          VesselFluxGradientEach(I,J) = Pvessel(Kves)  &
                  *SIN(PI*(Lves-x_right_1)/(x_right_2-x_right_1)) &
                  *COS(PI*Wves/(WidthVessel(Kves)))**2 

         ELSEIF(Lves<x_left_1.AND. &
            Lves>x_left_2)THEN
          VesselFluxGradientEach(I,J) = Pvessel(Kves)  &
                  *SIN(PI*(Lves-x_left_1)/(x_left_1-x_left_2)) &
                  *COS(PI*Wves/(WidthVessel(Kves)))**2 

         ELSEIF(Lves<=x_right_1.AND. &
            Lves>=x_left_1)THEN
          VesselFluxGradientEach(I,J) = ZERO
         ENDIF

        ENDIF  ! type 2

      ENDIF
    ENDDO
    ENDDO


!open(99,file='tmp.txt')
!do j=1,Nloc
! write(99,108)(VesselFluxGradientEach(i,j),i=1,Mloc)
!enddo
!close(99)
!108 format(5000E12.3)
!stop

END SUBROUTINE SLENDER_BODY_SOURCE

END MODULE VESSEL_MODULE

# endif 
! end vessel
