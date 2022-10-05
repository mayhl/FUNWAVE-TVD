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
!
!  INPUT_READ is a module to handle reading input.txt     
!
!  HISTORY :
!    02/17/2016  Fengyan Shi
!
!-------------------------------------------------------------------------------------

MODULE INPUT_READ

USE PARAM,ONLY : SP
#if defined (PARALLEL)
  USE GLOBAL,ONLY : myid,ier
  USE MPI
# endif
  USE LOGGER
  IMPLICIT NONE
  LOGICAL :: FILE_EXIST
  CHARACTER(LEN=80) :: VAL_TYPE 
  CHARACTER(LEN=80) :: VAL_READ 

INTERFACE READ_FOUR_TYPE_VALUES
  Module Procedure READ_FLOAT
  Module Procedure READ_STRING
  Module Procedure READ_LOGICAL
  Module Procedure READ_INTEGER
END INTERFACE

INTERFACE READ_PARAMETER
   MODULE PROCEDURE READ_INTEGER2
   MODULE PROCEDURE READ_FLOAT2
   MODULE PROCEDURE READ_STRING2
   MODULE PROCEDURE READ_LOGICAL2
END INTERFACE

INTERFACE FORMAT_VAL
        MODULE PROCEDURE FORMAT_INTEGER 
END INTERFACE 
 
  CHARACTER(:), TARGET, ALLOCATABLE :: input_file_path   
  TYPE(LOG_BUFFER), POINTER :: logbuf 
   
CONTAINS

SUBROUTINE SET_INPUT_LOGGER(fpath, logbuff)

    IMPLICIT NONE 
    CHARACTER(*)            , INTENT(IN) :: fpath
    TYPE(LOG_BUFFER), TARGET, INTENT(IN) :: logbuff

    INTEGER :: path_len
    CALL CLEAR_INPUT_LOGGER()

    path_len=LEN_TRIM(ADJUSTL(fpath))

    ALLOCATE(CHARACTER(path_len) :: input_file_path)
    input_file_path = TRIM(ADJUSTL(fpath))

    logbuf => logbuff
    
END SUBROUTINE

SUBROUTINE CLEAR_INPUT_LOGGER()

    IMPLICIT NONE
    IF (ALLOCATED(input_file_path)) DEALLOCATE(input_file_path)
    IF (ASSOCIATED(logbuf)) logbuf => NULL()

END SUBROUTINE

PURE FUNCTION FORMAT_INTEGER(val) RESULT (str)

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: val
        CHARACTER(80) :: str
        WRITE(str, *) val

END FUNCTION

PURE FUNCTION FORMAT_FLOAT(val) RESULT(str)

        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: val
        CHARACTER(80) :: str
       
        CHARACTER(20) :: coeff 
        INTEGER :: expon
        INTEGER :: i, ns, ne

        str = ""
        IF(myid.NE.0) RETURN

        ! Using scientific notation for larger or small numbers 
        IF (ABS(val).GT.1.0E6_SP.OR.ABS(val).LT.1.0E-6_SP) THEN
           WRITE(str,'(E15.6)') val
           i = INDEX(str,"+")
           if (i.EQ.0) i = INDEX(str,'-')
           coeff = str(:i-2)
           READ(str(i:), *) expon
        ELSE
           WRITE(coeff,'(F15.6)') val
           expon = 0
        END IF 
        
        ! Removing trailing zeros
        coeff = ADJUSTL(coeff)
        ne = INDEX(coeff,'.') + 2
        ns = LEN_TRIM(coeff)
        DO i = ns,ne,-1
           IF (coeff(i:i).NE.'0') THEN
              coeff = coeff(:i-1)
              EXIT
           ELSEIF (i.EQ.ne) THEN
              coeff = coeff(:i-1)
           END IF
        END DO

        str = ADJUSTL(coeff)
        ! Attaching exponent 
        IF (expon.NE.0) THEN
           WRITE(coeff,*) expon
           str = TRIM(str)//'E'//TRIM(ADJUSTL(coeff))
        END IF

END FUNCTION

PURE FUNCTION FORMAT_LOGICAL(val) RESULT(str)

        IMPLICIT NONE
        LOGICAL, INTENT(IN) :: val 
        CHARACTER(80) :: str 
       
        IF (val) THEN
           str = 'T'
        ELSE
           str = 'F'
        END IF

END FUNCTION

SUBROUTINE READ_VALUE2(val_name, is_required, val_type, default_str, val_read, is_default, check)

   IMPLICIT NONE
   
   CHARACTER(*)     , INTENT(IN)    :: val_name
   LOGICAL          , INTENT(IN)    :: is_required
   CHARACTER(*)     , INTENT(IN)    :: val_type
   CHARACTER(*)     , INTENT(IN)    :: default_str
   CHARACTER(*)     , INTENT(OUT)   :: val_read
   LOGICAL          , INTENT(OUT)   :: is_default 
   LOGICAL, OPTIONAL, INTENT(INOUT) :: check

   CHARACTER(80) :: type_detected
   LOGICAL :: file_exists 
   INTEGER :: err
   CHARACTER(:), POINTER :: fname

   IF (.NOT.ASSOCIATED(logbuf)) THEN
      CALL WRITE_STOP('FATAL ERROR: LOGGER NOT SET. STOP!!!')
   END IF

   IF (.NOT.ALLOCATED(input_file_path)) THEN
      CALL WRITE_STOP( 'FATAL ERROR: INPUT FILE PATH NOT SET. STOP!!!')
   END IF 

   fname => input_file_path

   INQUIRE(FILE=fname,EXIST=file_exists)
   IF ( .NOT.file_exists ) CALL WRITE_STOP( 'FILE '//TRIM(fname)//' CAN NOT BE FOUND. STOP!!!' )

   CALL READ_VALUE(fname, val_name, val_read, err)

   IF ( err.NE.0 ) THEN 
      IF ( is_required ) THEN
         IF (PRESENT(check)) THEN
            ! Bypassing require value checking to call location 
            IF (check) THEN
               check =.FALSE.
               RETURN
            ELSE
               CALL WRITE_STOP( 'REQUIRED VALUE '//TRIM(val_name)//' CAN NOT BE FOUND. STOP!!!' )
            END IF
         ELSE 
            CALL WRITE_STOP( 'REQUIRED VALUE '//TRIM(val_name)//' CAN NOT BE FOUND. STOP!!!' )
         END IF 
      ELSE
         is_default = .TRUE.
         CALL WRITE_LOG( 'VALUE '//TRIM(val_name)//' not specified, using default value '//TRIM(default_str)//'.') 
         RETURN
      END IF
   END IF 

   CALL CHECK_CONSISTENCY_TYPE(val_read, type_detected)

   IF ( type_detected.NE.val_type) THEN
        CALL WRITE_STOP( 'VALUE '//TRIM(val_name)//' IS NOT A/AN '//TRIM(val_type)//'. STOP !!!')
   END IF

   is_default = .FALSE.

END SUBROUTINE

SUBROUTINE READ_INTEGER2(val, val_name, default_val, check)

   IMPLICIT NONE
   INTEGER          , INTENT(OUT)   :: val
   CHARACTER(*)     , INTENT(IN)    :: val_name
   INTEGER, OPTIONAL, INTENT(IN)    :: default_val 
   LOGICAL, OPTIONAL, INTENT(INOUT) :: check  

   LOGICAL :: is_required, file_exists, is_default 
   CHARACTER(80) :: default_str 
   INTEGER :: err 
   !CHARACTER(*) :: val_read

   IF (PRESENT(default_val)) THEN
      is_required = .FALSE.
      default_str = FORMAT_INTEGER(val)
   ELSE
      is_required = .TRUE.
   END IF

   IF (PRESENT(check)) THEN
      CALL READ_VALUE2(val_name, is_required, 'INTEGER', default_str , val_read, is_default, check)
   ELSE
      CALL READ_VALUE2(val_name, is_required, 'INTEGER', default_str , val_read, is_default)
   END IF 
   IF ( is_default ) THEN
      val = default_val   
   ELSE
      READ(val_read, *) val
   END IF

   default_str = FORMAT_INTEGER(val)
   CALL logbuf % ADD(val_name, default_str)

END SUBROUTINE

SUBROUTINE READ_FLOAT2(val, val_name, default_val, units, check)

   IMPLICIT NONE
   REAL(SP)              , INTENT(OUT)   :: val
   CHARACTER(*)          , INTENT(IN)    :: val_name
   REAL(SP)    , OPTIONAL, INTENT(IN)    :: default_val
   CHARACTER(*), OPTIONAL, INTENT(IN)    :: units 
   LOGICAL     , OPTIONAL, INTENT(INOUT) :: check

   LOGICAL :: is_required, file_exists, is_default
   CHARACTER(80) :: default_str
   INTEGER :: err
   !CHARACTER(*) :: val_read

   IF (PRESENT(default_val)) THEN
      is_required = .FALSE.
      default_str = FORMAT_FLOAT(val)
      IF(PRESENT(units)) THEN 
         default_str = TRIM(ADJUSTL(default_str))//' '//TRIM(ADJUSTL(units))
      END IF 
   ELSE
      is_required = .TRUE.
   END IF

   IF (PRESENT(check)) THEN   
      CALL READ_VALUE2(val_name, is_required, 'float', default_str, val_read, is_default, check)
   ELSE
      CALL READ_VALUE2(val_name, is_required, 'float', default_str, val_read, is_default)
   END IF  
   
   IF ( is_default ) THEN
      val = default_val  
   ELSE
      READ(val_read, *) val
   END IF

   default_str = FORMAT_FLOAT(val)
   CALL logbuf % ADD(val_name, default_str)

   
END SUBROUTINE

SUBROUTINE READ_STRING2(val, val_name, default_val, check)

   IMPLICIT NONE
   CHARACTER(*)          , INTENT(OUT) :: val
   CHARACTER(*)          , INTENT(IN)  :: val_name
   CHARACTER(*), OPTIONAL, INTENT(IN)  :: default_val
   LOGICAL     , OPTIONAL, INTENT(INOUT) :: check

   LOGICAL :: is_required, file_exists, is_default
   INTEGER :: err
   !CHARACTER(*) :: val_read

   IF (PRESENT(default_val)) THEN
      is_required = .FALSE.
   ELSE
      is_required = .TRUE.
   END IF

   IF (PRESENT(check)) THEN
      CALL READ_VALUE2(val_name, is_required, 'string', default_val, val_read, is_default, check)
   ELSE
      CALL READ_VALUE2(val_name, is_required, 'string', default_val, val_read, is_default)
   END IF 

   IF ( is_default ) THEN
      val = ADJUSTL(TRIM(default_val))
   ELSE
      val = ADJUSTL(TRIM(val_read))
   END IF

   CALL logbuf % ADD(val_name, val)

END SUBROUTINE

SUBROUTINE READ_LOGICAL2(val, val_name, default_val, check)

   IMPLICIT NONE
   LOGICAL          , INTENT(OUT) :: val
   CHARACTER(*)     , INTENT(IN)  :: val_name
   LOGICAL, OPTIONAL, INTENT(IN)  :: default_val
   LOGICAL, OPTIONAL, INTENT(INOUT) :: check

   LOGICAL :: is_required, file_exists, is_default
   CHARACTER(80) :: default_str
   INTEGER :: err
   !CHARACTER(*) :: val_read

   IF (PRESENT(default_val)) THEN
      is_required = .FALSE.
      default_str = FORMAT_LOGICAL(default_val)
   ELSE
      is_required = .TRUE.
   END IF

   IF (PRESENT(check)) THEN
      CALL READ_VALUE2(val_name, is_required, 'logical', default_str, val_read, is_default, check)
   ELSE
      CALL READ_VALUE2(val_name, is_required, 'logical', default_str, val_read, is_default)
   END IF 

   IF ( is_default ) THEN
      val = default_val
   ELSE
      val = .FALSE.
      IF(val_read(1:1) == 'T') val = .TRUE.
   END IF

   default_str = FORMAT_LOGICAL(val)
   CALL logbuf % ADD(val_name, default_str)

END SUBROUTINE

SUBROUTINE READ_INTEGER(INTEGER_VAL,InputFileName,MatchCharacter,ERR_IF_ONE)  
  IMPLICIT NONE
  INTEGER,          INTENT(OUT) :: INTEGER_VAL 
  CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*), INTENT(IN ) :: MatchCharacter
  INTEGER           :: ERR_IF_ONE

  INQUIRE(FILE=InputFileName,EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(InputFileName), ' CAN NOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(InputFileName), 'CAN NOT BE FOUND. STOP'
    STOP
# endif
  ENDIF

  CALL READ_VALUE(InputFileName,MatchCharacter,VAL_READ,ERR_IF_ONE)
  IF(ERR_IF_ONE == 1)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# else
    WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# endif
     RETURN
  ENDIF

  !check argument type
  CALL CHECK_CONSISTENCY_TYPE(VAL_READ,VAL_TYPE)
  IF(TRIM(VAL_TYPE) .NE. 'INTEGER')THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' IS NOT INTEGER, STOP!'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(MatchCharacter), ' IS NOT INTEGER, STOP!'
    STOP
# endif
  ENDIF

  READ(VAL_READ,*)INTEGER_VAL   

  
  END SUBROUTINE READ_INTEGER


! Float

  SUBROUTINE READ_FLOAT(FLOAT_VAL,InputFileName,MatchCharacter,ERR_IF_ONE)  
  IMPLICIT NONE
  REAL(SP),         INTENT(OUT) :: FLOAT_VAL
  CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*), INTENT(IN ) :: MatchCharacter

  INTEGER           :: ERR_IF_ONE

  INQUIRE(FILE=InputFileName,EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(InputFileName), ' CAN NOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(InputFileName), 'CAN NOT BE FOUND. STOP'
    STOP
# endif
  ENDIF

  CALL READ_VALUE(InputFileName,MatchCharacter,VAL_READ,ERR_IF_ONE)
  IF(ERR_IF_ONE == 1)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# else
    WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# endif
     RETURN
  ENDIF

  CALL CHECK_CONSISTENCY_TYPE(VAL_READ,VAL_TYPE)
  IF(TRIM(VAL_TYPE) .NE. 'float')THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' IS NOT FLOAT, STOP!'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(MatchCharacter), ' IS NOT FLOAT, STOP!'
    STOP
# endif
  ENDIF

  READ(VAL_READ,*)FLOAT_VAL  
    
  
  End SUBROUTINE READ_FLOAT


! Logical
  SUBROUTINE READ_LOGICAL(LOGICAL_VAL,InputFileName,MatchCharacter,ERR_IF_ONE)  
  IMPLICIT NONE
  LOGICAL,          INTENT(OUT) :: LOGICAL_VAL
  CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*), INTENT(IN ) :: MatchCharacter

  INTEGER           :: ERR_IF_ONE

  INQUIRE(FILE=InputFileName,EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(InputFileName), ' CAN NOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(InputFileName), 'CAN NOT BE FOUND. STOP'
    STOP
# endif
  ENDIF

  call READ_VALUE(InputFileName,MatchCharacter,VAL_READ,ERR_IF_ONE)
  IF(ERR_IF_ONE == 1)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# else
    WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# endif
     RETURN
  ENDIF
  !check argument type
  CALL CHECK_CONSISTENCY_TYPE(VAL_READ,VAL_TYPE)
  IF(TRIM(VAL_TYPE) .NE. 'logical')THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' IS NOT LOGICAL, SET FALSE EXCEPT FIRST LETTER IS T'
# else
    WRITE(*,*) TRIM(MatchCharacter), ' IS NOT LOGICAL, SET FALSE EXCEPT FIRST LETTER IS T'
# endif
  ENDIF

  LOGICAL_VAL = .FALSE.
  IF(VAL_READ(1:1) == 'T') LOGICAL_VAL = .TRUE.

  End SUBROUTINE READ_LOGICAL


  SUBROUTINE READ_STRING(STRING_VAL,InputFileName,MatchCharacter,ERR_IF_ONE)  
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(OUT) :: STRING_VAL
  CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*), INTENT(IN ) :: MatchCharacter
  INTEGER, INTENT(OUT) :: ERR_IF_ONE

  INQUIRE(FILE=InputFileName,EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(InputFileName), ' CAN NOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(InputFileName), 'CAN NOT BE FOUND. STOP'
    STOP
# endif
  ENDIF


  call READ_VALUE(InputFileName,MatchCharacter,VAL_READ,ERR_IF_ONE)
  IF(ERR_IF_ONE == 1)THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# else
    WRITE(*,*) TRIM(MatchCharacter), ' DOES NOT EXIST. USE DEFAULT VALUE'
# endif
     RETURN
  ENDIF

  !check argument type
  call CHECK_CONSISTENCY_TYPE(VAL_READ,VAL_TYPE)
  IF(TRIM(VAL_TYPE) .NE. 'string')THEN
# if defined (PARALLEL)
   IF(MYID==0) WRITE(*,*) TRIM(MatchCharacter), ' IS NOT STRING, STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(MatchCharacter), ' IS NOT STRING, STOP'
    STOP
# endif
  ENDIF

  STRING_VAL = ADJUSTL(TRIM(VAL_READ))
  

  End SUBROUTINE READ_STRING


!  read value from file 

  SUBROUTINE READ_VALUE(FileName,MatchCharacter,VAL,ERR_IF_ONE)
  IMPLICIT NONE
  CHARACTER(LEN=* ), INTENT(IN ) :: FileName 
  CHARACTER(LEN=* ), INTENT(IN ) :: MatchCharacter 
  CHARACTER(LEN=80), INTENT(OUT) :: VAL
  INTEGER,           INTENT(OUT) :: ERR_IF_ONE
  
  CHARACTER(LEN=80)  :: TMPV,TMPV_NEXT,TEXT,TEXT_READ
  INTEGER            :: LOCATION,FRONT,TEXT_LENGTH,I
  CHARACTER(LEN=1), PARAMETER :: COM_CHAR = '!'

  ERR_IF_ONE   = 0
  OPEN(10,FILE=TRIM(FileName)) ; REWIND(10) 

   DO WHILE(.TRUE.)

     READ(10,'(A)',END=100) TEXT_READ 


    LOCATION = index(TEXT_READ,com_char)
    if(LOCATION == 0)LOCATION = len_trim(TEXT_READ)+1
    TMPV = TEXT_READ(1:LOCATION-1)
    TEXT = TMPV
    TEXT_LENGTH = len_trim(TEXT)
    FRONT = index(TEXT,"=")
    TMPV_NEXT = TEXT(1:FRONT-1)
    IF(ADJUSTL(TRIM(TMPV_NEXT)) == TRIM(MatchCharacter))THEN
      VAL  = ADJUSTL(TEXT(FRONT+1:TEXT_LENGTH))
      RETURN
    ENDIF

  ENDDO

  100 CONTINUE
  ERR_IF_ONE = 1
  CLOSE(10)

  End SUBROUTINE READ_VALUE  

  SUBROUTINE CHECK_CONSISTENCY_TYPE(VREAD,VTYPE)
  IMPLICIT NONE
  CHARACTER(LEN=80), INTENT(OUT) :: VTYPE 
  CHARACTER(LEN=80), INTENT(IN ) :: VREAD  

  CHARACTER(LEN=16) :: NUMBERS
  INTEGER           :: LOGICAL_VAL_READ
  INTEGER           :: DotLocation
  INTEGER           :: I
  numbers = "0123456789+-Ee. " 

  LOGICAL_VAL_READ = LEN_TRIM(ADJUSTL(VREAD))

! check it it is logical
  IF(LOGICAL_VAL_READ == 1)THEN
  IF(VREAD(1:1) == "T" .or. VREAD(1:1) == "F")then 
    VTYPE = 'logical' 
    RETURN
  ENDIF
  ENDIF
    
  ! check if it is a string
  DO I=1,LOGICAL_VAL_READ
    IF(INDEX(NUMBERS,VREAD(i:i)) == 0)THEN
      VTYPE = "string" 
      RETURN
    ENDIF
  ENDDO

  ! check if it is a float
  DotLocation = index(VREAD,".")
  IF(DotLocation .NE. 0) then
    VTYPE = "float"
  ELSE
    VTYPE = "INTEGER"
  ENDIF

  End SUBROUTINE CHECK_CONSISTENCY_TYPE

END MODULE INPUT_READ


