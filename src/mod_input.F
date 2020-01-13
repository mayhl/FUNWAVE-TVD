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

! Interface for function overloading
INTERFACE READ_VALUE_TMP
   MODULE PROCEDURE READ_FLOAT_TMP
   MODULE PROCEDURE READ_STRING_TMP
   MODULE PROCEDURE READ_LOGICAL_TMP
   MODULE PROCEDURE READ_INTEGER_TMP
END INTERFACE 

! Interface for function overloading and optional arguments
INTERFACE READ_VALUE_CHECK
   MODULE PROCEDURE READ_INTEGER_CHECK
   MODULE PROCEDURE READ_STRING_CHECK
   MODULE PROCEDURE READ_LOGICAL_CHECK
   MODULE PROCEDURE READ_FLOAT_CHECK
END INTERFACE

! Interface for function overloadings
INTERFACE READ_VALUE_CHECK_DELAY_DEFAULT
   MODULE PROCEDURE READ_INTEGER_CHECK_DELAY_DEFAULT
   MODULE PROCEDURE READ_FLOAT_CHECK_DELAY_DEFAULT
   MODULE PROCEDURE READ_STRING_CHECK_DELAY_DEFAULT
   MODULE PROCEDURE READ_LOGICAL_CHECK_DELAY_DEFAULT
END INTERFACE


! Interface for overloading format value functions
INTERFACE FORMAT_VALUE
   MODULE PROCEDURE FORMAT_INTEGER
   MODULE PROCEDURE FORMAT_FLOAT
   MODULE PROCEDURE FORMAT_STRING
   MODULE PROCEDURE FORMAT_LOGICAL
END INTERFACE


#if defined(DOUBLE_PRECISION)
   INTEGER , PARAMETER :: MaxDigits = 16
   REAL(SP), PARAMETER :: ErrTol = 1.0E-15
#else
   INTEGER , PARAMETER :: MaxDigits = 8
   REAL(SP), PARAMETER :: ErrTol = 1.0E-7
#endif
   INTEGER , PARAMETER :: FltStrLen = 2*MaxDigits+1

CONTAINS



!-------------------------------------------------------------------------------------
!
!    FORMAT_XXXXX are simple subroutine for standardizing formating of value types to
!                 strings
! 
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------

SUBROUTINE FORMAT_INTEGER( Val , ValStr )

   IMPLICIT NONE
   INTEGER         , INTENT(IN ) :: Val
   CHARACTER(LEN=*), INTENT(OUT) :: ValStr

   WRITE( ValStr,* ) Val

   ValStr = ADJUSTL(ValStr)

END SUBROUTINE

SUBROUTINE FORMAT_STRING( Val , ValStr )

   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN ) :: Val
   CHARACTER(LEN=*), INTENT(OUT) :: ValStr

   ValStr = ADJUSTL(Val)

END SUBROUTINE

SUBROUTINE FORMAT_FLOAT( Val , ValStr )

   USE PARAM, ONLY: SP

   IMPLICIT NONE
   REAL(SP)        , INTENT(IN ) :: Val
   CHARACTER(LEN=*), INTENT(OUT) :: ValStr

   REAL(SP)                      :: TmpVal

   INTEGER                       :: i
   CHARACTER(LEN=10)             :: FmtStr
   CHARACTER(LEN=FltStrLen)      :: TmpStr
   REAL(SP)                      :: ConvErr

   WRITE(FmtStr,'(A2,I2,A1,I2,A1)') '(F', (2*MaxDigits+1) , '.' , MaxDigits , ')' 
   WRITE(ValStr,FmtStr) Val   

   ! Formatting REAL to STRING with increasing number of digits and
   ! back to REAL until error tolerance is met
   DO i = 1, MaxDigits

      WRITE(FmtStr,'(A2,I2,A1,I2,A1)') '(F', (2*MaxDigits+1) , '.' , i , ')'
      WRITE(TmpStr,FmtStr) Val
      
      READ(TmpStr,*) TmpVal
      ConvErr = ABS( (TmpVal-Val)/Val )

      IF( ConvErr.LT.ErrTol ) THEN
         ValStr = TmpStr
         EXIT
      END IF

   END DO

   ValStr = ADJUSTL( ValStr )

END SUBROUTINE

SUBROUTINE FORMAT_LOGICAL( Val , ValStr )

   IMPLICIT NONE
   LOGICAL         , INTENT(IN ) :: Val
   CHARACTER(LEN=*), INTENT(OUT) :: ValStr

   IF ( Val ) THEN
      ValStr = 'T'
   ELSE
      ValStr = 'F'
   END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    WRITE_VARIABLE_VALUE is a simple wrapper subroutine for standardizing output
! 
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE WRITE_VARIABLE_VALUE( VariableName, VariableValueStr)

   USE LOG_OUTPUT, ONLY: WRITE_LOG

   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: VariableName
   CHARACTER(LEN=*), INTENT(IN) :: VariableValueStr

   CALL WRITE_LOG( TRIM(VariableName)//" = "//TRIM(VariableValueStr) )

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_CHECK is a subroutine to automatically generate error message from 
!               READ_VALUE_TMP.
!    NOTE 1: Subroutine terminate program unless 
!            1) ErrID is 0 (value successfully read from file), or
!            2) ErrID is 2 (variable name not found in file) AND DefaultValueStr 
!               (DefaultValue is wrapping subroutines) is specified  
!    NOTE 2: Units is an optional argument for adding units to DefaultValueStr in
!            output message.
! 
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_CHECK( InputFileName, VariableName, ValueType,  ErrID , DefaultValueStr, Units )

   USE LOG_OUTPUT, ONLY: WRITE_LOG, WRITE_ALL

   IMPLICIT NONE
   CHARACTER(LEN=*)          , INTENT(IN) :: InputFileName
   CHARACTER(LEN=*)          , INTENT(IN) :: VariableName
   CHARACTER(LEN=*)          , INTENT(IN) :: ValueType
   INTEGER                   , INTENT(IN) :: ErrID
   CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: DefaultValueStr
   CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: Units

   CHARACTER(LEN=120)                     :: Msg

   Msg = TRIM(VariableName)//" not read,"

   IF      ( ErrID.EQ.3 ) THEN
      ! File not found error
      CALL WRITE_ALL( "ERROR: "//TRIM(Msg)//" file '"//TRIM(InputFileName)//"' not found. STOPPING SIMULATION!" )
      CALL TERMINATE

   ELSE IF ( ErrID.EQ.2 ) THEN
      ! Varible not found error 

      IF ( PRESENT( DefaultValueStr) ) THEN
         Msg = "WARNING: "//TRIM(Msg)//" variable not found in input file."
      ELSE
         Msg = "ERROR: "//TRIM(Msg)//" variable not found in input file."
      END IF

      IF ( PRESENT( DefaultValueStr ) ) THEN
         ! Using default value and units if specified.

         Msg = TRIM(Msg)//" Defaulting to "//TRIM(DefaultValueStr)

         IF ( PRESENT( Units ) ) THEN
            Msg = TRIM(Msg)//" "//TRIM(Units)//"."
         ELSE
            Msg = TRIM(Msg)//"."
         END IF

         CALL WRITE_ALL( Msg )
         RETURN
      ELSE
         ! Stopping code if not default value found
         Msg = TRIM(Msg)//" STOPPING SIMULATION!"
         CALL WRITE_ALL( Msg )
         CALL TERMINATE
      END IF

   ELSE IF( ErrID.EQ.1 ) THEN
      ! Error type mismatch error
      CALL WRITE_ALL( "ERROR: "//TRIM(Msg)//" "//TRIM(ValueType)//" value type not detected. STOPPING SIMULATION!" )
      CALL TERMINATE

   ELSE IF ( ErrID.NE.0 ) THEN
      ! Invalid ErrID error
      CALL WRITE_ALL("ERROR: "//TRIM(Msg)//" invalid ErrID returned from READ_INPUT. STOPPING SIMULATION!" )
      CALL TERMINATE

   END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_INTEGER_CHECK is a soubroutine wrapping READ_CHECK subroutine for integer
!                       type values, defaulting to DefaultValue (if specified), and
!                       adding Units to DefaultValue in output message (if specified). 
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_INTEGER_CHECK(ReturnValue, InputFileName, VariableName, DefaultValue, Units  )

  IMPLICIT NONE

  INTEGER         ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  INTEGER         , OPTIONAL, INTENT(IN ) :: DefaultValue
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: Units

  CHARACTER(LEN=7), PARAMETER             :: ValueType="integer"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)

  IF ( PRESENT( DefaultValue ) ) THEN
     IF (ErrID.NE.0 ) ReturnValue = DefaultValue
     CALL FORMAT_VALUE( ReturnValue , ValueStr)

     IF ( PRESENT(Units) ) THEN
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr, Units )
     ELSE
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr )
     END IF
  ELSE
     CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID )
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
  END IF

  CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )


END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_FLOAT_CHECK is a soubroutine wrapping READ_CHECK subroutine for float
!                     type values, defaulting to DefaultValue (if specified), and
!                     adding Units to DefaultValue in output message (if specified).  
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_FLOAT_CHECK(ReturnValue, InputFileName, VariableName, DefaultValue, Units  )

  USE PARAM, ONLY: SP
  IMPLICIT NONE

  REAL(SP)        ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  REAL(SP)        , OPTIONAL, INTENT(IN ) :: DefaultValue
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: Units

  CHARACTER(LEN=5), PARAMETER             :: ValueType="float"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=FltStrLen)                :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)

  IF ( PRESENT( DefaultValue ) ) THEN

     IF (ErrID.NE.0 ) ReturnValue = DefaultValue
     CALL FORMAT_VALUE( ReturnValue , ValueStr)

     IF ( PRESENT(Units) ) THEN
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr, Units )
     ELSE
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr )
     END IF
  ELSE
     CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID )
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
  END IF

  CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_LOGICAL_CHECK is a soubroutine wrapping READ_CHECK subroutine for logical
!                       type values, defaulting to DefaultValue (if specified), and
!                       adding Units to DefaultValue in output message (if specified).  
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_LOGICAL_CHECK(ReturnValue, InputFileName, VariableName, DefaultValue, Units  )

  IMPLICIT NONE

  LOGICAL         ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  LOGICAL         , OPTIONAL, INTENT(IN ) :: DefaultValue
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: Units

  CHARACTER(LEN=7), PARAMETER             :: ValueType="logical"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)

  IF ( PRESENT( DefaultValue ) ) THEN

     IF (ErrID.NE.0 ) ReturnValue = DefaultValue
     CALL FORMAT_VALUE( ReturnValue , ValueStr)

     IF ( PRESENT(Units) ) THEN
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr, Units )
     ELSE
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr )
     END IF
  ELSE
     CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID )
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
  END IF

  CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_STRING_CHECK is a soubroutine wrapping READ_CHECK subroutine for string
!                      type values, defaulting to DefaultValue (if specified), and
!                      adding Units to DefaultValue in output message (if specified).  
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_STRING_CHECK(ReturnValue, InputFileName, VariableName, DefaultValue, Units  )

  IMPLICIT NONE

  CHARACTER(LEN=*),           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: DefaultValue
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: Units

  CHARACTER(LEN=6), PARAMETER             :: ValueType="string"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)

  IF ( PRESENT( DefaultValue ) ) THEN

     IF (ErrID.NE.0 ) ReturnValue = DefaultValue
     CALL FORMAT_VALUE( ReturnValue , ValueStr)

     IF ( PRESENT(Units) ) THEN
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr, Units )
     ELSE
        CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID, ValueStr )
     END IF
  ELSE
     CALL READ_CHECK( InputFileName, VariableName, ValueType, ErrID )
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
  END IF

  CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_FLOAT_CHECK_DELAY_DEFAULT is a soubroutine wrapping READ_CHECK_DELAY_DEFAULT
!                                   subroutine for integer type values and delays defaulting
!                                   to a default value in the case variable is not 
!                                   VariableName 
!    HISTORY: 
!        12/19/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_FLOAT_CHECK_DELAY_DEFAULT(ReturnValue, InputFileName, VariableName, IsDefault  )

  USE PARAM, ONLY: SP
  USE LOG_OUTPUT, ONLY: WRITE_ALL
  IMPLICIT NONE

  REAL(SP)        ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  LOGICAL         ,           INTENT(OUT) :: IsDefault


  CHARACTER(LEN=5), PARAMETER             :: ValueType="float"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=FltStrLen)                :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)
  CALL READ_CHECK_DELAY_DEFAULT( InputFileName, VariableName, ValueType, ErrID )

  IF       ( ErrID.EQ.2 ) THEN
     IsDefault=.TRUE.
  ELSE IF  ( ErrID.EQ.1 ) THEN
     IsDefault=.FALSE.
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
     CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )
  ELSE
     ! Saftey check, code should not be called. 
     CALL WRITE_ALL ("FATAL: Unexpected error state. STOPPING SIMULATION!" )
     CALL TERMINATE
  END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_LOGICAL_CHECK_DELAY_DEFAULT is a soubroutine wrapping READ_CHECK_DELAY_DEFAULT
!                                     subroutine for logical type values and delays defaulting
!                                     to a default value in the case variable is not 
!                                     VariableName 
!    HISTORY: 
!        12/19/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_LOGICAL_CHECK_DELAY_DEFAULT(ReturnValue, InputFileName, VariableName, IsDefault  )

  USE LOG_OUTPUT, ONLY: WRITE_ALL
  IMPLICIT NONE

  LOGICAL         ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  LOGICAL         ,           INTENT(OUT) :: IsDefault


  CHARACTER(LEN=7), PARAMETER             :: ValueType="logical"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)
  CALL READ_CHECK_DELAY_DEFAULT( InputFileName, VariableName, ValueType, ErrID )

  IF       ( ErrID.EQ.2 ) THEN
     IsDefault=.TRUE.
  ELSE IF  ( ErrID.EQ.1 ) THEN
     IsDefault=.FALSE.
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
     CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )
  ELSE
     ! Saftey check, code should not be called. 
     CALL WRITE_ALL ("FATAL: Unexpected error state. STOPPING SIMULATION!" )
     CALL TERMINATE
  END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_INTEGER_CHECK_DELAY_DEFAULT is a soubroutine wrapping READ_CHECK_DELAY_DEFAULT
!                                     subroutine for integer type values and delays defaulting
!                                     to a default value in the case variable is not 
!                                     VariableName 
!    HISTORY: 
!        12/19/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_INTEGER_CHECK_DELAY_DEFAULT(ReturnValue, InputFileName, VariableName, IsDefault  )

  USE LOG_OUTPUT, ONLY: WRITE_ALL
  IMPLICIT NONE

  INTEGER         ,           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  LOGICAL         ,           INTENT(OUT) :: IsDefault


  CHARACTER(LEN=7), PARAMETER             :: ValueType="integer"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)
  CALL READ_CHECK_DELAY_DEFAULT( InputFileName, VariableName, ValueType, ErrID )

  IF       ( ErrID.EQ.2 ) THEN
     IsDefault=.TRUE.
  ELSE IF  ( ErrID.EQ.1 ) THEN
     IsDefault=.FALSE.
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
     CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )
  ELSE
     ! Saftey check, code should not be called. 
     CALL WRITE_ALL ("FATAL: Unexpected error state. STOPPING SIMULATION!" )
     CALL TERMINATE
  END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_STRING_CHECK_DELAY_DEFAULT is a soubroutine wrapping READ_CHECK_DELAY_DEFAULT
!                                    subroutine for string type values and delays defaulting
!                                    to a default value in the case variable is not 
!                                    VariableName 
!    HISTORY: 
!        12/19/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_STRING_CHECK_DELAY_DEFAULT(ReturnValue, InputFileName, VariableName, IsDefault  )

  USE LOG_OUTPUT, ONLY: WRITE_ALL
  IMPLICIT NONE

  CHARACTER(LEN=*),           INTENT(OUT) :: ReturnValue
  CHARACTER(LEN=*),           INTENT(IN ) :: InputFileName
  CHARACTER(LEN=*),           INTENT(IN ) :: VariableName
  LOGICAL         ,           INTENT(OUT) :: IsDefault


  CHARACTER(LEN=6), PARAMETER             :: ValueType="string"
  INTEGER                                 :: ErrID
  CHARACTER(LEN=30)                       :: ValueStr

  CALL READ_VALUE_TMP( ReturnValue, InputFileName, VariableName, ErrID)
  CALL READ_CHECK_DELAY_DEFAULT( InputFileName, VariableName, ValueType, ErrID )

  IF       ( ErrID.EQ.2 ) THEN
     IsDefault=.TRUE.
  ELSE IF  ( ErrID.EQ.1 ) THEN
     IsDefault=.FALSE.
     CALL FORMAT_VALUE( ReturnValue , ValueStr)
     CALL WRITE_VARIABLE_VALUE( VariableName, ValueStr )
  ELSE
     ! Saftey check, code should not be called. 
     CALL WRITE_ALL ("FATAL: Unexpected error state. STOPPING SIMULATION!" ) 
     CALL TERMINATE
  END IF

END SUBROUTINE


!-------------------------------------------------------------------------------------
!
!    READ_CHECK_DELAY_DEFAULT is a subroutine to automatically generate error message
!                             from READ_VALUE_TMP. Delay proccessing of default value
!                             in the case VariableName defaults a different variable.
! 
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_CHECK_DELAY_DEFAULT( InputFileName, VariableName, ValueType,  ErrID )

   USE LOG_OUTPUT, ONLY: WRITE_LOG, WRITE_ALL

   IMPLICIT NONE
   CHARACTER(LEN=*)          , INTENT(IN) :: InputFileName
   CHARACTER(LEN=*)          , INTENT(IN) :: VariableName
   CHARACTER(LEN=*)          , INTENT(IN) :: ValueType
   INTEGER                   , INTENT(IN) :: ErrID

   CHARACTER(LEN=120)                     :: Msg

   Msg = TRIM(VariableName)//" not read,"

   IF      ( ErrID.EQ.3 ) THEN
      ! File not found error
      CALL WRITE_ALL( "ERROR: "//TRIM(Msg)//" file '"//TRIM(InputFileName)//"' not found. STOPPING SIMULATION!" )
      CALL TERMINATE

   ELSE IF ( ErrID.EQ.2 ) THEN
      ! Varible not found error 
      RETURN 

   ELSE IF( ErrID.EQ.1 ) THEN
      ! Error type mismatch error
      CALL WRITE_ALL( "ERROR: "//TRIM(Msg)//" "//TRIM(ValueType)//" value type not detected. STOPPING SIMULATION!" )
      CALL TERMINATE

   ELSE IF ( ErrID.NE.0 ) THEN
      ! Invalid ErrID error
      CALL WRITE_ALL("ERROR: "//TRIM(Msg)//" invalid ErrID returned from READ_INPUT. STOPPING SIMULATION!" )
      CALL TERMINATE

   END IF

END SUBROUTINE





!-------------------------------------------------------------------------------------
!
!    PARSE_VALUE is a subroutine for opening file, parsing file for variable name, and 
!                get variable value as a string 
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE PARSE_VALUE( ReturnValue, InputFileName, VariableName, VariableType, ErrID )
   IMPLICIT NONE

   CHARACTER(LEN=*) , INTENT(OUT) :: ReturnValue
   CHARACTER(LEN=*) , INTENT(IN ) :: InputFileName
   CHARACTER(LEN=*) , INTENT(IN ) :: VariableName
   CHARACTER(LEN=*) , INTENT(IN ) :: VariableType
   INTEGER          , INTENT(OUT) :: ErrID

   CHARACTER(LEN=12)             :: ValueType
   LOGICAL                       :: FileExists 

   INQUIRE(FILE=InputFileName,EXIST=FileExists)

   IF (.NOT.FileExists ) THEN
      ErrID = 3
      RETURN
   END IF

   CALL READ_VALUE( InputFileName, VariableName, ReturnValue, ErrID )

   IF ( ErrID.EQ.1 ) THEN
      ErrId = 2
      RETURN
   END IF

   CALL CHECK_CONSISTENCY_TYPE( ReturnValue, ValueType )

   IF ( TRIM(VariableType).NE.TRIM(ValueType) ) THEN
     ErrID = 1
     RETURN
   END IF

   ErrID = 0

END SUBROUTINE PARSE_VALUE


!-------------------------------------------------------------------------------------
!
!    READ_INTEGER_TMP is a subroutine wrapping PARSE_VALUE for integer value types
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_INTEGER_TMP( ReturnValue, InputFileName, VariableName, ErrID )
   IMPLICIT NONE

   INTEGER         , INTENT(OUT) :: ReturnValue
   CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
   CHARACTER(LEN=*), INTENT(IN ) :: VariableName 
   INTEGER         , INTENT(OUT) :: ErrID

   CHARACTER(LEN=40)             :: ValueString
  
   CALL PARSE_VALUE( ValueString, InputFileName, VariableName, 'INTEGER' , ErrID )

   IF ( ErrID.NE.0 ) RETURN
   
   READ(ValueString,*) ReturnValue

END SUBROUTINE READ_INTEGER_TMP


!-------------------------------------------------------------------------------------
!
!    READ_FLOAT_TMP is a subroutine wrapping PARSE_VALUE for float value types
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!------------------------------------------------------------------------------------- 
SUBROUTINE READ_FLOAT_TMP( ReturnValue, InputFileName, VariableName, ErrID )
   IMPLICIT NONE

   REAL(SP)        , INTENT(OUT) :: ReturnValue
   CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
   CHARACTER(LEN=*), INTENT(IN ) :: VariableName
   INTEGER         , INTENT(OUT) :: ErrID

   CHARACTER(LEN=40)             :: ValueString
  
   CALL PARSE_VALUE( ValueString, InputFileName, VariableName, 'float' , ErrID )

   IF ( ErrID.NE.0 ) RETURN
   
   READ(ValueString,*) ReturnValue

END SUBROUTINE READ_FLOAT_TMP


!-------------------------------------------------------------------------------------
!
!    READ_STRING_TMP is a subroutine wrapping PARSE_VALUE for string value types
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_STRING_TMP( ReturnValue, InputFileName, VariableName, ErrID )
   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(OUT) :: ReturnValue
   CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
   CHARACTER(LEN=*), INTENT(IN ) :: VariableName
   INTEGER         , INTENT(OUT) :: ErrID

   CHARACTER(LEN=40)             :: ValueString
  
   CALL PARSE_VALUE( ValueString, InputFileName, VariableName, 'string' , ErrID )

   IF ( ErrID.NE.0 ) RETURN

   ReturnValue = ADJUSTL(TRIM(ValueString))

END SUBROUTINE READ_STRING_TMP


!-------------------------------------------------------------------------------------
!
!    READ_LOGICAL_TMP is a subroutine wrapping PARSE_VALUE for logical value types
!
!    HISTORY: 
!        12/17/2019 Michael-Angelo Y.-H. Lam
!
!-------------------------------------------------------------------------------------
SUBROUTINE READ_LOGICAL_TMP( ReturnValue, InputFileName, VariableName, ErrID )
   IMPLICIT NONE

   LOGICAL         , INTENT(OUT) :: ReturnValue
   CHARACTER(LEN=*), INTENT(IN ) :: InputFileName
   CHARACTER(LEN=*), INTENT(IN ) :: VariableName
   INTEGER         , INTENT(OUT) :: ErrID

   CHARACTER(LEN=40)             :: ValueString
  
   CALL PARSE_VALUE( ValueString, InputFileName, VariableName, 'logical' , ErrID )

   IF ( ErrID.NE.0 ) RETURN

   IF ( ValueString(1:1) == 'T' ) THEN 
      ReturnValue = .TRUE.
   ELSE
      ReturnValue = .FALSE.
   END IF

END SUBROUTINE READ_LOGICAL_TMP


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
  CHARACTER(LEN=*), INTENT(OUT) :: VTYPE 
  CHARACTER(LEN=*), INTENT(IN ) :: VREAD  

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


