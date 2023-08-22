!------------------------------------------------------------------------------------
!
!    FILE logio.F
!
!    This file is part of the FUNWAVE-TVD program under the Simplified BSD license
!
!-------------------------------------------------------------------------------------
! 
!  Copyright (c) 2016, FUNWAVE Development Team
!
!  (See http://www.udel.edu/kirby/programs/funwave/funwave.html
!   for Development Team membership)
!
!  All rights reserved.
!
!  FUNWAVE_TVD is free software: you can redistribute it and/or modify
!  it under the terms of the Simplified BSD License as released by
!  the Berkeley Software Distribution (BSD).
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions are met:
!
!  1. Redistributions of source code must retain the above copyright notice, this
!     list of conditions and the following disclaimer.
!  2. Redistributions in binary form must reproduce the above copyright notice,
!  this list of conditions and the following disclaimer in the documentation
!  and/or other materials provided with the distribution.
!
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!  
!  The views and conclusions contained in the software and documentation are those
!  of the authors and should not be interpreted as representing official policies,
!  either expressed or implied, of the FreeBSD Project.
!  
!-------------------------------------------------------------------------------------


MODULE MOD_FUNITS

  USE LOG_IO
  IMPLICIT NONE

  ! Input file unit
  INTEGER, PARAMETER       :: start_unit = 20

  TYPE UNIT_ITEM
    CHARACTER(80)            :: name 
    INTEGER                  :: unit 
    TYPE(UNIT_ITEM), POINTER :: next => NULL()
    TYPE(UNIT_ITEM), POINTER :: prev => NULL()
  END TYPE

  TYPE FILE_UNITS
    TYPE(UNIT_ITEM), POINTER :: head => NULL()
    TYPE(UNIT_ITEM), POINTER :: tail => NULL() 
    INTEGER                  :: free_unit = 20
  CONTAINS
    PROCEDURE :: ADD       => ADD_FUNIT
    PROCEDURE :: ADD_RANGE => ADD_FUNIT_RANGE
    PROCEDURE :: FIND      => FIND_UITEM
    PROCEDURE :: GET       => GET_FUNIT
  END TYPE

  TYPE(FILE_UNITS), SAVE :: fhandle

CONTAINS

  FUNCTION FIND_UITEM(this, name) RESULT (uitem)

    IMPLICIT NONE
    CLASS(FILE_UNITS), INTENT(INOUT) :: this
    CHARACTER(LEN=*) , INTENT(IN)    :: name
    TYPE(UNIT_ITEM), POINTER         :: uitem, current 

    uitem => NULL()

    current => this % head

    !CALL DWRITE('FIND FUINIT')
    DO WHILE(ASSOCIATED(current))
  
        IF (TRIM(ADJUSTL(current % name)).EQ.TRIM(ADJUSTL(name))) THEN
          uitem => current 
          !CALL DWRITE('FUNIT FOUND')
          RETURN
        ENDIF
        current => current % next 

    END DO
    !CALL DWRITE('FUNIT NOT FOUND')

  END FUNCTION

  FUNCTION GET_FUNIT(this, name) RESULT (unit)

    IMPLICIT NONE
    CLASS(FILE_UNITS), INTENT(INOUT) :: this
    CHARACTER(LEN=*) , INTENT(IN)    :: name
    INTEGER                          :: unit 

    TYPE(UNIT_ITEM), POINTER :: uitem

    uitem => this % FIND(name)

    IF (.NOT.ASSOCIATED(uitem)) THEN
      CALL SWRITE("FAILED TO GET FILE UNIT FOR '"//TRIM(ADJUSTL(name))//"'.")
    END IF
    
    unit = uitem % unit

  END FUNCTION

  FUNCTION ADD_FUNIT(this, name) RESULT (unit)

    IMPLICIT NONE
    CLASS(FILE_UNITS), INTENT(INOUT) :: this
    CHARACTER(LEN=*) , INTENT(IN)    :: name
    INTEGER                          :: unit 

    unit = this % ADD_RANGE(name, 1)

  END FUNCTION

  FUNCTION ADD_FUNIT_RANGE(this, name, amount) RESULT (unit)

    IMPLICIT NONE
    CLASS(FILE_UNITS), INTENT(INOUT) :: this
    CHARACTER(LEN=*) , INTENT(IN)    :: name
    INTEGER          , INTENT(IN)    :: amount
    INTEGER                          :: unit 
    
    TYPE(UNIT_ITEM), POINTER :: uitem

    ! Check in file name is in list already 
    uitem => this % FIND(name)
    IF (ASSOCIATED(uitem)) THEN
      CALL SWRITE("FAILED TO ADD UNIT ITEM, NAME '"//TRIM(ADJUSTL(name))//"' IS ALREADY IN LIST.")
    END IF 

    ALLOCATE(uitem)

    ! Setup new file name and unit
    uitem % name = TRIM(ADJUSTL(name))
    uitem % unit = this % free_unit
    this % free_unit = this % free_unit + amount 

#if defined(DEBUG)
    WRITE(msg_buff, *) uitem % unit
    CALL DWRITE("Adding file '"//TRIM(uitem % name)//"' with unit "//TRIM(ADJUSTL(msg_buff))//".")  
#endif

    ! If first item to list, setup head and tail correctly 
    IF (.NOT.ASSOCIATED(this % head)) THEN
      this % head => uitem
      this % tail => uitem
    ELSE
      ! Adding new items to end of list 
      uitem % prev => this % tail
      this % tail % next => uitem 
      this % tail => uitem 
    ENDIF

    unit = uitem % unit 


  END FUNCTION

END MODULE

