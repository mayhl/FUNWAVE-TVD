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

# if defined (PARALLEL)
# define PWRITE IF (myid.EQ.0) WRITE
# else
# define PWRITE WRITE
# endif


MODULE LOG_IO

# if defined (PARALLEL)
  USE GLOBAL, ONLY : myid, ier 
  USE MPI 
# endif
  USE PARAM

  IMPLICIT NONE
  CHARACTER(LEN=100) :: msg_buff
  INTEGER, PARAMETER :: LU = 3

  !IMPLICIT NONE
  

  ! Doubly Linked list for buffer log file lines  before writing 
  TYPE LOG_ITEM
    CHARACTER(80) :: val_name
    CHARACTER(80) :: val_str
    TYPE(LOG_ITEM), POINTER :: next => NULL()
    TYPE(LOG_ITEM), POINTER :: prev => NULL()
  END TYPE

  TYPE LOG_BUFFER
    TYPE(LOG_ITEM), POINTER :: head => NULL()
    TYPE(LOG_ITEM), POINTER :: tail => NULL()
  CONTAINS
    PROCEDURE :: ADD          => ADD_LOG_LINE
    PROCEDURE :: CLEAR        => CLEAR_LOG_BUFFER
    PROCEDURE :: FLUSH2OUTPUT => WRITE_LOG_BUFFER
    PROCEDURE :: DPOP         => DELETE_POP
  END TYPE

CONTAINS


  ! AMLOST_EQUAL is a subroutine for checking if floats of 'equal' enough
  PURE FUNCTION ALMOST_EQUAL( a, b ) RESULT( are_equal )

    USE PARAM, ONLY : SP
    IMPLICIT NONE
    REAL(SP), INTENT(IN)  :: a, b
    LOGICAL :: are_equal


    IF ( a == 0.0_SP .OR. b == 0.0_SP ) THEN
      IF ( ABS(a-b) <= EPSILON(1.0_SP) ) THEN
        are_equal = .TRUE.
      ELSE
        are_equal = .FALSE.
      ENDIF
    ELSE
      IF( (abs(a-b) <= EPSILON(1.0_SP)*abs(a)) .OR. &
          (abs(a-b) <= EPSILON(1.0_SP)*abs(b)) ) THEN
        are_equal = .TRUE.
      ELSE
        are_equal = .FALSE.
      ENDIF
    ENDIF
  END FUNCTION


  SUBROUTINE FORMAT_ELAPSED(secs, fmt_elap)
  
    IMPLICIT NONE
    REAL(SP)        , INTENT(IN)  :: secs
    CHARACTER(LEN=*), INTENT(OUT) :: fmt_elap
    
    CHARACTER(10), PARAMETER :: fmt_str = '(F10.2, A)'

    REAL(SP) :: mins, hrs

    IF (secs.LT.60.0) THEN
      WRITE(fmt_elap, fmt_str) secs, ' seconds'
    ELSE
      mins = secs/60.0
      IF (mins.LT.60.0) THEN
        WRITE(fmt_elap, fmt_str) mins, ' minutes'
      ELSE
        hrs = mins/60.0
        WRITE(fmt_elap, fmt_str) hrs, ' hours'
      ENDIF

    ENDIF

    fmt_elap=ADJUSTL(fmt_elap)

  END SUBROUTINE
 

  ! Wrapper routine swapping between parallel and serial system tme
  SUBROUTINE GET_TIME(time)

    IMPLICIT NONE
    REAL(SP), INTENT(OUT) :: time
    !IMPLICIT NONE

#if defined (PARALLEL)
    IF (myid.EQ.0) time = MPI_Wtime()
#else
    CALL CPU_TIME(time)
#endif
   
  END SUBROUTINE

  ! Wrapper routine for writing debug messages
  SUBROUTINE DWRITE(msg)
  
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: msg

# if defined (DEBUG)
     PWRITE(*,*) TRIM(msg)
# endif

  END SUBROUTINE

  ! Wrapper routine for writing to console and log file
  SUBROUTINE LWRITE(msg)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: msg

    PWRITE(LU,*) TRIM(msg)
    PWRITE(* ,*) TRIM(msg)

  END SUBROUTINE 

  SUBROUTINE UWRITE(unit, msg)

    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER     , INTENT(IN) :: unit

    PWRITE(unit, *) TRIM(msg)

  END SUBROUTINE 

  ! Wrapper routine for write error message and exiting program
  SUBROUTINE SWRITE(msg)

# if defined PARALLEL
    USE MPI
# endif
    IMPLICIT NONE
    CHARACTER(*), INTENT(IN) :: msg 
    INTEGER :: ier

    CALL LWRITE("FATAL ERROR: "//TRIM(msg)//" STOP!!!") 
# if defined (PARALLEL)
    !CALL MPI_BARRIER(MPI_COMM_WORLD, ier)
    CALL MPI_FINALIZE(ier)
# endif
    STOP

  END SUBROUTINE

  ! Wrapper routine for writing a title wrapped in banners
  SUBROUTINE BWRITE(title)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: title
    INTEGER         , PARAMETER  :: BannerLength = 60
    CHARACTER(LEN=1), PARAMETER  :: BannerChar = '-'
    INTEGER                      :: TitleLength
    INTEGER                      :: ExtraLength
    INTEGER                      :: LeftLength
    INTEGER                      :: RightLength
    CHARACTER(BannerLength+1)    :: Banner

    TitleLength = LEN_TRIM(ADJUSTL(Title))
    ExtraLength = BannerLength - TitleLength
    LeftLength = ExtraLength/2 - 1
    RightLength = LeftLength + MOD( ExtraLength , 2 )

    banner = NEW_LINE('A')//&
             REPEAT(BannerChar, LeftLength)//&
             ' '//TRIM(ADJUSTL(Title))//' '//&
             REPEAT(BannerChar, RightLength)

#if defined (PARALLEL)
    IF (myid.EQ.0) WRITE(LU,*) banner
    IF (myid.EQ.0) PRINT *, banner 
#else
    WRITE(LU,*) banner
    PRINT *, banner
#endif 

  END SUBROUTINE

! -----------------------------
! LOGGER TYPE/CLASS SUBROUTINES
! -----------------------------
  SUBROUTINE WRITE_LOG_BUFFER(this)

    IMPLICIT NONE
    CLASS(LOG_BUFFER), INTENT(INOUT) :: this

    TYPE(LOG_ITEM), POINTER :: current
    INTEGER :: max_len, tmp_len
    CHARACTER(163) :: line 
    
# if defined (PARALLEL)
    ! Do nothing if not master node
    !IF (myid.NE.0 ) RETURN
# endif

    max_len = 0 
    current => this % head

    DO WHILE(ASSOCIATED(current))
      tmp_len = LEN_TRIM(current % val_name)
      IF (tmp_len.GT.max_len) max_len = tmp_len
      current => current % next
    END DO

    current => this % head
    DO WHILE(ASSOCIATED(current))
      line = current % val_name(:max_len)//' = '//current % val_str
      CALL LWRITE(TRIM(line))
      current => current % next
    END DO

    CALL this % CLEAR()

  END SUBROUTINE

  SUBROUTINE DELETE_POP(this, n)

    IMPLICIT NONE
    CLASS(LOG_BUFFER), INTENT(INOUT) :: this
    INTEGER          , INTENT(IN   ) :: n

    TYPE(LOG_ITEM), POINTER :: current, tmp
    INTEGER                 :: i

    current => this % tail

    DO I = 1,n
      tmp => current % prev
      NULLIFY(current % prev)
      NULLIFY(current % next)
      DEALLOCATE(current)
      current => tmp
    END DO

    NULLIFY(current % next)
    this % tail => current 

  END SUBROUTINE

  SUBROUTINE CLEAR_LOG_BUFFER(this)

    IMPLICIT NONE
    CLASS(LOG_BUFFER), INTENT(INOUT) :: this

    TYPE(LOG_ITEM), POINTER :: current, tmp

# if defined (PARALLEL)
    ! Do nothing if not master node
    !IF (myid.NE.0 ) RETURN
# endif

    current => this % tail

    DO WHILE(ASSOCIATED(current))
      tmp =>  current % prev
      NULLIFY(current % prev)
      NULLIFY(current % next)
      DEALLOCATE(current)
      current => tmp
      !DEALLOCATE(tmp)
    END DO

    NULLIFY(this % head)
    NULLIFY(this % tail)

  END SUBROUTINE


  SUBROUTINE ADD_LOG_LINE(this, val_name, val_str)

    IMPLICIT NONE
    CLASS(LOG_BUFFER), INTENT(INOUT) :: this
    CHARACTER(*)     , INTENT(IN)    :: val_name
    CHARACTER(*)     , INTENT(IN)    :: val_Str

    TYPE(LOG_ITEM), POINTER :: current, prev

# if defined (PARALLEL)
    ! Do nothing if not master node
    !IF (myid.NE.0 ) RETURN
# endif

    ALLOCATE(current)
    current % val_name = ADJUSTL(val_name)
    current % val_str  = ADJUSTL(val_str)

    !current % next => NULL()
    IF (.NOT.ASSOCIATED(this % head)) THEN
      !current % prev => NULL()
      !current % next => NULL()
      this % head => current
      this % tail => current
    ELSE
      this % tail % next => current
      current % prev => this % tail
      this % tail => current
    END IF

  END SUBROUTINE

END MODULE

