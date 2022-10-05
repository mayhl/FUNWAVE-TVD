!------------------------------------------------------------------------------------
!
!      FILE mod_logger.F
!
!      This file is part of the FUNWAVE-TVD program under the Simplified
!      BSD license
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
!    modification, are permitted provided that the following conditions
!    are met:
!
!    1. Redistributions of source code must retain the above copyright
!    notice, this
!       list of conditions and the following disclaimer.
!    2. Redistributions in binary form must reproduce the above
!    copyright notice,
!    this list of conditions and the following disclaimer in the
!    documentation
!    and/or other materials provided with the distribution.
!
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!    "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!    THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
!    ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
!    BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!    CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!    SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!    CAUSED AND
!    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!    OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
!    USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!  
!    The views and conclusions contained in the software and
!    documentation are those
!    of the authors and should not be interpreted as representing
!    official policies,
!    either expressed or implied, of the FreeBSD Project.
!  
!------------------------------------------------------------------------------------

# if defined (PARALLEL)
#define SWRITE IF (myid.EQ.0 ) WRITE
# else
#define SWRITE WRITE
# endif

MODULE LOGGER
# if defined (PARALLEL)
    USE GLOBAL, ONLY : myid, ier
    USE MPI
# endif
    INTEGER, PARAMETER :: LU = 3

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
    END TYPE

CONTAINS

    SUBROUTINE WRITE_LOG_BUFFER(this)

        IMPLICIT NONE
        CLASS(LOG_BUFFER), INTENT(INOUT) :: this

        TYPE(LOG_ITEM), POINTER :: current
        INTEGER :: max_len, tmp_len
        CHARACTER(163) :: line 
         
# if defined (PARALLEL)
        ! Do nothing if not master node
        IF (myid.NE.0 ) RETURN
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
            CALL WRITE_ALL(TRIM(line))
            current => current % next
        END DO

        CALL this % CLEAR()

    END SUBROUTINE 

    SUBROUTINE CLEAR_LOG_BUFFER(this)

        IMPLICIT NONE
        CLASS(LOG_BUFFER), INTENT(INOUT) :: this

        TYPE(LOG_ITEM), POINTER :: current, tmp

# if defined (PARALLEL)
        ! Do nothing if not master node
        IF (myid.NE.0 ) RETURN 
# endif

        current => this % tail

        DO WHILE(ASSOCIATED(current))
            tmp =>  current
            current => current % prev 
            DEALLOCATE(tmp)
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
        IF (myid.NE.0 ) RETURN
# endif

        ALLOCATE(current)
        current % val_name = ADJUSTL(val_name)
        current % val_str  = ADJUSTL(val_str)

        current % next => NULL()
        IF (.NOT.ASSOCIATED(this % head)) THEN
           current % prev => NULL()
           current % next => NULL()
           this % head => current 
           this % tail => current
        ELSE
           this % tail % next => current
           current % prev => this % tail
           this % tail => current 
        END IF

    END SUBROUTINE

    SUBROUTINE WRITE_ALL(msg)

        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: msg

        SWRITE(*,*) msg
        SWRITE(LU,*) msg
        
    END SUBROUTINE

    SUBROUTINE WRITE_LOG(msg)

        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: msg

        SWRITE(LU,*) msg 

    END SUBROUTINE

    SUBROUTINE WRITE_STOP(msg)

        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: msg 

        SWRITE(LU,*) msg 
# if defined (PARALLEL)
        CALL MPI_FINALIZE(ier)
# else
        STOP
# endif

    END SUBROUTINE 


    SUBROUTINE WRITE_BANNER( title )

        IMPLICIT NONE 

        CHARACTER(LEN=*), INTENT(IN) :: title
        INTEGER         , PARAMETER  :: BannerLength = 30 
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

        SWRITE(LU,*) banner
        SWRITE(*,*) banner

END SUBROUTINE




END MODULE
