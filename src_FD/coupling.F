

# if defined (COUPLING)

SUBROUTINE OneWayCoupling
    USE GLOBAL
    IMPLICIT NONE
    INTEGER :: I,J,K
    REAL(SP) :: tmp1,tmp2
    LOGICAL, SAVE :: FirstReadCoupling = .TRUE.

119      FORMAT(5E16.6)  ! this is a fixed format for I/O
 
! determine time slot

    IF(TIME>TIME_COUPLING_1.AND.TIME>TIME_COUPLING_2) THEN
         TIME_COUPLING_1=TIME_COUPLING_2
         
         READ(11,*,END=120) ! time coupling
         READ(11,*,END=120) TIME_COUPLING_2 
! east
         IF(N_COUPLING_EAST.GT.0)THEN
             READ(11,*,END=120)   ! east

             U_COUPLING_EAST(:,:,1)=U_COUPLING_EAST(:,:,2)
             V_COUPLING_EAST(:,:,1)=V_COUPLING_EAST(:,:,2)
             W_COUPLING_EAST(:,:,1)=W_COUPLING_EAST(:,:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)
             P_COUPLING_EAST(:,:,1)=P_COUPLING_EAST(:,:,2)
             S_COUPLING_EAST(:,:,1)=S_COUPLING_EAST(:,:,2)
             T_COUPLING_EAST(:,:,1)=T_COUPLING_EAST(:,:,2)

             READ(11,119,END=120)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119,END=120)((U_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119,END=120)((V_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119,END=120)((W_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119,END=120)((P_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119,END=120)((S_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119,END=120)((T_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)

            IF (FirstReadCoupling)THEN
             U_COUPLING_EAST(:,:,1)=U_COUPLING_EAST(:,:,2)
             V_COUPLING_EAST(:,:,1)=V_COUPLING_EAST(:,:,2)
             W_COUPLING_EAST(:,:,1)=W_COUPLING_EAST(:,:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)
             P_COUPLING_EAST(:,:,1)=P_COUPLING_EAST(:,:,2)
             S_COUPLING_EAST(:,:,1)=S_COUPLING_EAST(:,:,2)
             T_COUPLING_EAST(:,:,1)=T_COUPLING_EAST(:,:,2)
            ENDIF

         ELSE
             READ(11,*,END=120)   ! east            
         ENDIF
! west
         IF(N_COUPLING_WEST.GT.0)THEN
             READ(11,*,END=120)   ! west

             U_COUPLING_WEST(:,:,1)=U_COUPLING_WEST(:,:,2)
             V_COUPLING_WEST(:,:,1)=V_COUPLING_WEST(:,:,2)
             W_COUPLING_WEST(:,:,1)=W_COUPLING_WEST(:,:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)
             P_COUPLING_WEST(:,:,1)=P_COUPLING_WEST(:,:,2)
             S_COUPLING_WEST(:,:,1)=S_COUPLING_WEST(:,:,2)
             T_COUPLING_WEST(:,:,1)=T_COUPLING_WEST(:,:,2)

             READ(11,119,END=120)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119,END=120)((U_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119,END=120)((V_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119,END=120)((W_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119,END=120)((P_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119,END=120)((S_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119,END=120)((T_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
            IF (FirstReadCoupling)THEN
             U_COUPLING_WEST(:,:,1)=U_COUPLING_WEST(:,:,2)
             V_COUPLING_WEST(:,:,1)=V_COUPLING_WEST(:,:,2)
             W_COUPLING_WEST(:,:,1)=W_COUPLING_WEST(:,:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)
             P_COUPLING_WEST(:,:,1)=P_COUPLING_WEST(:,:,2)
             S_COUPLING_WEST(:,:,1)=S_COUPLING_WEST(:,:,2)
             T_COUPLING_WEST(:,:,1)=T_COUPLING_WEST(:,:,2)
            ENDIF

         ELSE
             READ(11,*,END=120)   ! west            
         ENDIF
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
             READ(11,*,END=120)   ! south

             U_COUPLING_SOUTH(:,:,1)=U_COUPLING_SOUTH(:,:,2)
             V_COUPLING_SOUTH(:,:,1)=V_COUPLING_SOUTH(:,:,2)
             W_COUPLING_SOUTH(:,:,1)=W_COUPLING_SOUTH(:,:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)
             P_COUPLING_SOUTH(:,:,1)=P_COUPLING_SOUTH(:,:,2)
             S_COUPLING_SOUTH(:,:,1)=S_COUPLING_SOUTH(:,:,2)
             T_COUPLING_SOUTH(:,:,1)=T_COUPLING_SOUTH(:,:,2)

             READ(11,119,END=120)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119,END=120)((U_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119,END=120)((V_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119,END=120)((W_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119,END=120)((P_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119,END=120)((S_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119,END=120)((T_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
            IF (FirstReadCoupling)THEN
             U_COUPLING_SOUTH(:,:,1)=U_COUPLING_SOUTH(:,:,2)
             V_COUPLING_SOUTH(:,:,1)=V_COUPLING_SOUTH(:,:,2)
             W_COUPLING_SOUTH(:,:,1)=W_COUPLING_SOUTH(:,:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)
             P_COUPLING_SOUTH(:,:,1)=P_COUPLING_SOUTH(:,:,2)
             S_COUPLING_SOUTH(:,:,1)=S_COUPLING_SOUTH(:,:,2)
             T_COUPLING_SOUTH(:,:,1)=T_COUPLING_SOUTH(:,:,2)
            ENDIF

         ELSE
             READ(11,*,END=120)   ! south            
         ENDIF
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
             READ(11,*,END=120)   ! north

             U_COUPLING_NORTH(:,:,1)=U_COUPLING_NORTH(:,:,2)
             V_COUPLING_NORTH(:,:,1)=V_COUPLING_NORTH(:,:,2)
             W_COUPLING_NORTH(:,:,1)=W_COUPLING_NORTH(:,:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)
             P_COUPLING_NORTH(:,:,1)=P_COUPLING_NORTH(:,:,2)
             S_COUPLING_NORTH(:,:,1)=S_COUPLING_NORTH(:,:,2)
             T_COUPLING_NORTH(:,:,1)=T_COUPLING_NORTH(:,:,2)

             READ(11,119,END=120)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119,END=120)((U_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119,END=120)((V_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119,END=120)((W_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119,END=120)((P_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119,END=120)((S_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119,END=120)((T_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
           IF (FirstReadCoupling)THEN
             U_COUPLING_NORTH(:,:,1)=U_COUPLING_NORTH(:,:,2)
             V_COUPLING_NORTH(:,:,1)=V_COUPLING_NORTH(:,:,2)
             W_COUPLING_NORTH(:,:,1)=W_COUPLING_NORTH(:,:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)
             P_COUPLING_NORTH(:,:,1)=P_COUPLING_NORTH(:,:,2)
             S_COUPLING_NORTH(:,:,1)=S_COUPLING_NORTH(:,:,2)
             T_COUPLING_NORTH(:,:,1)=T_COUPLING_NORTH(:,:,2)
           ENDIF

         ELSE
             READ(11,*,END=120)   ! north            
         ENDIF

         FirstReadCoupling = .FALSE.

    ENDIF  !stime>time_2 and time_1

120 CONTINUE

    tmp2=1.0
    tmp1=ZERO

    IF(TIME>TIME_COUPLING_1)THEN
      IF(TIME_COUPLING_1.EQ.TIME_COUPLING_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=1.0
      ELSE
      tmp2=(TIME_COUPLING_2-TIME) &
            /MAX(SMALL, ABS(TIME_COUPLING_2-TIME_COUPLING_1))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1


! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_WEST)THEN

      DO J=Kstart_WEST,Kend_WEST 
      DO I=1,Nghost
        ETA(I,J)=Z_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +Z_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        D(I,J)=ETA(I,J)+Hc(I,J)
      DO K=1+Nghost,Kglob+Nghost
        U(I,J,K)=U_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +U_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2
        V(I,J,K)=V_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +V_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2  
        W(I,J,K)=W_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +W_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2  
        P(I,J,K)=P_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +P_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2 
        Sali(I,J,K)=S_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +S_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2
        Temp(I,J,K)=T_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,2)*tmp1&
                +T_COUPLING_WEST(J-Nghost+Kshift_WEST,K-Nghost,1)*tmp2

        DU(I,J,K)=D(I,J)*U(I,J,K)
        DV(I,J,K)=D(I,J)*V(I,J,K)
        DW(I,J,K)=D(I,J)*W(I,J,K)     
        Dsali(I,J,K)=D(I,J)*Sali(I,J,K)  
        Dtemp(I,J,K)=D(I,J)*Temp(I,J,K)     

      ENDDO
      ENDDO
      ENDDO


     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_west>0


! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_EAST)THEN
      DO J=Kstart_EAST,Kend_EAST  
      DO I=Iend+1,Iend+Nghost
        ETA(I,J)=Z_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +Z_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        D(I,J)=ETA(I,J)+Hc(I,J)
      DO K=1+Nghost,Kglob+Nghost
        U(I,J,K)=U_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +U_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2
        V(I,J,K)=V_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +V_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2
        W(I,J,K)=W_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +W_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2
        P(I,J,K)=P_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +P_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2
        Sali(I,J,K)=S_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +S_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2
        Temp(I,J,K)=T_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,2)*tmp1&
                +T_COUPLING_EAST(J-Nghost+Kshift_EAST,K-Nghost,1)*tmp2

        DU(I,J,K)=D(I,J)*U(I,J,K)
        DV(I,J,K)=D(I,J)*V(I,J,K)
        DW(I,J,K)=D(I,J)*W(I,J,K)     
        Dsali(I,J,K)=D(I,J)*Sali(I,J,K)  
        Dtemp(I,J,K)=D(I,J)*Temp(I,J,K) 
      ENDDO
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_east>0

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_SOUTH)THEN
      DO I=Kstart_SOUTH,Kend_SOUTH  
      DO J=1,Nghost
        ETA(I,J)=Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,2)*tmp1&
                +Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,1)*tmp2
        D(I,J)=ETA(I,J)+Hc(I,J)
      DO K=1+Nghost,Kglob+Nghost    
        U(I,J,K)=U_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +U_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2
        V(I,J,K)=V_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +V_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2
        W(I,J,K)=W_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +W_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2
        P(I,J,K)=P_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +P_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2
        Sali(I,J,K)=S_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +S_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2
        Temp(I,J,K)=T_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,2)*tmp1&
                +T_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,K-Nghost,1)*tmp2

        DU(I,J,K)=D(I,J)*U(I,J,K)
        DV(I,J,K)=D(I,J)*V(I,J,K)
        DW(I,J,K)=D(I,J)*W(I,J,K)     
        Dsali(I,J,K)=D(I,J)*Sali(I,J,K)  
        Dtemp(I,J,K)=D(I,J)*Temp(I,J,K) 
      ENDDO
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_south>0

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_NORTH)THEN
      DO I=Kstart_NORTH,Kend_NORTH  
      DO J=Jend+1,Jend+Nghost
        ETA(I,J)=Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,2)*tmp1&
                +Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,1)*tmp2
        D(I,J)=ETA(I,J)+Hc(I,J)
      DO K=1+Nghost,Kglob+Nghost       
        U(I,J,K)=U_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +U_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2
        V(I,J,K)=V_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +V_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2
        W(I,J,K)=W_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +W_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2
        P(I,J,K)=P_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +P_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2
        Sali(I,J,K)=S_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +S_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2
        Temp(I,J,K)=T_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,2)*tmp1&
                +T_COUPLING_NORTH(I-Nghost+Kshift_NORTH,K-Nghost,1)*tmp2

        DU(I,J,K)=D(I,J)*U(I,J,K)
        DV(I,J,K)=D(I,J)*V(I,J,K)
        DW(I,J,K)=D(I,J)*W(I,J,K)     
        Dsali(I,J,K)=D(I,J)*Sali(I,J,K)  
        Dtemp(I,J,K)=D(I,J)*Temp(I,J,K) 
      ENDDO
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_north>0

END SUBROUTINE OneWayCoupling
# endif 



 
