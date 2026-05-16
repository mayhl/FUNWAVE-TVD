

#ifndef UNIFIED_BUILD
PROGRAM MASTER

     USE GLOBAL
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif
     IMPLICIT NONE
     REAL(SP) :: tbegin,tend
     INTEGER :: Kgrid,Isubgrid,myvar
     INTEGER :: Ksubgrid_internal,NumInternal
     INTEGER :: i


# if defined (PARALLEL)
     call MPI_INIT ( ier )
     call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ier)
     call MPI_COMM_SIZE(MPI_COMM_WORLD,NumP,ier)
# endif

     CALL READ_INPUT

# if defined (PARALLEL)
     CALL PARALLEL_CARTESIAN
# endif

     CALL INDEX_LOCAL(1)   ! grid(1): mloc,nloc,ibeg, iend, etc.

# if defined (DEBUG)
print*,'index 1',mloc,nloc,kloc
# endif

     CALL ALLOCATE_VARIABLES ! using maximum dimension

# if defined (DEBUG)
print*,'allocate variables with max mloc,nloc,kloc',mloc,nloc,kloc
# endif

     ! read bathymetry
     call read_bathymetry  ! for grid(1) only

# if defined (DEBUG)
print*,'read bathymetry OK'
# endif

     CALL INITIALIZATION   ! for grid(1) only

# if defined (DEBUG)
print*,'init OK with Mloc,Nloc,Kloc=',Mloc,Nloc,Kloc
# endif

     ! generate grids
     call generate_grid   ! for grid(1) only

# if defined (DEBUG)
print*,'generate_grid OK'
# endif


# if defined (COUPLING)
     call read_nesting_file
# if defined (DEBUG)
print*,'read nesting file OK'
# endif

# endif

# if defined (VESSEL)

     CALL VESSEL_INITIAL

# if defined (DEBUG)
print*,'vessel initial OK'
# endif

# endif
  ! end vessel

# if defined (COUPLING)
     TIME=TIME_COUPLING_1
# endif
! time integration

     ! record wall time
     call WALL_TIME_SECS(tbegin)

       CALL SINGLE_GRID_LOOP

# if defined (COUPLING)
     CLOSE(11)
# endif

! close station files
       do i=1,NSTAT
             iunit=100+i
             close(iunit)
       enddo

     ! record wall time at the end
     call WALL_TIME_SECS(tend)

# if defined (PARALLEL)
     if(myid.eq.0) write(*,*) 'Simulation takes',tend-tbegin,'seconds'
     if(myid.eq.0) write(3,*) 'Simulation takes',tend-tbegin,'seconds'
     if (myid.eq.0) WRITE(*,*)'Normal Termination!'
     if (myid.eq.0) WRITE(3,*)'Normal Termination!'
# else
     write(*,*) 'Simulation takes',tend-tbegin,'seconds'
     write(3,*) 'Simulation takes',tend-tbegin,'seconds'
     WRITE(*,*)'Normal Termination!'
     WRITE(3,*)'Normal Termination!'
# endif

# if defined (PARALLEL)
     call MPI_FINALIZE ( ier )
# endif


END PROGRAM MASTER
#endif /* UNIFIED_BUILD */


SUBROUTINE SINGLE_GRID_LOOP
     USE GLOBAL
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif
     IMPLICIT NONE
     integer :: i,j,Istage
     REAL(SP) :: tbegin,tend,myvar
     INTEGER :: Kgrid,Isubgrid
     INTEGER :: Ksubgrid_internal,NumInternal

     ! time integration
     do while (TIME<TOTAL_TIME.and.RUN_STEP<SIM_STEPS)


# if defined (COUPLING)
! fyshi added coupling(with a large domain model) 
! condition option 05/15/2013
         CALL OneWayCoupling
# endif

# if defined (OBSTACLE)
         CALL READ_3D_MASK
# endif

       ! time step     
       call estimate_dt

# if defined (DEBUG)
print*,'estimate_at OK'
# endif


       ! update boundary conditions       
       call update_wave_bc

# if defined (DEBUG)
print*,'update_wave_bc OK'
# endif

       ! update mask
       call update_mask

# if defined (DEBUG)
print*,'update_mask  OK'
# endif


       ! update vars
       call update_vars


# if defined (DEBUG)
print*,'update_vars OK'
# endif

       ! SSP Runge-Kutta time stepping
       do Istage = 1,It_Order

         ! well-balanced source terms
         call source_terms

# if defined (DEBUG)
print*,'source_terms OK'
# endif

         ! fluxes at cell faces
         call fluxes(1)


# if defined (DEBUG)
print*,'fluxes OK'
# endif

         ! update all variables
         call eval_duvw(Istage,1)

# if defined (DEBUG)
print*,'eval_duvw OK'
# endif

         ! sponge layer
         if(SPONGE_ON) then
           call sponge_damping

# if defined (DEBUG)
print*,'sponge_damping OK'
# endif

         endif


         ! turbulence model
         if(VISCOUS_FLOW) then
           call eval_turb(Istage,1)

# if defined (DEBUG)
print*,'eval_turb OK'
# endif

         endif


# if defined (BAROCLINIC)
         ! update salinity
         call eval_sali(Istage,1)

# if defined (DEBUG)
print*,'eval_sali OK'
# endif


         ! update density
         call eval_dens  

# if defined (DEBUG)
print*,'eval_dens OK'
# endif

# endif



# if defined (SEDIMENT)
         if(TIME>=TIM_Sedi) then
           ! settling velocity
           call settling_velocity

           ! update sediment concentration
           call eval_sedi(Istage)

           if(BED_CHANGE.and.TIME>=10.0) then
             ! update bed elevation
             call update_bed(Istage)          
           endif

           if(COUPLE_FS) then
             ! update mixture density
             call eval_dens
           endif
         endif

# if defined (DEBUG)
print*,'sediment  OK'
# endif

# endif

# if defined (COUPLING)
! fyshi added nesting option 
         CALL OneWayCoupling

# if defined (DEBUG)
print*,'OneWayCoupling OK'
# endif


# endif

       enddo

# if defined (BALANCE2D)
       ! evaluate momentum balance in cross-shore
       call eval_balance

# if defined (DEBUG)
print*,'eval_balance OK'
# endif


# endif

       ! wave average quantities
       if(WAVE_AVERAGE_ON) then
         call wave_average

# if defined (DEBUG)
print*,'wave_average OK'
# endif

       endif

       ! screen output
       Screen_Count = Screen_Count+dt
       if(Screen_Count>=Screen_Intv) then
         Screen_Count = Screen_Count-Screen_Intv
         call statistics
       endif

       ! probe output to files
       if(NSTAT>0) then
         Plot_Count_Stat = Plot_Count_Stat+dt
         if(Plot_Count_Stat>=Plot_Intv_Stat) then
           Plot_Count_Stat=Plot_Count_Stat-Plot_Intv_Stat
           call probes
         endif
       endif

       ! output resistance

# if defined (VESSEL_tmp)
  ! need to modify this part if calculating resistance 08/30/2024

! here I duplicated writeout if having station NSTAT>0
         Plot_Count_Stat = Plot_Count_Stat+dt
         if(Plot_Count_Stat>=Plot_Intv_Stat) then
           Plot_Count_Stat=Plot_Count_Stat-Plot_Intv_Stat


# if defined (DEBUG)
print*,'vessel print OK'
# endif

      ! calculate resistance
    ResX = 0.0
    ResY = 0.0
    DO J=Jbeg,Jend
    DO I=Ibeg,Iend

! resistance
       IF(ABS(VesselPressureTotal(I,J))>0.0)THEN       
         DetaX = (ETA(I+1,J)-ETA(I-1,J))/2.0_SP/DX
         DetaY = (ETA(I,J+1)-ETA(I,J-1))/2.0_SP/DY
         ResX=ResX+VesselPressureTotal(I,J)*DetaX
         ResY=ResY+VesselPressureTotal(I,J)*DetaY        
       ENDIF

    ENDDO
    ENDDO

# if defined (DEBUG)
print*,'vessel resistance OK'
# endif


# if defined (PARALLEL)
     call MPI_ALLREDUCE(ResX,myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResX = myvar
     call MPI_ALLREDUCE(ResY,myvar,1,MPI_SP,MPI_SUM,MPI_COMM_WORLD,ier)
     ResY = myvar
# endif


    call writeout_resistance
    endif

# endif
 ! end vessel_tmp


       ! field output to files
       if(TIME>=Plot_Start) then
         Plot_Count = Plot_Count+dt
         if(Plot_Count>=Plot_Intv) then
           Plot_Count=Plot_Count-Plot_Intv
           call preview
         endif
       endif
      
     end do


END SUBROUTINE SINGLE_GRID_LOOP





