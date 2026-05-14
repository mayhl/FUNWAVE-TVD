

SUBROUTINE READ_INPUT
     USE GLOBAL
     USE INPUT_READ
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif
# if defined (INTEL)
     use IFPORT, only: rand
# endif
     IMPLICIT NONE

    CHARACTER(LEN=2) Ngrid
    CHARACTER(LEN=80) SubGrid_FILE
    INTEGER :: itmp1,itmp2,itmp3,itmp4,itmp5
     character(len=80) :: FILE_NAME,TMP_NAME,FILE_NAME_number
     real(SP) :: Segma,Celerity,Wave_Length,Wave_Number,  &
                 Fk,Fkdif,Theta_Calc,Wnumy,tmp,tmp1,DFreq, &
                 Freq_Peak,gam,sa,sb,SumInt,A_Jon
     integer :: line,ierr,Iter,i,j,k
 
     ! log and error file
     open(3,file='LOG.txt')

     ! read from input.txt
     FILE_NAME='input.txt'

     ! title
     CALL READ_STRING(TITLE,FILE_NAME,'TITLE',ierr)
     IF(ierr==1)THEN
# if defined (PARALLEL)
       if(myid.eq.0) write(3,*) 'No TITLE in ', FILE_NAME, 'use default'
# else
       write(3,*) 'No TITLE in ', FILE_NAME, 'use default'
# endif
       TITLE='---TEST RUN---'
     ENDIF
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,*)'---- LOG FILE ---'
     if(myid.eq.0) WRITE(3,*)TITLE
     if(myid.eq.0) WRITE(3,*)'--------------input start --------------'
# else
     WRITE(3,*)'---- LOG FILE ---'
     WRITE(3,*)TITLE
     WRITE(3,*)'--------------input start --------------'
# endif

     ! dimension                                             
     CALL READ_INTEGER(Mglob,FILE_NAME,'Mglob',ierr)
     CALL READ_INTEGER(Nglob,FILE_NAME,'Nglob',ierr)
     CALL READ_INTEGER(Kglob,FILE_NAME,'Kglob',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A7,I5)')'Mglob= ',Mglob
     if(myid.eq.0) WRITE(3,'(A7,I5)')'Nglob= ',Nglob
     if(myid.eq.0) WRITE(3,'(A7,I5)')'Kglob= ',Kglob
# else
     WRITE(3,'(A7,I5)')'Mglob= ',Mglob
     WRITE(3,'(A7,I5)')'Nglob= ',Nglob
     WRITE(3,'(A7,I5)')'Kglob= ',Kglob
# endif

# if defined (PARALLEL)
     ! processor number
     CALL READ_INTEGER(PX,FILE_NAME,'PX',ierr)
     CALL READ_INTEGER(PY,FILE_NAME,'PY',ierr)
     if(myid.eq.0) WRITE(3,'(A4,I5)')'PX= ',PX
     if(myid.eq.0) WRITE(3,'(A4,I5)')'PY= ',PY
     if(PX*PY.ne.NumP) then
       if(myid.eq.0) WRITE(3,'(A6,I5)') 'NumP= ',NumP
       stop
     endif
# endif

     ! grid sizes
     CALL READ_FLOAT(dx,FILE_NAME,'DX',line)
     CALL READ_FLOAT(dy,FILE_NAME,'DY',line)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A4,F8.4)')'DX= ',dx
     if(myid.eq.0) WRITE(3,'(A4,F8.4)')'DY= ',dy
# else
     WRITE(3,'(A4,F8.4)')'DX= ',dx
     WRITE(3,'(A4,F8.4)')'DY= ',dy
# endif

     ! vertical grid option
     call READ_INTEGER(Ivgrd,FILE_NAME,'IVGRD',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A7,I3)')'Ivgrd= ',Ivgrd
# else
     WRITE(3,'(A7,I3)')'Ivgrd= ',Ivgrd
# endif
     IF(Ivgrd==2)THEN
       CALL READ_FLOAT(Grd_R,FILE_NAME,'GRD_R',line)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A7,f5.2)')'Grd_R= ',Grd_R
# else
     WRITE(3,'(A7,f5.2)')'Grd_R= ',Grd_R
# endif
     ENDIF

     ! time step
     CALL READ_FLOAT(dt_ini,FILE_NAME,'DT_INI',line)
     CALL READ_FLOAT(dt_min,FILE_NAME,'DT_MIN',line)
     CALL READ_FLOAT(dt_max,FILE_NAME,'DT_MAX',line)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A8,F8.4)')'DT_INI= ',dt_ini
     if(myid.eq.0) WRITE(3,'(A8,F8.4)')'DT_MIN= ',dt_min
     if(myid.eq.0) WRITE(3,'(A8,F8.4)')'DT_MAX= ',dt_max
# else
     WRITE(3,'(A8,F8.4)')'DT_INI= ',dt_ini
     WRITE(3,'(A8,F8.4)')'DT_MIN= ',dt_min
     WRITE(3,'(A8,F8.4)')'DT_MAX= ',dt_max
# endif

     ! result folder                                     
     CALL READ_STRING(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER= ', RESULT_FOLDER
# else
     WRITE(3,'(A15,A50)')'RESULT_FOLDER= ', RESULT_FOLDER
# endif

     ! simulation steps and time
     call READ_INTEGER(SIM_STEPS,FILE_NAME,'SIM_STEPS',ierr)
     CALL READ_FLOAT(TOTAL_TIME,FILE_NAME,'TOTAL_TIME',line)
     CALL READ_FLOAT(Plot_Start,FILE_NAME,'PLOT_START',line)
     CALL READ_FLOAT(Plot_Intv,FILE_NAME,'PLOT_INTV',line)
     CALL READ_FLOAT(Screen_Intv,FILE_NAME,'SCREEN_INTV',line)
     CALL READ_LOGICAL(HOTSTART,FILE_NAME,'HOTSTART',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A11,I12)')'SIM_STEPS= ', SIM_STEPS
     if(myid.eq.0) WRITE(3,'(A12,F8.2)')'TOTAL_TIME= ', TOTAL_TIME
     if(myid.eq.0) WRITE(3,'(A11,F8.2)')'PLOT_START= ', Plot_Start
     if(myid.eq.0) WRITE(3,'(A11,F8.2)')'PLOT_INTV= ', Plot_Intv
     if(myid.eq.0) WRITE(3,'(A13,F8.2)')'SCREEN_INTV= ', Screen_Intv
     if(myid.eq.0) WRITE(3,'(A11,L4)')'HOTSTART= ',HOTSTART
# else
     WRITE(3,'(A11,I12)')'SIM_STEPS= ', SIM_STEPS
     WRITE(3,'(A12,F8.2)')'TOTAL_TIME= ', TOTAL_TIME
     WRITE(3,'(A11,F8.2)')'PLOT_START= ', Plot_Start
     WRITE(3,'(A11,F8.2)')'PLOT_INTV= ', Plot_Intv
     WRITE(3,'(A13,F8.2)')'SCREEN_INTV= ', Screen_Intv
     WRITE(3,'(A11,L4)')'HOTSTART= ',HOTSTART
# endif

     ! laminar viscosity
     CALL READ_LOGICAL(VISCOUS_FLOW,FILE_NAME,'VISCOUS_FLOW',ierr)
     CALL READ_INTEGER(IVturb,FILE_NAME,'IVTURB',ierr)
     CALL READ_INTEGER(IHturb,FILE_NAME,'IHTURB',ierr)
     CALL READ_FLOAT(Visc,FILE_NAME,'VISCOSITY',ierr)
     CALL READ_FLOAT(Schmidt,FILE_NAME,'Schmidt',ierr)
     CALL READ_FLOAT(Cvs,FILE_NAME,'Cvs',ierr)
     CALL READ_FLOAT(Chs,FILE_NAME,'Chs',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A14,L4)')'VISCOUS_FLOW= ',VISCOUS_FLOW
     if(myid.eq.0) WRITE(3,'(A8,I2)')'IVTURB= ',IVturb
     if(myid.eq.0) WRITE(3,'(A8,I2)')'IHTURB= ',IHturb
     if(myid.eq.0) WRITE(3,'(A11,F8.3)')'VISCOSITY= ',Visc
     if(myid.eq.0) WRITE(3,'(A9,F8.3)')'Schmidt= ',Schmidt
     if(myid.eq.0) WRITE(3,'(A6,F8.3)')'Cvs= ',Cvs
     if(myid.eq.0) WRITE(3,'(A6,F8.3)')'Chs= ',Chs
# else
     WRITE(3,'(A14,L4)')'VISCOUS_FLOW= ',VISCOUS_FLOW
     WRITE(3,'(A7,I2)')'IVTURB= ',IVturb
     WRITE(3,'(A7,I2)')'IHTURB= ',IHturb
     WRITE(3,'(A11,F8.3)')'VISCOSITY= ',Visc
     WRITE(3,'(A9,F8.3)')'Schmidt= ',Schmidt
     WRITE(3,'(A6,F8.3)')'Cvs= ',Cvs
     WRITE(3,'(A6,F8.3)')'Chs= ',Chs
# endif


     ! viscous number
     CALL READ_FLOAT(VISCOUS_NUMBER,FILE_NAME,'VISCOUS_NUMBER',line)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A16,F8.3)')'VISCOUS_NUMBER= ',VISCOUS_NUMBER
# else
     WRITE(3,'(A16,F8.3)')'VISCOUS_NUMBER= ',VISCOUS_NUMBER
# endif

     IF(HOTSTART)THEN
       CALL READ_STRING(Eta_HotStart_File,FILE_NAME,'Eta_HotStart_File',ierr)
       CALL READ_STRING(U_HotStart_File,FILE_NAME,'U_HotStart_File',ierr)
       CALL READ_STRING(V_HotStart_File,FILE_NAME,'V_HotStart_File',ierr)
       CALL READ_STRING(W_HotStart_File,FILE_NAME,'W_HotStart_File',ierr)
       CALL READ_STRING(P_HotStart_File,FILE_NAME,'P_HotStart_File',ierr)
# if defined (BAROCLINIC)
       CALL READ_STRING(Sali_HotStart_File,FILE_NAME,'Sali_HotStart_File',ierr)
       CALL READ_STRING(Temp_HotStart_File,FILE_NAME,'Temp_HotStart_File',ierr)
# endif
      IF(VISCOUS_FLOW)THEN
       CALL READ_STRING(Rho_HotStart_File,FILE_NAME,'Rho_HotStart_File',ierr)
       CALL READ_STRING(TKE_HotStart_File,FILE_NAME,'TKE_HotStart_File',ierr)
       CALL READ_STRING(EPS_HotStart_File,FILE_NAME,'EPS_HotStart_File',ierr)
      ENDIF
# if defined (AIR_PRESSURE)
       CALL READ_STRING(Pressure_HotStart_File,FILE_NAME,'Pmb_HotStart_File',ierr)
# endif

     ENDIF ! end hotstart

# if defined (FROUDE_CAP)
     CALL READ_FLOAT(FROUDECAP,FILE_NAME,'FROUDE_CAP',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A13,F8.3)')'FROUDE_CAP= ',FROUDECAP
# else
     WRITE(3,'(A13,F8.3)')'FROUDE_CAP= ',FROUDECAP
# endif

# endif
! end froude cap

     CALL READ_STRING(Depth_File,FILE_NAME,'DEPTH_FILE',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A14,A20)')'Depth File= ', TRIM(Depth_File)
# else
       WRITE(3,'(A14,A20)')'Depth File= ', TRIM(Depth_File)
# endif

     ! courant number
     CALL READ_FLOAT(CFL,FILE_NAME,'CFL',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A5,F8.3)')'CFL= ',CFL
# else
     WRITE(3,'(A5,F8.3)')'CFL= ',CFL
# endif

     ! minimum depth
     CALL READ_FLOAT(MinDep,FILE_NAME,'MinDep',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A8,F8.3)')'MinDep= ',MinDep
# else
     WRITE(3,'(A8,F8.3)')'MinDep= ',MinDep
# endif

     ! bathymetry     
     CALL READ_STRING(DEPTH_TYPE,FILE_NAME,'DEPTH_TYPE',ierr)
     CALL READ_LOGICAL(ANA_BATHY,FILE_NAME,'ANA_BATHY',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_TYPE= ',DEPTH_TYPE
     if(myid.eq.0) WRITE(3,'(A11,L4)')'ANA_BATHY= ',ANA_BATHY
# else
     WRITE(3,'(A12,A50)')'DEPTH_TYPE= ',DEPTH_TYPE
     WRITE(3,'(A11,L4)')'ANA_BATHY= ',ANA_BATHY
# endif

     ! bottom roughness
     CALL READ_INTEGER(Ibot,FILE_NAME,'Ibot',ierr)
     CALL READ_FLOAT(Cd0,FILE_NAME,'Cd0',ierr)
     CALL READ_FLOAT(Zob,FILE_NAME,'Zob',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A6,I2)')'Ibot= ',Ibot
     if(myid.eq.0) WRITE(3,'(A5,F8.5)')'Cd0= ',Cd0
     if(myid.eq.0) WRITE(3,'(A5,F8.5)')'Zob= ',Zob
# else
     WRITE(3,'(A6,I2)')'Ibot= ',Ibot
     WRITE(3,'(A5,F8.5)')'Cd0= ',Cd0
     WRITE(3,'(A5,F8.5)')'Zob= ',Zob
# endif

     ! barotropic or baroclinic
     CALL READ_LOGICAL(BAROTROPIC,FILE_NAME,'BAROTROPIC',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A12,L4)')'BAROTROPIC= ',BAROTROPIC
# else
     WRITE(3,'(A12,L4)')'BAROTROPIC= ',BAROTROPIC
# endif

     ! numerical scheme
     CALL READ_STRING(HIGH_ORDER,FILE_NAME,'HIGH_ORDER',ierr)
     CALL READ_STRING(TIME_ORDER,FILE_NAME,'TIME_ORDER',ierr)
     CALL READ_STRING(CONVECTION,FILE_NAME,'CONVECTION',ierr)
     CALL READ_LOGICAL(ADV_HLLC,FILE_NAME,'HLLC',ierr)
     IF(ierr==1)THEN
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER', 'NOT DEFINED, USE DEFAULT'
# else
       WRITE(3,'(A12,A50)')'HIGH_ORDER', 'NOT DEFINED, USE DEFAULT'
# endif
       HIGH_ORDER='SECOND'
     ENDIF
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER= ', HIGH_ORDER
# else
     WRITE(3,'(A12,A50)')'HIGH_ORDER= ', HIGH_ORDER
# endif
     IF(ierr==1)THEN
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A12,A50)')'TIME_ORDER', 'NOT DEFINED, USE DEFAULT'
# else
       WRITE(3,'(A12,A50)')'TIME_ORDER', 'NOT DEFINED, USE DEFAULT'
# endif
       TIME_ORDER='THIRD'
     ENDIF
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A12,A50)')'TIME_ORDER= ', TIME_ORDER
# else
     WRITE(3,'(A12,A50)')'TIME_ORDER= ', TIME_ORDER
# endif

     ! ramp up the simulation
     CALL READ_FLOAT(TRamp,FILE_NAME,'TRAMP',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A7,E12.3)')'TRAMP= ',TRamp
# else
     WRITE(3,'(A7,E12.3)')'TRAMP= ',TRamp
# endif

     ! if non-hydrostatic simulation
     CALL READ_LOGICAL(NON_HYDRO,FILE_NAME,'NON_HYDRO',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A11,L4)')'NON_HYDRO= ',NON_HYDRO
# else
     WRITE(3,'(A11,L4)')'NON_HYDRO= ',NON_HYDRO
# endif

     ! poisson solver
     CALL READ_INTEGER(isolver,FILE_NAME,'ISOLVER',ierr)
     CALL READ_INTEGER(itmax,FILE_NAME,'ITMAX',ierr)
     CALL READ_FLOAT(tol,FILE_NAME,'TOL',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A9,I2)')'ISOLVER= ',isolver
     if(myid.eq.0) WRITE(3,'(A7,I5)')'ITMAX= ',itmax
     if(myid.eq.0) WRITE(3,'(A5,E12.3)')'TOL= ',tol
# else
     WRITE(3,'(A9,I2)')'ISOLVER= ',isolver
     WRITE(3,'(A7,I5)')'ITMAX= ',itmax
     WRITE(3,'(A5,E12.3)')'TOL= ',tol
# endif

     ! periodic bc
     CALL READ_LOGICAL(PERIODIC_X,FILE_NAME,'PERIODIC_X',ierr)
     CALL READ_LOGICAL(PERIODIC_Y,FILE_NAME,'PERIODIC_Y',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A12,L4)')'PERIODIC_X= ',PERIODIC_X
     if(myid.eq.0) WRITE(3,'(A12,L4)')'PERIODIC_Y= ',PERIODIC_Y
# else
     WRITE(3,'(A12,L4)')'PERIODIC_X= ',PERIODIC_X
     WRITE(3,'(A12,L4)')'PERIODIC_Y= ',PERIODIC_Y
# endif

     ! boundary type
     CALL READ_INTEGER(Bc_X0,FILE_NAME,'BC_X0',ierr)
     CALL READ_INTEGER(Bc_Xn,FILE_NAME,'BC_Xn',ierr)
     CALL READ_INTEGER(Bc_Y0,FILE_NAME,'BC_Y0',ierr)
     CALL READ_INTEGER(Bc_Yn,FILE_NAME,'BC_Yn',ierr)
     CALL READ_INTEGER(Bc_Z0,FILE_NAME,'BC_Z0',ierr)
     CALL READ_INTEGER(Bc_Zn,FILE_NAME,'BC_Zn',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_X0= ',Bc_X0
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_Xn= ',Bc_Xn
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_Y0= ',Bc_Y0
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_Yn= ',Bc_Yn
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_Z0= ',Bc_Z0
     if(myid.eq.0) WRITE(3,'(A7,I2)')'BC_Zn= ',Bc_Zn
# else
     WRITE(3,'(A7,I2)')'BC_X0= ',Bc_X0
     WRITE(3,'(A7,I2)')'BC_Xn= ',Bc_Xn
     WRITE(3,'(A7,I2)')'BC_Y0= ',Bc_Y0
     WRITE(3,'(A7,I2)')'BC_Yn= ',Bc_Yn
     WRITE(3,'(A7,I2)')'BC_Z0= ',Bc_Z0
     WRITE(3,'(A7,I2)')'BC_Zn= ',Bc_Zn
# endif

     ! wavemaker 
     CALL READ_STRING(WaveMaker,FILE_NAME,'WAVEMAKER',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A11,A50)')'WAVEMAKER= ', WAVEMAKER
# else
     WRITE(3,'(A11,A50)')'WAVEMAKER= ', WAVEMAKER
# endif

! absorbing generating wavemaker
     IF(WaveMaker(1:3)=='ABS') THEN
       CALL READ_STRING(Wave_Comp_File,FILE_NAME,'LinearWaveSerFile',ierr)
       OPEN(1,FILE=TRIM(Wave_Comp_File))
         READ(1,*) ! title line
         READ(1,*) Dep_Ser
         READ(1,*) U_FLOW_LEFT,U_FLOW_RIGHT
         READ(1,*) NumCompSer
         ALLOCATE(Amp_Ser(NumCompSer),Per_Ser(NumCompSer),&
                  Phase_Ser(NumCompSer),Theta_Ser(NumCompSer),&
                  Segma_Ser(NumCompSer),Wave_Number_Ser(NumCompSer),&
                  Stokes_Drift_Ser(NumCompSer))
         DO I=1,NumCompSer
           READ(1,*)Amp_Ser(I),Per_Ser(I),Phase_Ser(I),Theta_Ser(I)
           IF(Per_Ser(I)==ZERO)THEN
             WRITE(*,*)'input wave frequency is zero, stop'
             STOP
           ELSE
             Per_Ser(I)=1.0_SP/Per_Ser(I)
           ENDIF
         ENDDO
       CLOSE(1)
     
       CALL READ_FLOAT(WaveMaker_West_Width,FILE_NAME,'WaveMaker_West_Width',ierr)
       CALL READ_FLOAT(WaveMaker_East_Width,FILE_NAME,'WaveMaker_East_Width',ierr)
       CALL READ_FLOAT(R_WaveMaker,FILE_NAME,'R_WaveMaker',ierr)
       CALL READ_FLOAT(A_WaveMaker,FILE_NAME,'A_WaveMaker',ierr)

   ENDIF  ! end abs wavemaker


     IF(WaveMaker(1:3)=='LEF'.or.WaveMaker(1:3)=='INT'.or. &
       WaveMaker(1:3)=='FLU')THEN
       CALL READ_FLOAT(Amp_Wave,FILE_NAME,'AMP',ierr)
       CALL READ_FLOAT(Per_Wave,FILE_NAME,'PER',ierr)
       CALL READ_FLOAT(Dep_Wave,FILE_NAME,'DEP',ierr)
       CALL READ_FLOAT(Theta_Wave,FILE_NAME,'THETA',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A9,F6.3)')'AMP_WAVE= ', Amp_Wave
       if(myid.eq.0) WRITE(3,'(A9,F6.3)')'PER_WAVE= ', Per_Wave
       if(myid.eq.0) WRITE(3,'(A9,F6.3)')'DEP_WAVE= ', Dep_Wave
       if(myid.eq.0) WRITE(3,'(A12,F6.3)')'THETA_WAVE= ', Theta_Wave
# else
       WRITE(3,'(A9,F6.3)')'AMP_WAVE= ', Amp_Wave
       WRITE(3,'(A9,F6.3)')'PER_WAVE= ', Per_Wave
       WRITE(3,'(A9,F6.3)')'DEP_WAVE= ', Dep_Wave
       WRITE(3,'(A12,F6.3)')'THETA_WAVE= ', Theta_Wave
# endif

       IF(WaveMaker(1:3)=='INT') then
         CALL READ_FLOAT(Xsource_West,FILE_NAME,'Xsource_West',ierr)
         CALL READ_FLOAT(Xsource_East,FILE_NAME,'Xsource_East',ierr)
         CALL READ_FLOAT(Ysource_Suth,FILE_NAME,'Ysource_Suth',ierr)
         CALL READ_FLOAT(Ysource_Nrth,FILE_NAME,'Ysource_Nrth',ierr)
# if defined (PARALLEL)
         if(myid.eq.0) WRITE(3,'(A14,F6.3)')'Xsource_West= ',Xsource_West
         if(myid.eq.0) WRITE(3,'(A14,F6.3)')'Xsource_East= ',Xsource_East
         if(myid.eq.0) WRITE(3,'(A14,F6.3)')'Ysource_Suth= ',Ysource_Suth
         if(myid.eq.0) WRITE(3,'(A14,F6.3)')'Ysource_Nrth= ',Ysource_Nrth
# else
         WRITE(3,'(A14,F6.3)')'Xsource_West= ',Xsource_West 
         WRITE(3,'(A14,F6.3)')'Xsource_East= ',Xsource_East
         WRITE(3,'(A14,F6.3)')'Ysource_Suth= ',Ysource_Suth
         WRITE(3,'(A14,F6.3)')'Ysource_Nrth= ',Ysource_Nrth
# endif
       ENDIF

       ! test periodicity
       IF(PERIODIC_Y.and.Theta_Wave.ne.Zero) then
         ! find wave number
         Segma = 2.0*pi/Per_Wave
         Celerity = sqrt(Grav*Dep_Wave)
         Wave_Length = Celerity*Per_Wave
         Wave_Number = 2.0*pi/Wave_Length

         Iter = 0
 75      Fk = Grav*Wave_Number*tanh(Wave_Number*Dep_Wave)-Segma**2
         if(abs(Fk)<=1.0e-8.or.Iter>1000) goto 85
         Fkdif = Grav*Wave_Number*Dep_Wave*(1.0-tanh(Wave_Number*Dep_Wave)**2)+  &                
            Grav*tanh(Wave_Number*Dep_Wave)
         Wave_Number = Wave_Number-Fk/Fkdif
         Iter = Iter+1
         goto 75
 85      continue

         if(Theta_Wave>Zero) then
           ! find right angle for periodic bc
           tmp = Large       
           do Iter = 1,10000
             Wnumy = Iter*2.0*pi/(Nglob*dy)
             if(WnumY<Wave_Number) then
               ! theta based on Ky = K*sin(theta)
               tmp1 = asin(Wnumy/Wave_Number)*180./pi
               if(abs(tmp1-Theta_Wave)<tmp) then
                 tmp = abs(tmp1-Theta_Wave)
                 Theta_Calc = tmp1
               endif
             endif
           enddo
         elseif(Theta_Wave<Zero) then
           ! find right angle for periodic bc 
           tmp = Large
           do Iter = 1,10000
             Wnumy = Iter*2.0*pi/(Nglob*dy)
             if(WnumY<Wave_Number) then
               ! theta based on Ky = K*sin(theta)
               tmp1 = -asin(Wnumy/Wave_Number)*180./pi
               if(abs(tmp1-Theta_Wave)<tmp) then
                 tmp = abs(tmp1-Theta_Wave)
                 Theta_Calc = tmp1
               endif
             endif
           enddo
         endif

# if defined (PARALLEL)
         if(myid.eq.0) then
           write(3,'(A20,F6.3)') 'Wave angle you set= ',Theta_Wave         
           write(3,'(A28,F6.3)') 'Wave angle for periodic bc= ',Theta_Calc
         endif
# else
         write(3,'(A20,F6.3)') 'Wave angle you set= ',Theta_Wave                                                      
         write(3,'(A28,F6.3)') 'Wave angle for periodic bc= ',Theta_Calc
# endif
         Theta_Wave = Theta_Calc
       ENDIF
     ENDIF

     ! random wave, read in 2d spectrum
     IF(WaveMaker(5:7)=='SPC') then
       open(14,file='spc2d.txt')
       read(14,*) NumFreq,NumDir
       if(NumFreq>MaxNumFreq) then
# if defined (PARALLEL)
         if(myid.eq.0) then
           write(3,'(A)') 'Please set a larger MaxNumFreq in mod_glob.F'
           stop
         endif
# else
         write(3,'(A)') 'Please set a larger MaxNumFreq in mod_glob.F'
         stop
# endif
       endif
       if(NumDir>MaxNumDir) then
# if defined (PARALLEL)
         if(myid.eq.0) then
           write(3,'(A)') 'Please set a larger MaxNumDir in mod_glob.F'
           stop
         endif
# else
         write(3,'(A)') 'Please set a larger MaxNumDir in mod_glob.F'
         stop
# endif
       endif
       do i = 1,NumFreq
         read(14,*) Freq(i)
       enddo
       do i = 1,NumDir
         read(14,*) Dire(i)
       enddo
       do j = 1,NumFreq
       do i = 1,NumDir
         read(14,*) Wave_Spc2d(i,j)
       enddo
       enddo
       close(14)

       ! random phase for each component
       do j = 1,NumFreq
       do i = 1,NumDir
# if defined (INTEL)
         Random_Phs(i,j) = rand()*2.0*pi
# else
         Random_Phs(i,j) = rand(0)*2.0*pi
# endif
       enddo
       enddo
     ENDIF

     ! JONSWAP spectrum
     if(WaveMaker(5:7)=='JON') then
       CALL READ_FLOAT(Hm0,FILE_NAME,'Hm0',ierr)
       CALL READ_FLOAT(Tp,FILE_NAME,'Tp',ierr)
       CALL READ_FLOAT(Freq_Min,FILE_NAME,'Freq_Min',ierr)
       CALL READ_FLOAT(Freq_Max,FILE_NAME,'Freq_Max',ierr)
       CALL READ_INTEGER(NumFreq,FILE_NAME,'NumFreq',ierr) 

       ! frequency
       DFreq = (Freq_Max-Freq_Min)/NumFreq
       do i = 1,NumFreq
         Freq(i) = Freq_Min+0.5*DFreq+(i-1)*DFreq
       enddo

       ! jonswap spectrum
       gam = 3.3; sa = 0.07; sb = 0.09
       Freq_Peak = 1.0/Tp
       do i = 1,NumFreq
         if(Freq(i)<Freq_Peak) then
           Jon_Spc(i) = Grav**2/Freq(i)**5*exp(-1.25*(Freq_Peak/Freq(i))**4)*  &
               gam**(-0.5*(Freq(i)/Freq_Peak-1.0)**2/sa**2)
         else
           Jon_Spc(i) = Grav**2/Freq(i)**5*exp(-1.25*(Freq_Peak/Freq(i))**4)*  &
               gam**(-0.5*(Freq(i)/Freq_Peak-1.0)**2/sb**2)
         endif
       enddo
         
       ! make sure m0=Hm0**2/16=int S(f)df
       SumInt = Zero
       do i = 1,NumFreq
         SumInt = SumInt+Jon_Spc(i)*DFreq
       enddo
       A_Jon = Hm0**2/16.0/SumInt

       do i = 1,NumFreq
         Jon_Spc(i) = Jon_Spc(i)*A_Jon
         RanPhs(i) = rand()*2.0*pi
       enddo
     endif

     ! sponge layer
     CALL READ_LOGICAL(SPONGE_ON,FILE_NAME,'SPONGE_ON',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A11,L4)')'SPONGE_ON= ', SPONGE_ON
# else
     WRITE(3,'(A11,L4)')'SPONGE_ON= ', SPONGE_ON
# endif
     IF(SPONGE_ON)THEN
       CALL READ_FLOAT(Sponge_West_Width,FILE_NAME,'Sponge_West_Width',ierr)
       CALL READ_FLOAT(Sponge_East_Width,FILE_NAME,'Sponge_East_Width',ierr)
       CALL READ_FLOAT(Sponge_South_Width,FILE_NAME,'Sponge_South_Width',ierr)
       CALL READ_FLOAT(Sponge_North_Width,FILE_NAME,'Sponge_North_Width',ierr)
       CALL READ_FLOAT(R_Sponge,FILE_NAME,'R_Sponge',ierr)
       CALL READ_FLOAT(A_Sponge,FILE_NAME,'A_Sponge',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A19,F6.3)')'Sponge_West_Width= ', Sponge_West_Width
       if(myid.eq.0) WRITE(3,'(A19,F6.3)')'Sponge_East_Width= ', Sponge_East_Width
       if(myid.eq.0) WRITE(3,'(A20,F6.3)')'Sponge_South_Width= ', Sponge_South_Width
       if(myid.eq.0) WRITE(3,'(A20,F6.3)')'Sponge_North_Width= ', Sponge_North_Width
       if(myid.eq.0) WRITE(3,'(A10,F6.3)')'R_Sponge= ', R_Sponge
       if(myid.eq.0) WRITE(3,'(A10,F6.3)')'A_Sponge= ', A_Sponge
# else
       WRITE(3,'(A19,F6.3)')'Sponge_West_Width= ', Sponge_West_Width
       WRITE(3,'(A19,F6.3)')'Sponge_East_Width= ', Sponge_East_Width
       WRITE(3,'(A20,F6.3)')'Sponge_South_Width= ', Sponge_South_Width
       WRITE(3,'(A20,F6.3)')'Sponge_North_Width= ', Sponge_North_Width
       WRITE(3,'(A10,F6.3)')'R_Sponge= ', R_Sponge
       WRITE(3,'(A10,F6.3)')'A_Sponge= ', A_Sponge
# endif
     ENDIF

     ! wave average control
     CALL READ_LOGICAL(WAVE_AVERAGE_ON,FILE_NAME,'WAVE_AVERAGE_ON',ierr)
     CALL READ_FLOAT(Wave_Ave_Start,FILE_NAME,'WAVE_AVERAGE_START',ierr)
     CALL READ_FLOAT(Wave_Ave_End,FILE_NAME,'WAVE_AVERAGE_END',ierr)
     CALL READ_INTEGER(WaveheightID,FILE_NAME,'WaveheightID',ierr)

# if defined (SEDIMENT)
     ! sediment parameters
     CALL READ_STRING(Sed_Type,FILE_NAME,'Sed_Type',ierr)
     CALL READ_STRING(Sed_Load,FILE_NAME,'Sed_Load',ierr)
     CALL READ_LOGICAL(COUPLE_FS,FILE_NAME,'COUPLE_FS',ierr) 
     CALL READ_FLOAT(SD50,FILE_NAME,'D50',ierr)
     CALL READ_FLOAT(Shields_c,FILE_NAME,'Shields_c',ierr)
     CALL READ_FLOAT(Af,FILE_NAME,'Af',ierr)
     CALL READ_FLOAT(Tau_ce,FILE_NAME,'Tau_ce',ierr)
     CALL READ_FLOAT(Tau_cd,FILE_NAME,'Tau_cd',ierr)
     CALL READ_FLOAT(Erate,FILE_NAME,'Erate',ierr)
     CALL READ_FLOAT(Mud_Visc,FILE_NAME,'Mud_Visc',ierr)
     CALL READ_FLOAT(Tim_Sedi,FILE_NAME,'Tim_Sedi',ierr)
     CALL READ_LOGICAL(BED_CHANGE,FILE_NAME,'BED_CHANGE',ierr)
# endif


# if defined (OBSTACLE)
     CALL READ_STRING(MASK3D_File,FILE_NAME,'MASK3D_FILE',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A14,A20)')'MASK3D File= ', TRIM(MASK3D_File)
# else
       WRITE(3,'(A14,A20)')'MASK3D File= ', TRIM(MASK3D_File)
# endif
# endif

     ! if there is external forcing
     CALL READ_LOGICAL(EXTERNAL_FORCING,FILE_NAME,'EXTERNAL_FORCING',ierr)

     ! probe output stations fyshi update 09/23/2024


     CALL READ_INTEGER(NSTAT,FILE_NAME,'NSTAT',ierr)
     CALL READ_FLOAT(Plot_Intv_Stat,FILE_NAME,'PLOT_INTV_STAT',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A7,I3)')'NSTAT= ', NSTAT
       if(myid.eq.0) WRITE(3,'(A19,F6.3)')'Plot_Intv_Stat= ', Plot_Intv_Stat
# else
       WRITE(3,'(A7,I3)')'NSTAT= ', NSTAT
       WRITE(3,'(A19,F6.3)')'Plot_Intv_Stat= ', Plot_Intv_Stat
# endif

     if(NSTAT>0) then
      ALLOCATE (xstat(NSTAT),ystat(NSTAT))
      CALL READ_STRING(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',ierr)

       FDIR = TRIM(RESULT_FOLDER)
       FILE_NAME_number = ' '
       open(15,file=TRIM(STATIONS_FILE))
       do i = 1,NSTAT
         read(15,*) xstat(i),ystat(i)
         write(file_name_number(1:4),'(I4.4)') i
         TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME_number)
         iunit=100+i
         open(iunit,FILE=TMP_NAME)

       enddo
       close(15)
     endif


     ! output parameters  
     CALL READ_LOGICAL(OUT_DEP,FILE_NAME,'OUT_DEP',ierr)
     CALL READ_LOGICAL(OUT_ETA,FILE_NAME,'OUT_ETA',ierr)
     CALL READ_LOGICAL(OUT_U,FILE_NAME,'OUT_U',ierr)
     CALL READ_LOGICAL(OUT_V,FILE_NAME,'OUT_V',ierr)
     CALL READ_LOGICAL(OUT_W,FILE_NAME,'OUT_W',ierr)
     CALL READ_LOGICAL(OUT_P,FILE_NAME,'OUT_P',ierr)
     CALL READ_LOGICAL(OUT_TKE,FILE_NAME,'OUT_TKE',ierr)
     CALL READ_LOGICAL(OUT_EPS,FILE_NAME,'OUT_EPS',ierr)
     CALL READ_LOGICAL(OUT_S,FILE_NAME,'OUT_S',ierr)
     CALL READ_LOGICAL(OUT_MU,FILE_NAME,'OUT_MU',ierr)
     CALL READ_LOGICAL(OUT_BUB,FILE_NAME,'OUT_BUB',ierr)
     CALL READ_LOGICAL(OUT_A,FILE_NAME,'OUT_A',ierr)
     CALL READ_LOGICAL(OUT_F,FILE_NAME,'OUT_F',ierr)
     CALL READ_LOGICAL(OUT_T,FILE_NAME,'OUT_T',ierr)
     CALL READ_LOGICAL(OUT_G,FILE_NAME,'OUT_G',ierr)
     CALL READ_LOGICAL(OUT_SALI,FILE_NAME,'OUT_SALI',ierr)
     CALL READ_LOGICAL(OUT_TEMP,FILE_NAME,'OUT_TEMP',ierr)
     CALL READ_LOGICAL(OUT_RHO,FILE_NAME,'OUT_RHO',ierr)


# if defined (COUPLING)
      CALL READ_STRING(COUPLING_FILE,FILE_NAME,'COUPLING_FILE',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# else
      WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# endif
# endif

# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_DEP= ',OUT_DEP
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_ETA= ',OUT_ETA
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_U= ',OUT_U
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_V= ',OUT_V
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_W= ',OUT_W
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_P= ',OUT_P
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_TKE= ',OUT_TKE
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_EPS= ',OUT_EPS
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_S= ',OUT_S
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_MU= ',OUT_MU
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_BUB= ',OUT_BUB
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_A= ',OUT_A
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_F= ',OUT_F
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_T= ',OUT_T
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_G= ',OUT_G
     if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_SALI= ',OUT_SALI
    if(myid.eq.0) WRITE(3,'(A10,L4)') 'OUT_TEMP= ',OUT_TEMP
# else
     WRITE(3,'(A10,L4)') 'OUT_DEP= ',OUT_DEP
     WRITE(3,'(A10,L4)') 'OUT_ETA= ',OUT_ETA
     WRITE(3,'(A10,L4)') 'OUT_U= ',OUT_U
     WRITE(3,'(A10,L4)') 'OUT_V= ',OUT_V
     WRITE(3,'(A10,L4)') 'OUT_W= ',OUT_W
     WRITE(3,'(A10,L4)') 'OUT_P= ',OUT_P
     WRITE(3,'(A10,L4)') 'OUT_TKE= ',OUT_TKE
     WRITE(3,'(A10,L4)') 'OUT_EPS= ',OUT_EPS
     WRITE(3,'(A10,L4)') 'OUT_S= ',OUT_S
     WRITE(3,'(A10,L4)') 'OUT_MU= ',OUT_MU
     WRITE(3,'(A10,L4)') 'OUT_BUB= ',OUT_BUB
     WRITE(3,'(A10,L4)') 'OUT_A= ',OUT_A
     WRITE(3,'(A10,L4)') 'OUT_F= ',OUT_F
     WRITE(3,'(A10,L4)') 'OUT_T= ',OUT_T
     WRITE(3,'(A10,L4)') 'OUT_G= ',OUT_G
     WRITE(3,'(A10,L4)') 'OUT_SALI= ',OUT_SALI
     WRITE(3,'(A10,L4)') 'OUT_TEMP= ',OUT_TEMP
# endif

# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,*)'--------------input end --------------'
# else
     WRITE(3,*)'--------------input end --------------'
# endif


! fyshi separate boundary condition from wavemaker in order to do both wavemaker
! and boundary conditions
     CALL READ_STRING(BOUNDARY,FILE_NAME,'BOUNDARY',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A11,A50)')'BOUNDARY= ', BOUNDARY
# else
     WRITE(3,'(A11,A50)')'BOUNDARY= ', BOUNDARY
# endif

     IF(BOUNDARY(1:10)=='TID_FLX_LR')THEN
       BC_X0 = 3
       BC_Xn = 3
     ELSEIF(BOUNDARY(1:10)=='TID_ELE_LR')THEN
       BC_X0 = 1
       BC_Xn = 1
     ENDIF
     IF(BOUNDARY(1:10)=='TID_FLX_LR'.OR.  &
        BOUNDARY(1:10)=='TID_ELE_LR') THEN

       CALL READ_STRING(BoundaryFile,FILE_NAME,'BoundaryFile',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A14,A20)')'BoundaryFile= ', TRIM(BoundaryFile)
# else
       WRITE(3,'(A14,A20)')'BoundaryFile= ', TRIM(BoundaryFile)
# endif

       OPEN(2,FILE=TRIM(BoundaryFile))
         READ(2,*)WHAT
         READ(2,*)NumTimeData
         ALLOCATE (TimeData(NumTimeData),  &
                   DataU_L(NumTimeData), DataU_R(NumTimeData), DataSal_R(NumTimeData),&   
                   DataEta_L(NumTimeData), DataEta_R(NumTimeData),DataSal_L(NumTimeData),&
                   DataTem_L(NumTimeData),DataTem_R(NumTimeData) )   
         ALLOCATE (Z_pct_West(Kglob),Z_pct_East(Kglob))
         READ(2,*)WHAT ! z percentage of flux
         READ(2,*)WHAT ! west side
         READ(2,*)(Z_pct_West(I),I=1,Kglob)
         READ(2,*)WHAT ! east side
         READ(2,*)(Z_pct_East(I),I=1,Kglob)   
         READ(2,*)WHAT ! time series
         DO I=1,NumTimeData
           READ(2,*,END=111) TimeData(I)
           READ(2,*)WHAT ! west side    
           READ(2,*,END=111) DataEta_L(I),DataU_L(I),DataSal_L(I), &
                                         DataTem_L(I)
           READ(2,*)WHAT ! east side 
           READ(2,*,END=111) DataEta_R(I),DataU_R(I),DataSal_R(I), &
                                         DataTem_R(I)
         ENDDO
111      CONTINUE
# if defined (PARALLEL)
        IF(myid==0) WRITE(*,*) 'Number of Data: ', I-1
# else
        WRITE(*,*) 'Number of Data: ', I-1
# endif
       CLOSE(2)
     ENDIF ! end flx boundary conditions

# if defined(BAROCLINIC)
     CALL READ_STRING(INI_SALI_INPUT,FILE_NAME,'INI_SALI_INPUT',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A17,A50)')'INI_SALI_INPUT= ', INI_SALI_INPUT
# else
     WRITE(3,'(A17,A50)')'INI_SALI_INPUT= ', INI_SALI_INPUT
# endif

     IF(INI_SALI_INPUT(1:4)=='CONS')THEN
        CALL READ_FLOAT(INI_SALI,FILE_NAME,'INI_SALI',line)
# if defined (PARALLEL)
        if(myid.eq.0) WRITE(3,'(A11,F12.6)')'INI_SALI= ', INI_SALI
# else
         WRITE(3,'(A11,F12.6)')'INI_SALI= ', INI_SALI
# endif
     ELSE IF(INI_SALI_INPUT(1:4)=='DATA')THEN
       CALL READ_STRING(INI_SALI_FILE,FILE_NAME,'INI_SALI_FILE',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A16,A20)')'INI_SALI_FILE= ', TRIM(INI_SALI_FILE)
# else
       WRITE(3,'(A16,A20)')'INI_SALI_FILE= ', TRIM(INI_SALI_FILE)
# endif
     ENDIF ! end read sali
# endif
!  end salinity

# if defined(BAROCLINIC)
     CALL READ_STRING(INI_TEMP_INPUT,FILE_NAME,'INI_TEMP_INPUT',ierr)
# if defined (PARALLEL)
     if(myid.eq.0) WRITE(3,'(A17,A50)')'INI_TEMP_INPUT= ', INI_TEMP_INPUT
# else
     WRITE(3,'(A17,A50)')'INI_TEMP_INPUT= ', INI_TEMP_INPUT
# endif

     IF(INI_TEMP_INPUT(1:4)=='CONS')THEN
        CALL READ_FLOAT(INI_TEMP,FILE_NAME,'INI_TEMP',line)
# if defined (PARALLEL)
        if(myid.eq.0) WRITE(3,'(A11,F12.6)')'INI_TEMP= ', INI_TEMP
# else
         WRITE(3,'(A11,F12.6)')'INI_TEMP= ', INI_TEMP
# endif
     ELSE IF(INI_TEMP_INPUT(1:4)=='DATA')THEN
       CALL READ_STRING(INI_TEMP_FILE,FILE_NAME,'INI_TEMP_FILE',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A16,A20)')'INI_TEMP_FILE= ', TRIM(INI_TEMP_FILE)
# else
       WRITE(3,'(A16,A20)')'INI_TEMP_FILE= ', TRIM(INI_TEMP_FILE)
# endif
     ENDIF ! end read temperatre
# endif
!  end temperature

! fyshi add tidal low pass
     CALL READ_LOGICAL(TID_LOW_PASS,FILE_NAME,'TID_LOW_PASS',ierr)
# if defined (PARALLEL)
       if(myid.eq.0) WRITE(3,'(A15,L4)') 'TID_LOW_PASS= ',TID_LOW_PASS
# else
       WRITE(3,'(A15,L4)') 'TID_LOW_PASS= ',TID_LOW_PASS
# endif



END SUBROUTINE READ_INPUT


     subroutine preview
!--------------------------------------------------- 
!    This subroutine is used to preview
!    Called by                         
!       main 
!--------------------------------------------------
     use global
! fyshi add foam
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif
     implicit none
     integer :: i,j,k,I1,I2,I3,I4
!     character(len=80) :: FDIR=' ' ! move to mod_glob
     character(len=80) ::FILE_NAME=' '
     character(LEN=80) :: file=' '
     real(SP), dimension(Mloc,Nloc) :: Int2Flo
     real(SP), dimension(Mloc,Nloc,Kloc) :: Int2Flo3D

     ! file number
     Icount = Icount+1
   
     ! results directory
     FDIR = TRIM(RESULT_FOLDER)

# if defined (PARALLEL)
     if(myid.eq.0) write(*,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
     if(myid.eq.0) write(3,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME     
# else
     write(*,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
     write(3,102) 'Printing file No.',Icount,' TIME/TOTAL: ',TIME,'/',TOTAL_TIME
# endif
102  FORMAT(A20,I4,A14,F16.3,A2,F16.3)
100  FORMAT(15000E16.6)

     I1 = mod(Icount/1000,10)
     I2 = mod(Icount/100,10)
     I3 = mod(Icount/10,10)
     I4 = mod(Icount,10)

     write(FILE_NAME(1:1),'(I1)') I1
     write(FILE_NAME(2:2),'(I1)') I2
     write(FILE_NAME(3:3),'(I1)') I3
     write(FILE_NAME(4:4),'(I1)') I4

# if defined (PARALLEL)
     if(myid.eq.0) then
# endif
     open(5,file=TRIM(FDIR)//'time',position="append")
     write(5,*) TIME
     close(5)
# if defined (PARALLEL)
     endif
# endif

     if(Icount==1) then
       if(OUT_DEP) then
         file=TRIM(FDIR)//'depth'
         call putfile2D(file,Hc(1:Mloc,1:Nloc))
       endif
     endif

     if(OUT_ETA) then
       file=TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
       call putfile2D(file,Eta(1:Mloc,1:Nloc))
     endif

     if(OUT_U) then
       file=TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
       call putfile3D(file,U(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_V) then
       file=TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
       call putfile3D(file,V(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_W) then
       file=TRIM(FDIR)//'w_'//TRIM(FILE_NAME)
       call putfile3D(file,W(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_P) then
       file=TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
       call putfile3D(file,P(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_TKE) then
       file=TRIM(FDIR)//'tke_'//TRIM(FILE_NAME)
       call putfile3D(file,Tke(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_EPS) then
       file=TRIM(FDIR)//'eps_'//TRIM(FILE_NAME)
       call putfile3D(file,Eps(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_S) then
       file=TRIM(FDIR)//'prod_'//TRIM(FILE_NAME)
       call putfile3D(file,Prod_s(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_MU) then
       file=TRIM(FDIR)//'mu_'//TRIM(FILE_NAME)
       call putfile3D(file,CmuVt(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_A) then
       file=TRIM(FDIR)//'upwp_'//TRIM(FILE_NAME)
       call putfile3D(file,UpWp(1:Mloc,1:Nloc,1:Kloc))
     endif
# if defined (OBSTACLE)
      file=TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
       Int2Flo3D=Mask3D
       call putfile3D(file,Int2Flo3D(1:Mloc,1:Nloc,1:Kloc))
# endif

# if defined (BUBBLE)
     if(OUT_BUB) then
       file=TRIM(FDIR)//'b_'//TRIM(FILE_NAME)
       call putfile3D(file,Vbg(1:Mloc,1:Nloc,1:Kloc))
     endif
# endif

# if defined (SEDIMENT)
     if(OUT_F) then
       file=TRIM(FDIR)//'f_'//TRIM(FILE_NAME)
       call putfile3D(file,Conc(1:Mloc,1:Nloc,1:Kloc))
     endif
     if(OUT_T) then
       file=TRIM(FDIR)//'t_'//TRIM(FILE_NAME)
       call putfile2D(file,Taub(1:Mloc,1:Nloc))
     endif
     if(OUT_G) then
       file=TRIM(FDIR)//'g_'//TRIM(FILE_NAME)
       call putfile2D(file,Bed(1:Mloc,1:Nloc))
     endif
# endif

# if defined (BAROCLINIC)
     if(OUT_SALI) then
       file=TRIM(FDIR)//'sali_'//TRIM(FILE_NAME)
       call putfile3D(file,Sali(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_TEMP) then
       file=TRIM(FDIR)//'temp_'//TRIM(FILE_NAME)
       call putfile3D(file,Temp(1:Mloc,1:Nloc,1:Kloc))
     endif

     if(OUT_RHO) then
       file=TRIM(FDIR)//'rho_'//TRIM(FILE_NAME)
       call putfile3D(file,Rho(1:Mloc,1:Nloc,1:Kloc))
     endif
# endif

! fyshi add foam
# if defined (VESSEL)
     if(OUT_VESSEL) then
       file=TRIM(FDIR)//'Pves_'//TRIM(FILE_NAME)
       call putfile2D(file,VesselPressureTotal(1:Mloc,1:Nloc))
     endif
# endif

     end subroutine preview


# if defined (PARALLEL)

    subroutine putfile2D(file,phi)
    use global
    implicit none
    real(SP),dimension(Mloc,Nloc),intent(in) :: phi
    character(len=80) :: file
    integer,dimension(NumP) :: npxs,npys
    integer,dimension(1) :: req
    real(SP),dimension(Mloc,Nloc) :: xx
    real(SP),dimension(Mglob,Nglob) :: phiglob
    integer,dimension(MPI_STATUS_SIZE,1) :: status
    integer :: i,j,iglob,jglob,len,n

    call MPI_GATHER(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,  &
           0,MPI_COMM_WORLD,ier)
    call MPI_GATHER(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,  &
           0,MPI_COMM_WORLD,ier)

    ! put the data in master processor into the global var
    if(myid==0) then
      do j = Jbeg,Jend
      do i = Ibeg,Iend
        iglob = i-Nghost
        jglob = j-Nghost
        phiglob(iglob,jglob) = Phi(i,j)
      enddo
      enddo
    endif

    ! collect data from other processors into the master processor
    len = Mloc*Nloc

    do n = 1,NumP-1
      if(myid==0) then
        call MPI_IRECV(xx,len,MPI_SP,n,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
        do j = Jbeg,Jend
        do i = Ibeg,Iend
          iglob = npxs(n+1)*(Iend-Ibeg+1)+i-Nghost
          jglob = npys(n+1)*(Jend-Jbeg+1)+j-Nghost
          phiglob(iglob,jglob) = xx(i,j)
        enddo
        enddo
      endif

      if(myid==n) then
        call MPI_SEND(phi,len,MPI_SP,0,0,MPI_COMM_WORLD,ier)
      endif
    enddo       

    if(myid==0) then
      open(5,file=TRIM(file))
      do j = 1,Nglob
        write(5,100) (phiglob(i,j),i=1,Mglob)
      enddo
      close(5)
    endif
100 FORMAT(15000f15.5)

    end subroutine putfile2D

    subroutine putfile3D(file,phi)
    use global
    implicit none
    real(SP),dimension(Mloc,Nloc,Kloc),intent(in) :: phi
    character(len=80) :: file
    integer,dimension(NumP) :: npxs,npys
    integer,dimension(1) :: req
    real(SP),dimension(:,:),allocatable :: xx,philoc
    real(SP),dimension(Mglob,Nglob,Kglob) :: phiglob
    integer,dimension(MPI_STATUS_SIZE,1) :: status
    integer :: i,j,k,jk,iglob,jglob,kk,n,len,nreq,NKloc

    call MPI_GATHER(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,  &
          0,MPI_COMM_WORLD,ier)
    call MPI_GATHER(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,  &
          0,MPI_COMM_WORLD,ier)

    NKloc = Nloc*Kloc

    ! put the data in master processor into the global var
    if(myid==0) then
      do k = Kbeg,Kend
      do j = Jbeg,Jend
      do i = Ibeg,Iend
        iglob = i-Nghost
        jglob = j-Nghost
        kk = k-Nghost
        phiglob(iglob,jglob,kk) = Phi(i,j,k)
      enddo
      enddo
      enddo
    endif

    allocate(philoc(Mloc,NKloc))
    allocate(xx(Mloc,NKloc))

    do k = 1,Kloc
    do j = 1,Nloc
    do i = 1,Mloc
      jk = (k-1)*Nloc+j
      philoc(i,jk) = phi(i,j,k)
    enddo
    enddo
    enddo

    ! collect data from other processors into the master processor
    len = Mloc*NKloc

    do n = 1,NumP-1
      if(myid==0) then
        call MPI_IRECV(xx,len,MPI_SP,n,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
        do k = Kbeg,Kend
        do j = Jbeg,Jend
        do i = Ibeg,Iend
          iglob = npxs(n+1)*(Iend-Ibeg+1)+i-Nghost
          jglob = npys(n+1)*(Jend-Jbeg+1)+j-Nghost
          kk = k-Nghost
          jk = (k-1)*Nloc+j
          phiglob(iglob,jglob,kk) = xx(i,jk)
        enddo
        enddo
        enddo
      endif

      if(myid==n) then
        call MPI_SEND(philoc,len,MPI_SP,0,0,MPI_COMM_WORLD,ier)
      endif
    enddo

    if(myid.eq.0) then
      open(5,file=TRIM(file))
      do k = 1,Kglob
      do j = 1,Nglob
        write(5,100) (phiglob(i,j,k),i=1,Mglob)
      enddo
      enddo
      close(5)
    endif
100 FORMAT(15000E16.8)

    deallocate(philoc)
    deallocate(xx)

    end subroutine putfile3D


# else
  ! not parallel


     subroutine putfile2D(file,phi)

     use global
     implicit none
     real(SP),dimension(Mloc,Nloc),intent(in) :: phi
     character(len=80) :: file
     integer :: i,j

     open(5,file=trim(file))
# if defined(DEBUG)
     do j = 1,Nloc
       write(5,100) (phi(i,j),i=1,Mloc)
     enddo
# else
     do j = Jbeg,Jend
       write(5,100) (phi(i,j),i=Ibeg,Iend)
     enddo
# endif 
! end debug
     close(5)
 100 FORMAT(15000f15.5)

     end subroutine putfile2D


     subroutine putfile3D(file,phi)
     use global
     implicit none
     real(SP),dimension(Mloc,Nloc,Kloc),intent(in) :: phi
     character(len=80) :: file
     integer :: i,j,k


     open(5,file=trim(file))
# if defined (DEBUG)
     do k = 1,Kloc
     do j = 1,Nloc
       write(5,100) (phi(i,j,k),i=1,Mloc)
     enddo
     enddo
# else
     do k = Kbeg,Kend
     do j = Jbeg,Jend
       write(5,100) (phi(i,j,k),i=Ibeg,Iend)
     enddo
     enddo
# endif
! end debug
     close(5)
 100 FORMAT(15000f15.5)


     end subroutine putfile3D

# endif
 ! end parallel

     subroutine read_bathymetry
!------------------------------------------------------ 
!    This subroutine is used to read bathymetry                                                
!    Called by  
!       main    
!    Last update: 21/12/2010, Gangfeng Ma      
!-----------------------------------------------------
     use global
     implicit none
     integer :: i,j,m,n,iter,iglob,jglob,II,JJ
     integer :: Maskp(Mglob+1,Nglob+1)
     real(SP), dimension(Mglob+1,Nglob+1) :: HG

     ! read bathymetry at grid points
     if(trim(adjustl(DEPTH_TYPE))=='CELL_GRID') then
       if(ANA_BATHY) then
         do j = 1,Nglob+1
         do i = 1,Mglob+1
           HG(i,j) = 0.5
         enddo
         enddo
       else
! fyshi add standard depth file
         open(5,file=TRIM(Depth_File),status='old')
         do j = 1,Nglob+1
           read(5,*) (HG(i,j),i=1,Mglob+1)
         enddo
       endif

       ! find pernament dry points
       Maskp = 1
       do j = 1,Nglob+1
       do i = 1,Mglob+1
         if(HG(i,j)<-1000.0) Maskp(i,j) = 0
       enddo
       enddo
 
       ! interpolate depth into cell center
       do j = 1,Nglob
       do i = 1,Mglob
         HCG(i,j) = (HG(i,j)*Maskp(i,j)+HG(i+1,j)*Maskp(i+1,j)+  &
             HG(i,j+1)*Maskp(i,j+1)+HG(i+1,j+1)*Maskp(i+1,j+1))/  &
             (Maskp(i,j)+Maskp(i+1,j)+Maskp(i,j+1)+Maskp(i+1,j+1)+1.e-16)
       enddo
       enddo

# if defined (PARALLEL)
       do j = Jbeg,Jend
       do i = Ibeg,Iend
         iglob = npx*(Mloc-2*Nghost)+i-Nghost
         jglob = npy*(Nloc-2*Nghost)+j-Nghost
         Hc(i,j) = HCG(iglob,jglob)
       enddo
       enddo
# else
       do j = Jbeg,Jend
       do i = Ibeg,Iend
         iglob = i-Nghost
         jglob = j-Nghost
         Hc(i,j) = HCG(iglob,jglob)
       enddo
       enddo
# endif

     elseif(trim(adjustl(DEPTH_TYPE))=='CELL_CENTER') then
       ! read bathymetry at cell center
       if(ANA_BATHY) then

       else ! not analytical bathymetry, read from depth file

         open(5,file=TRIM(Depth_File),status='old')
         do j = 1,Nglob
           read(5,*) (HCG(i,j),i=1,Mglob)
         enddo
         close(5)

# if defined (PARALLEL)
         do j = Jbeg,Jend
         do i = Ibeg,Iend
           iglob = npx*(Mloc-2*Nghost)+i-Nghost
           jglob = npy*(Nloc-2*Nghost)+j-Nghost
           Hc(i,j) = HCG(iglob,jglob)
         enddo
         enddo
# else
         do j = Jbeg,Jend
         do i = Ibeg,Iend
           iglob = i-Nghost
           jglob = j-Nghost
           Hc(i,j) = HCG(iglob,jglob)
         enddo
         enddo
# endif

       endif

     endif

     ! find pernament dry cells
     Mask_Struct = 1
     do j = Jbeg,Jend
     do i = Ibeg,Iend
       if(Hc(i,j)<-10.0) then
         Mask_Struct(i,j) = 0
       endif
     enddo
     enddo

     ! collect data into ghost cells
     call phi_2D_coll(Hc)

     ! reconstruct depth at x-y faces
     do j = 1,Nloc
     do i = 2,Mloc
       Hfx(i,j) = 0.5*(Hc(i-1,j)+Hc(i,j))
     enddo
     Hfx(1,j) = Hc(1,j)
     Hfx(Mloc1,j) = Hc(Mloc,j)
     enddo

     do i = 1,Mloc
     do j = 2,Nloc
       Hfy(i,j) = 0.5*(Hc(i,j-1)+Hc(i,j))
     enddo
     Hfy(i,1) = Hc(i,1)
     Hfy(i,Nloc1) = Hc(i,Nloc)
     enddo

     ! derivatives of water depth at cell center
     do j = 1,Nloc
     do i = 1,Mloc
       DelxH(i,j) = (Hfx(i+1,j)-Hfx(i,j))/dx
       DelyH(i,j) = (Hfy(i,j+1)-Hfy(i,j))/dy
     enddo
     enddo

     ! save initial bathymetry
     Hc0 = Hc


     end subroutine read_bathymetry

  

# if defined (COUPLING)
SUBROUTINE READ_NESTING_FILE
!---------------------------------------------------
!    This subroutine is used to read nesting file at the first time
!    Called by 
!       main
!    Last update: 05/15/2013, fyshi
!---------------------------------------------------
     use global
     use input_util
     implicit none
     INTEGER :: I,J,K
       OPEN(11,FILE=TRIM(COUPLING_FILE))
         READ(11,*)  ! title
         READ(11,*)  ! boundary info
! boundary basic info including point number of coupling, start point, etc
! east
         READ(11,*)  ! east
         READ(11,*) N_COUPLING_EAST,J_START_EAST
! west 
         READ(11,*)  ! west
         READ(11,*) N_COUPLING_WEST,J_START_WEST
! south 
         READ(11,*)  ! south
         READ(11,*) N_COUPLING_SOUTH,I_START_SOUTH
! north 
         READ(11,*)  ! north
         READ(11,*) N_COUPLING_NORTH,I_START_NORTH

! read time and variable at the first level

         READ(11,*) ! time start title
         READ(11,*) TIME_COUPLING_1 
! initialize time_2
         TIME_COUPLING_2 = TIME_COUPLING_1

! east
         IF(N_COUPLING_EAST.GT.0)THEN
           ALLOCATE(U_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               V_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               W_COUPLING_EAST(N_COUPLING_EAST,Kglob,2),&
               Z_COUPLING_EAST(N_COUPLING_EAST,2), &
               P_COUPLING_EAST(N_COUPLING_EAST,Kglob,2), &
               S_COUPLING_EAST(N_COUPLING_EAST,Kglob,2), &
               T_COUPLING_EAST(N_COUPLING_EAST,Kglob,2) &
               )
             READ(11,*)   ! east
             READ(11,119)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119)((U_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((V_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((W_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((P_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((S_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
             READ(11,119)((T_COUPLING_EAST(I,J,2),I=1,N_COUPLING_EAST),J=1,Kglob)
!   initialize first step
             U_COUPLING_EAST(:,:,1)=U_COUPLING_EAST(:,:,2)
             V_COUPLING_EAST(:,:,1)=V_COUPLING_EAST(:,:,2)
             W_COUPLING_EAST(:,:,1)=W_COUPLING_EAST(:,:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)
             P_COUPLING_EAST(:,:,1)=P_COUPLING_EAST(:,:,2)
             S_COUPLING_EAST(:,:,1)=S_COUPLING_EAST(:,:,2)
             T_COUPLING_EAST(:,:,1)=T_COUPLING_EAST(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_east
119      FORMAT(5E16.6)

! west
         IF(N_COUPLING_WEST.GT.0)THEN
           ALLOCATE(U_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               V_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               W_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               Z_COUPLING_WEST(N_COUPLING_WEST,2),&
               P_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               S_COUPLING_WEST(N_COUPLING_WEST,Kglob,2),&
               T_COUPLING_WEST(N_COUPLING_WEST,Kglob,2) &
               )
             READ(11,*)   ! west
             READ(11,119)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119)((U_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((V_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((W_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((P_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((S_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
             READ(11,119)((T_COUPLING_WEST(I,J,2),I=1,N_COUPLING_WEST),J=1,Kglob)
!   initialize first step
             U_COUPLING_WEST(:,:,1)=U_COUPLING_WEST(:,:,2)
             V_COUPLING_WEST(:,:,1)=V_COUPLING_WEST(:,:,2)
             W_COUPLING_WEST(:,:,1)=W_COUPLING_WEST(:,:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)
             P_COUPLING_WEST(:,:,1)=P_COUPLING_WEST(:,:,2)
             S_COUPLING_WEST(:,:,1)=S_COUPLING_WEST(:,:,2)
             T_COUPLING_WEST(:,:,1)=T_COUPLING_WEST(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_west
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
           ALLOCATE(U_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               V_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               W_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               Z_COUPLING_SOUTH(N_COUPLING_SOUTH,2),&
               P_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               S_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2),&
               T_COUPLING_SOUTH(N_COUPLING_SOUTH,Kglob,2) &
               )
             READ(11,*)   ! south
             READ(11,119)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119)((U_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((V_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((W_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((P_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((S_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
             READ(11,119)((T_COUPLING_SOUTH(I,J,2),I=1,N_COUPLING_SOUTH),J=1,Kglob)
!   initialize first step
             U_COUPLING_SOUTH(:,:,1)=U_COUPLING_SOUTH(:,:,2)
             V_COUPLING_SOUTH(:,:,1)=V_COUPLING_SOUTH(:,:,2)
             W_COUPLING_SOUTH(:,:,1)=W_COUPLING_SOUTH(:,:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)
             P_COUPLING_SOUTH(:,:,1)=P_COUPLING_SOUTH(:,:,2)
             S_COUPLING_SOUTH(:,:,1)=S_COUPLING_SOUTH(:,:,2)
             T_COUPLING_SOUTH(:,:,1)=T_COUPLING_SOUTH(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_south
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
           ALLOCATE(U_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               V_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               W_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2),&
               Z_COUPLING_NORTH(N_COUPLING_NORTH,2), &
               P_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2), &
               S_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2), &
               T_COUPLING_NORTH(N_COUPLING_NORTH,Kglob,2) &
               )
             READ(11,*)   ! north
             READ(11,119)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119)((U_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((V_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((W_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((P_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((S_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)
             READ(11,119)((T_COUPLING_NORTH(I,J,2),I=1,N_COUPLING_NORTH),J=1,Kglob)

!   initialize first step
             U_COUPLING_NORTH(:,:,1)=U_COUPLING_NORTH(:,:,2)
             V_COUPLING_NORTH(:,:,1)=V_COUPLING_NORTH(:,:,2)
             W_COUPLING_NORTH(:,:,1)=W_COUPLING_NORTH(:,:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)
             P_COUPLING_NORTH(:,:,1)=P_COUPLING_NORTH(:,:,2)
             S_COUPLING_NORTH(:,:,1)=S_COUPLING_NORTH(:,:,2)
             T_COUPLING_NORTH(:,:,1)=T_COUPLING_NORTH(:,:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_north


! specify boundary start points

! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
      Kstart_WEST=J_START_WEST+Nghost -npy*Nglob/py
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1 -npy*Nglob/py
      IF((Kstart_WEST>Nghost.AND.Kstart_WEST<Nloc-Nghost+1).OR.&
     (Kend_WEST<Nloc-Nghost+1.AND.Kend_WEST>Nghost))THEN
       IF(Kstart_WEST<Nghost+1)THEN
         Kshift_WEST= -Kstart_WEST+Nghost+1
         Kstart_WEST=Nghost+1
       ELSE
         Kshift_WEST=-(Kstart_WEST-Nghost)+1
       ENDIF
       IF(Kend_WEST>Nloc-Nghost)THEN
         Kend_WEST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_WEST=.TRUE.
      ELSE
       IF(Kstart_WEST<=Nghost.AND.Kend_WEST>=Nloc-Nghost+1)THEN
         Kshift_WEST=-Kstart_WEST+Nghost+1
         Kstart_WEST = Nghost+1
         Kend_WEST = Nloc-Nghost
         IN_DOMAIN_WEST=.TRUE.
       ELSE
         IN_DOMAIN_WEST=.FALSE.
       ENDIF
      ENDIF
! check print*,myid,Kshift_WEST,Kstart_WEST,Kend_WEST
     endif
# else
      Kstart_WEST=J_START_WEST+Nghost
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1
      Kshift_WEST = -(Kstart_WEST-Nghost)+1
      IN_DOMAIN_WEST = .TRUE.
# endif

   ENDIF

! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
      Kstart_EAST=J_START_EAST+Nghost -npy*Nglob/py
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1 -npy*Nglob/py
      IF((Kstart_EAST>Nghost.AND.Kstart_EAST<Nloc-Nghost+1).OR.&
     (Kend_EAST<Nloc-Nghost+1.AND.Kend_EAST>Nghost))THEN
       IF(Kstart_EAST<Nghost+1)THEN
         Kshift_EAST= -Kstart_EAST+Nghost+1
         Kstart_EAST=Nghost+1
       ELSE
         Kshift_EAST=-(Kstart_EAST-Nghost)+1
       ENDIF
       IF(Kend_EAST>Nloc-Nghost)THEN
         Kend_EAST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_EAST=.TRUE.
      ELSE

       IF(Kstart_EAST<=Nghost.AND.Kend_EAST>=Nloc-Nghost+1)THEN
         Kshift_EAST=-Kstart_EAST+Nghost+1
         Kstart_EAST = Nghost+1
         Kend_EAST = Nloc-Nghost
         IN_DOMAIN_EAST=.TRUE.
       ELSE
         IN_DOMAIN_EAST=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_EAST=J_START_EAST+Nghost
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1
      Kshift_EAST = -(Kstart_EAST-Nghost)+1
      IN_DOMAIN_EAST = .TRUE.
# endif
    ENDIF

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
      Kstart_SOUTH=I_START_SOUTH+Nghost -npx*Mglob/px
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1 -npx*Mglob/px
      IF((Kstart_SOUTH>Nghost.AND.Kstart_SOUTH<Mloc-Nghost+1).OR.&
     (Kend_SOUTH<Mloc-Nghost+1.AND.Kend_SOUTH>Nghost))THEN
       IF(Kstart_SOUTH<Nghost+1)THEN
         Kshift_SOUTH= -Kstart_SOUTH+Nghost+1
         Kstart_SOUTH=Nghost+1
       ELSE
         Kshift_SOUTH=-(Kstart_SOUTH-Nghost)+1
       ENDIF
       IF(Kend_SOUTH>Mloc-Nghost)THEN
         Kend_SOUTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_SOUTH=.TRUE.
      ELSE

       IF(Kstart_SOUTH<=Nghost.AND.Kend_SOUTH>=Mloc-Nghost+1)THEN
         Kshift_SOUTH=-Kstart_SOUTH+Nghost+1
         Kstart_SOUTH = Nghost+1
         Kend_SOUTH = Mloc-Nghost
         IN_DOMAIN_SOUTH=.TRUE.
       ELSE
         IN_DOMAIN_SOUTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SOUTH=I_START_SOUTH+Nghost
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1
      Kshift_SOUTH = -(Kstart_SOUTH-Nghost)+1
      IN_DOMAIN_SOUTH = .TRUE.
# endif
   ENDIF

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
      Kstart_NORTH=I_START_NORTH+Nghost -npx*Mglob/px
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1 -npx*Mglob/px
      IF((Kstart_NORTH>Nghost.AND.Kstart_NORTH<Mloc-Nghost+1).OR.&
     (Kend_NORTH<Mloc-Nghost+1.AND.Kend_NORTH>Nghost))THEN
       IF(Kstart_NORTH<Nghost+1)THEN
         Kshift_NORTH= -Kstart_NORTH+Nghost+1
         Kstart_NORTH=Nghost+1
       ELSE
         Kshift_NORTH=-(Kstart_NORTH-Nghost)+1
       ENDIF
       IF(Kend_NORTH>Mloc-Nghost)THEN
         Kend_NORTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_NORTH=.TRUE.
      ELSE

       IF(Kstart_NORTH<=Nghost.AND.Kend_NORTH>=Mloc-Nghost+1)THEN
         Kshift_NORTH=-Kstart_NORTH+Nghost+1
         Kstart_NORTH = Nghost+1
         Kend_NORTH = Mloc-Nghost
         IN_DOMAIN_NORTH=.TRUE.
       ELSE
         IN_DOMAIN_NORTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_NORTH=I_START_NORTH+Nghost
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1
      Kshift_NORTH = -(Kstart_NORTH-Nghost)+1
      IN_DOMAIN_NORTH = .TRUE.
# endif
   ENDIF

END SUBROUTINE READ_NESTING_FILE
# endif




