#-----------BEGIN MAKEFILE-------------------------------------------------
         SHELL         = /bin/sh
         DEF_FLAGS     = -P -traditional 
         EXEC          = full_dispersion
#==========================================================================
#--------------------------------------------------------------------------
#        PRECISION          DEFAULT PRECISION: SINGLE                     
#                           UNCOMMENT TO SELECT DOUBLE PRECISION
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
         FLAG_1 = -DDOUBLE_PRECISION
         FLAG_2 = -DPARALLEL
#         FLAG_3 = -DLANDSLIDE
#          FLAG_4 = -DBAROCLINIC
#         FLAG_6 = -DSEDIMENT
#          FLAG_7 = -DINTEL
#          FLAG_8 = -DCOUPLING
#          FLAG_9 = -DDEBUG
#          FLAG_10 = -DOBSTACLE
#          FLAG_11 = -DMASK3D_TOPLAYER
#          FLAG_12 = -DFROUDE_CAP
          FLAG_13 = -DVESSEL
#           FLAG_14 = -DAIR_PRESSURE
#--------------------------------------------------------------------------
#  mpi defs 
#--------------------------------------------------------------------------
         CPP      = /usr/bin/cpp 
         CPPFLAGS = $(DEF_FLAGS)
#          FC       = ifort
         FC        = mpif90
#         FC       = mpiifort
         DEBFLGS  = 
         OPT      = #-g
         CLIB     = 
#==========================================================================
         FFLAGS = $(DEBFLGS) $(OPT) 
         MDEPFLAGS = --cpp --fext=f90 --file=-
         RANLIB = ranlib
#         MPILIB =  -lmpi
#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
         CPPARGS = $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_1) $(FLAG_2) $(FLAG_3) \
                   $(FLAG_4) $(FLAG_5) $(FLAG_6) $(FLAG_7) $(FLAG_8) \
                   $(FLAG_9) $(FLAG_10) $(FLAG_11) $(FLAG_12) $(FLAG_13) \
                   $(FLAG_14) $(FLAG_15) $(FLAG_16) $(FLAG_17) $(FLAG_18) \
                   $(FLAG_19) $(FLAG_20)
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------
          LIBS  = -L/Users/fyshi/OUTSIDE_Google/GITHUB_M3/hypre/src/hypre/lib -lHYPRE
          INCS  = -I/Users/fyshi/OUTSIDE_Google/GITHUB_M3/hypre/src/hypre/include
#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F .F90 

.F.o:
	$(CPP) $(CPPARGS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90
#	\rm $*.f90
#--------------------------------------------------------------------------
#  NHWAVE Source Code.
#--------------------------------------------------------------------------

MODS  = mod_param.F mod_global.F mod_input.F mod_vessel.F \

MAIN  = master.F kernel.F io.F init.F coupling.F 

SRCS = $(MODS)  $(MAIN)

OBJS = $(SRCS:.F=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)
#		mv $(EXEC) ../bin/
#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod *.f90

clobber:	clean
		/bin/rm -f *.f90 *.o $(EXEC)







