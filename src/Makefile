#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh
            DEF_FLAGS     = -P -traditional 

	   FUNWAVE_ARCH   ?= ${shell cat ../currentVersion/signature}
	   include ../versionsConfig/version.${FUNWAVE_ARCH} 
           EXEC          = funwave

#==========================================================================
#--------------------------------------------------------------------------
#        PRECISION          DEFAULT PRECISION: SINGLE                     
#                           UNCOMMENT TO SELECT DOUBLE PRECISION
#--------------------------------------------------------------------------

            FLAG_1 = -DDOUBLE_PRECISION 
            FLAG_2 = -DPARALLEL
#            FLAG_3 = -DSAMPLES
            FLAG_4 = -DCARTESIAN

#             FLAG_7 = -DMIXING
#              FLAG_8 = -DCOUPLING
#             FLAG_9 = -DZALPHA
#             FLAG_10 = -DMANNING
#             FLAG_11 = -DSPHERICAL_IJ_STATION
#               FLAG_12 = -DVESSEL
#              FLAG_13 = -DVIS_KENNEDY
#              FLAG_14 = -DVESSEL_PANEL_SOURCE
#              FLAG_15 = -DREALISTIC_VESSEL_BODY
#              FLAG_16 = -DMETEO
#              FLAG_17 = -DWIND

ifdef INTEL_FLAG
        FLAG_6 = -DINTEL
endif

ifneq ($(wildcard  ../currentVersion/externalPackages),)
	FLAG_18 = -DEXTERNAL_PACKAGES
endif

#--------------------------------------------------------------------------
#  mpi defs 
#--------------------------------------------------------------------------
         CPP      = ${CCP_VERSION}
         CPPFLAGS = $(DEF_FLAGS)
         FC       = ${MPIFC_VERSION}
         DEBFLGS  = 
         OPT      = 
         CLIB     = 
#==========================================================================

         FFLAGS = $(DEBFLGS) $(OPT) 
         MDEPFLAGS = --cpp --fext=f90 --file=-
         RANLIB = ranlib
	 MPILIB = ${MPILIB_VERSION}
#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
           CPPARGS = $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_1) $(FLAG_2) \
			$(FLAG_3) $(FLAG_4) $(FLAG_5) $(FLAG_6) \
			$(FLAG_7) $(FLAG_8) $(FLAG_9) $(FLAG_10)  \
			$(FLAG_11) $(FLAG_12) $(FLAG_13) $(FLAG_14) \
			$(FLAG_15) $(FLAG_16) $(FLAG_17) $(FLAG_18) \
			$(FLAG_19) $(FLAG_20) $(FLAG_21) $(FLAG_22) \
			$(FLAG_23) $(FLAG_24)
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------

#            LIBS  = $(PV3LIB) $(CLIB)  $(PARLIB) $(IOLIBS) $(MPILIB) $(GOTMLIB)
#            INCS  = $(IOINCS) $(GOTMINCS)


# Adding links to external packages if flag is defined
ifneq (,$(findstring DEXTERNAL_PACKAGES,$(CPPARGS)))
	     LIBS ?= -L${shell cat ../currentVersion/flibs} 
	     INCS ?= -I${shell cat ../currentVersion/include}
endif

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F .F90 

.F.o:
	$(CPP) $(CPPARGS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90
	/bin/rm $*.f90
#--------------------------------------------------------------------------
#  FUNWAVE-TVD Source Code.
#--------------------------------------------------------------------------

MODS  = mod_param.F mod_global.F mod_input.F mod_vessel.F mod_bathy_correction.F \
        mod_meteo.F mod_parallel_field_io.F

MAIN  = main.F bc.F fluxes.F init.F io.F tridiagnal.F       \
        breaker.F derivatives.F dispersion.F etauv_solver.F \
        sponge.F sources.F masks.F parallel.F statistics.F \
        wavemaker.F mixing.F nesting.F misc.F samples.F\

SRCS = $(MODS)  $(MAIN)

OBJS = $(SRCS:.F=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)
		mv $(EXEC) ../${FUNWAVE_ARCH}/bin/
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

tarfile:
	tar cvf funwave_tvd.tar *.F  Makefile

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod

clobber:	clean
		/bin/rm -f *.f90 *.o $(EXEC)

test:
	@echo ${INTEL_FLAG}
	${eval TEST123=Y}
	@echo ${TEST123}
