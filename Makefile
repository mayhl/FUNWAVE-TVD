# exporting variables to submakes and scripts
# Note: Required for bash scripts
export

#setting environment variables from shell if they are not set

#FUNWAVE ?= $(shell pwd)
#FUNWAVE_ARCH ?= $(shell python -c "import sys; print (sys.platform)")
#FUNWAVE_PREFIX ?= ${FUNWAVE}/${FUNWAVE_ARCH}
#FUNWAVE_PYTHON ?= ${FUNWAVE_PREFIX}/bin/python
#ifeq ($(FUNWAVE_ARCH), darwin)
#	PLATFORM_ENV = MACOSX_DEPLOYMENT_TARGET=$(shell sw_vers -productVersion | sed "s/\(10.[0-9]\).*/\1/")
#endif


.PHONY: all clean cleaner help


all:	version_select verify_config_files install_externalPackages install_funwave


verify_config_files:
	${eval TITLE=Verifying configure files }
	@cd scripts && ./printTitleCmd.sh
	${eval DIRECTORY=versionsConfig/} 
	${eval NAME=Global}
	${eval SUB_NAME=version} 
	@cd scripts && ./verifyConfigFile.sh
	@cd externalPackages && $(MAKE) -s verify_all_config_files

version_select:
	@cd scripts && ./selectGlobalVersionMenu.sh BUILD_SELECTED=${BUILD_SELECTED}

check_all:
	cd externalPackages && make check_all


# installs
install_funwave:
	${eval TITLE=Building FUNWAVE-TVD}
	@cd scripts && ./printTitleCmd.sh
	@cd src && make

install_externalPackages:
	@cd externalPackages; $(MAKE) -s all


# cleans
clean_externalPackages: 
	cd externalPackages && make -k distclean

clean_funwave:
	cd src && make clean

clean:	clean_funwave clean_externalPackages

cleaner: 
	cd src && make clobber
