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

test3: 
	@cd scripts && \
	${eval DIRECTORY=versionsConfig/} \
	${eval NAME=Global}\
	${eval SUB_NAME=version} \
	./configFileCheck.sh


test2:
	cd externalPackages && make test

test:
	@cd scripts && ./menu.sh BUILD_SELECTED=${BUILD_SELECTED} || exit 1
	${eval FUNWAVE_ARCH = ${shell cat scripts/version.current.string}}
#	${eval TEST = ${shell printf "%-30s" "*"}}
#	echo ${TEST// /-} 
#	echo ${TEST}
#	@echo ${TEST}
#	${eval TEST2 = "${TEST// /-}"}
#	@echo ${TEST2}
#	@echo ""
#	@echo "-----------------------------------------"${TEST}
#	@echo "Building" ${FUNWAVE_ARCH} "Configuration"  
#	@echo "-----------------------------------------"
# Note: Double $ is to pass out correctly between shells

all: testGlobalConfig installExternalPackages

check_all:
	cd externalPackages && make check_all

testGlobalConfig:
		@test -f versionsConfig/version.${FUNWAVE_ARCH}|| { echo "Global config file does not exist for current version: '"${FUNWAVE_ARCH}"'. Exiting..." ; exit 1; }

installExternalPackages:
	cd externalPackages; make all

clean_externalPackages: 
	cd externalPackages && make -k distclean
