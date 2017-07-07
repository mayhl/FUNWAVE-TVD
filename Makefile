#setting environment variables from shell if they are not set
FUNWAVE ?= $(shell pwd)
FUNWAVE_ARCH ?= $(shell python -c "import sys; print (sys.platform)")
FUNWAVE_PREFIX ?= ${FUNWAVE}/${FUNWAVE_ARCH}
FUNWAVE_PYTHON ?= ${FUNWAVE_PREFIX}/bin/python
ifeq ($(FUNWAVE_ARCH), darwin)
	PLATFORM_ENV = MACOSX_DEPLOYMENT_TARGET=$(shell sw_vers -productVersion | sed "s/\(10.[0-9]\).*/\1/")
endif

# exporting variables to submakes
export

.PHONY: all clean cleaner help

all: testGlobalConfig installExternalPackages

check_all:
	cd externalPackages && make check_all

testGlobalConfig:
		@test -f versionsConfig/version.${FUNWAVE_ARCH}|| { echo "Global config file does not exist for current version: '"${FUNWAVE_ARCH}"'. Exiting..." ; exit 1; }

installExternalPackages:
	cd externalPackages; make all

clean_externalPackages: 
	cd externalPackages && make -k distclean
