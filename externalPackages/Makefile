# exporting variables to submakes and scripts
# Note: Required for bash scripts
export

#Note: ?= assignment operator evaulates expression on right hand side



FUNWAVE ?= $(shell cd .. ; pwd)
FUNWAVE_ARCH ?= ${shell cat ../currentVersion/signature}
FUNWAVE_PREFIX ?= ${FUNWAVE}/${FUNWAVE_ARCH}

# for autoconf_tools
export PATH:=${FUNWAVE_PREFIX}/util/bin:${PATH}


export PATH:=${LD_LIBRARY_PATH}:${PATH}

EXTERN_PACK_DIR=externalPackages/

# quick hack for Topaz                                                                                   
# ifeq (${FUNWAVE_ARCH}, Topaz.Intel)
#	AUTORECONF = '/app/unsupported/COST/autoconf/2.69/intel/bin/autoreconf'
# else
#	AUTORECONF = autoreconf
# endif

include ../versionsConfig/version.${FUNWAVE_ARCH}


.PHONY: all clean cleaner help 

all:	title install_autoconf_tools install_zlib install_hdf5 install_netcdf_c install_netcdf_fortran save_libary_dependencies

title:
	${eval TITLE=Building Libary Dependencies}
	@cd ../scripts && ./printTitleCmd.sh


# Saving linking options for building FUNWAVE with NetCDF libary
save_libary_dependencies:
	@cd ${FUNWAVE_PREFIX}/bin && ./nf-config --includedir > ../../currentVersion/include 2>&1
	@cd ${FUNWAVE_PREFIX}/bin && ./nf-config --flibs > ../../currentVersion/flibs 2>&1


# Note checks may take a while, in particular hdf5 check
check_all:	check_zlib check_hdf5 check_netcdf_c check_netcdf_fortran


# Verify existence of configure files for current build
verify_all_config_files: verify_zlib_config_file \
			 verify_hdf5_config_file \
			 verify_netcdf_c_config_file \
			 verify_netcdf_fortran_config_file 

# Restores local submodules repos to original state and
# cleans current version builds of submodules
distclean:
	git submodule foreach git clean -f
	rm -rf ${FUNWAVE_PREFIX}/*

#------------------
#  autoconf tools
#------------------
install_autoconf_tools:

ifneq ($(wildcard  ${FUNWAVE_PREFIX}/install_autoconf_tools),)
	@echo "autoconf tools appears to be installed, skipping installation" 
else
	@echo "Installing autoconf tools"
	@cd autoconf_tools && bash preinstall_autoconf_tools.sh > ${FUNWAVE_PREFIX}/install_autoconf_tools_progress 2>&1
	@mv ${FUNWAVE_PREFIX}/install_autoconf_tools_progress ${FUNWAVE_PREFIX}/install_autoconf_tools
endif

#--------
#  zlib
#--------

verify_zlib_config_file:
	${eval DIRECTORY=${EXTERN_PACK_DIR}zlibConfig/}
	${eval NAME=zLib}
	${eval SUB_NAME=configure}
	@cd ../scripts && ./verifyConfigFile.sh  

config_zlib:
	@echo "Configuring zlib"
	@cd ${ZLIB_VERSION} && ../zlibConfig/configure.${FUNWAVE_ARCH} > ${FUNWAVE_PREFIX}/config_zlib_progress 2>&1

build_zlib:
	@echo "Building zlib"
	@cd ${ZLIB_VERSION} && make > ${FUNWAVE_PREFIX}/build_zlib_progress 2>&1

check_zlib:
	@echo "Checking zlib"
	@cd ${ZLIB_VERSION} && make test > ${FUNWAVE_PREFIX}/check_zlib_progress 2>&1


install_zlib:
ifneq ($(wildcard  ${FUNWAVE_PREFIX}/install_zlib),)
	@echo "zlib appears to be installed, skipping installation" 
else
	@make -s config_zlib build_zlib 
	@echo "Installing zlib"
	@cd ${ZLIB_VERSION} && make install > ${FUNWAVE_PREFIX}/install_zlib_progress 2>&1
	@cd ${FUNWAVE_PREFIX} && cat config_zlib_progress build_zlib_progress install_zlib_progress > install_zlib
	@cd ${FUNWAVE_PREFIX} && rm config_zlib_progress build_zlib_progress install_zlib_progress
endif

#--------
#  hdf5
#--------


verify_hdf5_config_file:
	${eval DIRECTORY=${EXTERN_PACK_DIR}hdf5Config/}
	${eval NAME=HDF5}
	${eval SUB_NAME=configure}
	@cd ../scripts && ./verifyConfigFile.sh  

config_hdf5:
	@echo "Libtoolize HDF5"
	@cd ${HDF5_VERSION} && libtoolize > ${FUNWAVE_PREFIX}/libtoolize_hdf5_progress 2>&1
	@echo "Autoreconf HDF5"
	@cd ${HDF5_VERSION} && autoreconf -ivf > ${FUNWAVE_PREFIX}/autoreconf_hdf5_progress 2>&1
	@echo "Configuring HDF5"
	@cd ${HDF5_VERSION} && ../hdf5Config/configure.${FUNWAVE_ARCH} > ${FUNWAVE_PREFIX}/config_hdf5_progress 2>&1

build_hdf5: 
	@echo "Building HDF5"
	@cd ${HDF5_VERSION} && make > ${FUNWAVE_PREFIX}/build_hdf5_progress 2>&1

# Note: Using -i flag in make check to skip failed test in cache_api.
#       All other test should pass.
check_hdf5: 
	@echo "Checking HDF5"
	@cd ${HDF5_VERSION} && make -i check > ${FUNWAVE_PREFIX}/check_hdf5_progress 2>&1

install_hdf5: 
ifneq ($(wildcard  ${FUNWAVE_PREFIX}/install_hdf5),)
	@echo "HDF5 appears to be installed, skipping installation" 
else
	@make -s config_hdf5 build_hdf5
	@echo "Installing HDF5"
	@cd ${HDF5_VERSION} && make install > ${FUNWAVE_PREFIX}/install_hdf5_progress 2>&1
	@cd ${FUNWAVE_PREFIX} && cat libtoolize_hdf5_progress autoreconf_hdf5_progress config_hdf5_progress build_hdf5_progress install_hdf5_progress > install_hdf5
	@cd ${FUNWAVE_PREFIX} && rm libtoolize_hdf5_progress autoreconf_hdf5_progress config_hdf5_progress build_hdf5_progress install_hdf5_progress
endif

#------------
#  netCDF-c
#------------

verify_netcdf_c_config_file:
	${eval DIRECTORY=${EXTERN_PACK_DIR}/netcdf-cConfig/}
	${eval NAME=NetCDF-C}
	${eval SUB_NAME=configure}
	@cd ../scripts && ./verifyConfigFile.sh

# autoreconf used to genereate configure, makefiles, etc
# flags used correctly call, aclocal, autoconf, and automake
# from autotools
# Note: libtoolize not call with autoreconf -ivf on some platforms
config_netcdf_c:
	@echo "Libtoolize netCDF-c"
	@cd ${NETCDF_C_VERSION} && libtoolize > ${FUNWAVE_PREFIX}/libtoolize_netcdf_c_progress 2>&1
	@echo "Autoreconf netCDF-c"
	@cd ${NETCDF_C_VERSION} && autoreconf -ivf > ${FUNWAVE_PREFIX}/autoreconf_netcdf_c_progress 2>&1
	@echo "Configuring netCDF-c"
	${eval LDFLAGS=-L${FUNWAVE_PREFIX}/lib -Wl,-rpath=${FUNWAVE_PREFIX}/lib}
	@cd ${NETCDF_C_VERSION} && ../netcdf-cConfig/configure.${FUNWAVE_ARCH} > ${FUNWAVE_PREFIX}/config_netcdf_c_progress 2>&1


build_netcdf_c:
	@echo "Building netCDF-c"
	@cd ${NETCDF_C_VERSION} && make > ${FUNWAVE_PREFIX}/build_netcdf_c_progress 2>&1

check_netcdf_c: 
	@echo "Checking netCDF-c"
	@cd ${NETCDF_C_VERSION} && make check > ${FUNWAVE_PREFIX}/check_netcdf_c_progress 2>&1

install_netcdf_c:
ifneq ($(wildcard  ${FUNWAVE_PREFIX}/install_netcdf_c),)
	@echo "netCDF-c appears to be installed, skipping installation" 
else
	@make -s config_netcdf_c build_netcdf_c
	@echo "Installing netCDF-c"
	@cd ${NETCDF_C_VERSION} && make install > ${FUNWAVE_PREFIX}/install_netcdf_c_progress 2>&1
	@cd ${FUNWAVE_PREFIX} && cat libtoolize_netcdf_c_progress autoreconf_netcdf_c_progress config_netcdf_c_progress build_netcdf_c_progress install_netcdf_c_progress > install_netcdf_c
	@cd ${FUNWAVE_PREFIX} && rm libtoolize_netcdf_c_progress autoreconf_netcdf_c_progress config_netcdf_c_progress build_netcdf_c_progress install_netcdf_c_progress
endif

#------------------
#  netCDF-fortran
#------------------

verify_netcdf_fortran_config_file:
	${eval DIRECTORY=${EXTERN_PACK_DIR}netcdf-fortranConfig/}
	${eval NAME=NetCDF-Fortran}
	${eval SUB_NAME=configure}
	@cd ../scripts && ./verifyConfigFile.sh

# autoreconf used to genereate configure, makefiles, etc
# see previous note
config_netcdf_fortran:
	@echo "Libtoolize netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && libtoolize > ${FUNWAVE_PREFIX}/libtoolize_netcdf_fortran_progress 2>&1
	@echo "Autoreconf netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && autoreconf -ivf > ${FUNWAVE_PREFIX}/autoreconf_netcdf_fortran_progress 2>&1
	@echo "Configuring netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && ../netcdf-fortranConfig/configure.${FUNWAVE_ARCH} > ${FUNWAVE_PREFIX}/config_netcdf_fortran_progress 2>&1


build_netcdf_fortran:
	@echo "Building netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make > ${FUNWAVE_PREFIX}/build_netcdf_fortran_progress 2>&1

check_netcdf_fortran: 
	@echo "Checking netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make check > ${FUNWAVE_PREFIX}/check_netcdf_fortran_progress 2>&1

install_netcdf_fortran: 

ifneq ($(wildcard  ${FUNWAVE_PREFIX}/install_netcdf_fortran),)
	@echo "netCDF-fortran appears to be installed, skipping installation" 
else
	@make -s config_netcdf_fortran build_netcdf_fortran
	@echo "Installing netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make install > ${FUNWAVE_PREFIX}/install_netcdf_fortran_progress 2>&1
	@cd ${FUNWAVE_PREFIX} && cat libtoolize_netcdf_fortran_progress autoreconf_netcdf_fortran_progress config_netcdf_fortran_progress build_netcdf_fortran_progress install_netcdf_fortran_progress > install_netcdf_fortran
	@cd ${FUNWAVE_PREFIX} && rm libtoolize_netcdf_fortran_progress autoreconf_netcdf_fortran_progress config_netcdf_fortran_progress build_netcdf_fortran_progress install_netcdf_fortran_progress
endif
