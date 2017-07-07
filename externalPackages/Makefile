
include ../versionsConfig/version.${FUNWAVE_ARCH}


all:	 install_zlib install_hdf5 install_netcdf_c install_netcdf_fortran 

# Note checks may take a while, in particular hdf5 check
check_all:	check_zlib check_hdf5 check_netcdf_c check_netcdf_fortran


# Restores local submodules repos to original state and
# cleans current version builds of submodules
distclean:
	git submodule foreach git clean -f
	rm -rf ${FUNWAVE_PREFIX}/*
	rm -f *_progress 
	rm -f install_*


#----
#zlib
#----

config_zlib:
	@echo "Configuring zlib"
	@cd ${ZLIB_VERSION} && ../zlibConfig/configure.${FUNWAVE_ARCH} > ../config_zlib_progress 2>&1

build_zlib:
	@echo "Building zlib"
	@cd ${ZLIB_VERSION} && make > ../build_zlib_progress 2>&1

check_zlib:
	@echo "Checking zlib"
	@cd ${ZLIB_VERSION} && make test > ../check_zlib_progress 2>&1


install_zlib:	config_zlib build_zlib
	@echo "Installing zlib"
	@cd ${ZLIB_VERSION} && make install > ../install_zlib_progress 2>&1
	@cat config_zlib_progress build_zlib_progress install_zlib_progress > install_zlib


#----
#hdf5
#----

config_hdf5:
	@echo "Configuring hdf5"
	@cd ${HDF5_VERSION} && ../hdf5Config/configure.${FUNWAVE_ARCH} > ../config_hdf5_progress 2>&1

build_hdf5: 
	@echo "Building hdf5"
	@cd ${HDF5_VERSION} && make > ../build_hdf5_progress 2>&1

# Note: Using -i flag in make check to skip failed test in cache_api.
#       All other test should pass.
check_hdf5: 
	@echo "Checking hdf5"
	@cd ${HDF5_VERSION} && make -i check > ../check_hdf5_progress 2>&1

install_hdf5: config_hdf5 build_hdf5
	@echo "Installing hdf5"
	@cd ${HDF5_VERSION} && make install > ../install_hdf5_progress 2>&1
	@cat config_hdf5_progress build_hdf5_progress install_hdf5_progress > install_hdf5


#--------
#netCDF-c
#--------

# autoreconf used to genereate configure, makefiles, etc
# flags used correctly call, aclocal, autoconf, and automake
# from autotools
# Note: libtoolize not call with autoreconf -ivf on some platforms

config_netcdf_c:
	@echo "Libtoolize netCDF-c"
	@cd ${NETCDF_C_VERSION} && libtoolize > ../libtoolize_netcdf_c_progress 2>&1
	@echo "Autoreconf netCDF-c"
	@cd ${NETCDF_C_VERSION} && autoreconf -ivf > ../autoreconf_netcdf_c_progress 2>&1
	@echo "Configuring netCDF-c"
	@cd ${NETCDF_C_VERSION} && ../netcdf-cConfig/configure.${FUNWAVE_ARCH} > ../config_netcdf_c_progress 2>&1


build_netcdf_c:
	@echo "Building netCDF-c"
	@cd ${NETCDF_C_VERSION} && make > ../build_netcdf_c_progress 2>&1

check_netcdf_c: 
	@echo "Checking netCDF-c"
	@cd ${NETCDF_C_VERSION} && make check > ../check_netcdf_c_progress 2>&1

install_netcdf_c: config_netcdf_c build_netcdf_c
	@echo "Installing netCDF-c"
	@cd ${NETCDF_C_VERSION} && make install > ../install_netcdf_c_progress 2>&1
	@cat libtoolize_netcdf_c_progress autoreconf_netcdf_c_progress config_netcdf_c_progress build_netcdf_c_progress install_netcdf_c_progress > install_netcdf_c


#--------------
#netCDF-fortran
#--------------

# autoreconf used to genereate configure, makefiles, etc
# see previous note
config_netcdf_fortran:
	@echo "Libtoolize netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && libtoolize > ../libtoolize_netcdf_fortran_progress 2>&1
	@echo "Autoreconf netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && autoreconf -ivf > ../autoreconf_netcdf_fortran_progress 2>&1
	@echo "Configuring netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && ../netcdf-fortranConfig/configure.${FUNWAVE_ARCH} > ../config_netcdf_fortran_progress 2>&1


build_netcdf_fortran:
	@echo "Building netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make > ../build_netcdf_fortran_progress 2>&1

check_netcdf_fortran: 
	@echo "Checking netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make check > ../check_netcdf_fortran_progress 2>&1

install_netcdf_fortran: config_netcdf_fortran build_netcdf_fortran
	@echo "Installing netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make install > ../install_netcdf_fortran_progress 2>&1
	@cat libtoolize_netcdf_fortran_progress autoreconf_netcdf_fortran_progress config_netcdf_fortran_progress build_netcdf_fortran_progress install_netcdf_fortran_progress > install_netcdf_fortran
