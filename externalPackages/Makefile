
include ../versionsConfig/version.${FUNWAVE_ARCH}

all:	install_zlib install_hdf5 install_netcdf_c

check_all: check_zlib check_hdf5 check_netcdf_c

distclean:
	rm -rf ${FUNWAVE_PREFIX}/*
	rm -f *_progress 
	rm -f install_*

#zlib

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

#hdf5

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
	@cd ${HDF5_VERSION} && make check > ../check_hdf5_progress 2>&1

install_hdf5: config_hdf5 build_hdf5
	@echo "Installing hdf5"
	@cd ${HDF5_VERSION} && make install > ../install_hdf5_progress 2>&1
	@cat config_hdf5_progress build_hdf5_progress install_hdf5_progress > install_hdf5

#netCDF-c

config_netcdf_c:
	@echo "Configuring netCDF-c"
	@cd ${NETCDF_C_VERSION} && autoreconf -ivf > ../autoreconf_netcdf_c_progress 2>&1
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
	@cat autoreconf_netcdf_c_progress config_netcdf_c_progress build_netcdf_c_progress install_netcdf_c_progress > install_netcdf_c


#netCDF-fortran
config_netcdf_fortran:
	@echo "Configuring netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && ../netcdf-fortranConfig/configure.${FUNWAVE_ARCH} > ../config_netcdf_fortran_progress 2>&1


build_netcdf_fortran:
	@echo "Building netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make > ../build_netcdf_fortran_progress 2>&1

check_netcdf_fortran: 
	@echo "Checking netCDF-fortran"
	@cd ${NETCDF_F_VERSION} && make check > ../check_netcdf_fortran_progress 2>&1

install_netcdf_fortran: build_netcdf_fortran
#	@echo "Installing netCDF-fortran"
#	@cd ${NETCDF_F_VERSION} && make install > ../install_netcdf_fortran_progress 2>&1
#	@cat config_netcdf_fortran_progress build_netcdf_fortran_progress check_netcdf_fortran_progress install_netcdf_fortran_progress > install_netcdf_fortran
