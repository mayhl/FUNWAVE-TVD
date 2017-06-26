
include ../versionsConfig/version.${FUNWAVE_ARCH}

all:	install_hdf5

distclean: 
	rm -f *_progress 
	rm -f install_*

#zlib

config_zlib:
	@echo "Configuring zlib"
	@cd ${ZLIB_VERSION} && ../zlibConfig/configure.${FUNWAVE_ARCH} > ../config_zlib_progress 2>&1

build_zlib:
	@echo "Building zlib"
	@cd ${ZLIB_VERSION} && make > ../build_zlib_progress 2>&1

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

check_hdf5: 
	@echo "Checking hdf5"
	@cd ${HDF5_VERSION} && make check > ../check_hdf5_progress 2>&1

install_hdf5: config_hdf5 build_hdf5 check_hdf5
	@echo "Installing hdf5"
	@cd ${HDF5_VERSION} && make install > ../install_hdf5_progress 2>&1
	@cat config_hdf5_progress build_hdf5_progress check_hdf5_progress install_hdf5_progress > install_hdf5

#netCDF-c


build_netCDF_c:



install_netCDF_c: build_netCDF_c
