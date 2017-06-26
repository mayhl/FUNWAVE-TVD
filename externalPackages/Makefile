
include ../versionsConfig/version.${FUNWAVE_ARCH}

all:	install_zlib


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
