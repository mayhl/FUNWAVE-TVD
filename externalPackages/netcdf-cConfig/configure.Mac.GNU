CC=mpicc \
CXX=mpicxx \
F77=mpif90 \
FC=mpif90 \
F90=mpif90 \
CPP='mpic++ -E' \
CXXCPP='mpic++ -E' \
./configure \
--prefix=${FUNWAVE_PREFIX} \
--enable-netcdf-4 \
--disable-dap \
--enable-parallel-tests \
--enable-shared \
CPPFLAGS="-I${FUNWAVE_PREFIX}/include" \
CXXFLAGS="-I${FUNWAVE_PREFIX}/include" \
CFLAGS="-I${FUNWAVE_PREFIX}/include" \
FFLAGS="-I${FUNWAVE_PREFIX}/include" \
LDFLAGS="-L${FUNWAVE_PREFIX}/lib"

