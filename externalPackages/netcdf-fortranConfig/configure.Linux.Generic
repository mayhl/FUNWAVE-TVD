CC=mpicc \
CXX=mpicxx \
F77=mpif90 \
FC=mpif90 \
F90=mpif90 \
CPP='mpic++ -E' \
CXXCPP='mpic++ -E' \
CPPFLAGS="-I=${FUNWAVE_PREFIX}/include" \
LIBS="-L${FUNWAVE_PREFIX}/lib -Wl,-rpath=${FUNWAVE_PREFIX}/lib  -lnetcdf -lhdf5hl_fortran -lhdf5_fortran -lhdf5_hl -lhdf5 -lz" \
./configure \
--prefix=${FUNWAVE_PREFIX}  \
--enable-parallel-tests \
--enable-shared \
CPPFLAGS="-I${FUNWAVE_PREFIX}/include" \
CXXFLAGS="-I${FUNWAVE_PREFIX}/include" \
CFLAGS="-I${FUNWAVE_PREFIX}/include" \
FFLAGS="-I${FUNWAVE_PREFIX}/include" \
LDFLAGS="-L${FUNWAVE_PREFIX}/lib -Wl,-rpath=${FUNWAVE_PREFIX}/lib"

