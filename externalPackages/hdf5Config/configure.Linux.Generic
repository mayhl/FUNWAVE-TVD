CC=mpicc \
F9X=mpif90 \
CXX=mpicxx \
./configure --prefix=${FUNWAVE_PREFIX} \
--with-zlib=${FUNWAVE_PREFIX} \
--enable-shared  \
--enable-parallel \
--enable-fortran \
LDFLAGS="-L${FUNWAVE_PREFIX}/lib -Wl,-rpath=${FUNWAVE_PREFIX}/lib"