# FUNWAVE-TVD
This version is a beta version. Please report any bugs if you find in your case.
The released version can be downloaded from http://fengyanshi.github.io/build/html/index.html


### Prerequisites
- Fortran compilers:
  - Intel ifort
  - GFortran
  - HPE Cray
- CMake
- MPI
- Optional
  - MPI [recommended)
  - pfUnit (unit testing)

##### Unit and pfUnit 

FUNWAVE unit testing utilizes the [pfUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) framework, which needs to be built separately. However, pfUnit is optional, and FUNWAVE can be built without it. Instructions to build pfUnit may be found [here](https://github.com/Goddard-Fortran-Ecosystem/pFUnit?tab=readme-ov-file#building-and-installing-pfunit).

### Obtaining FUNWAVE

The best way to obtain FUNWAVE is to clone the git repository as follows:

    $ git clone https://github.com/fengyanshi/FUNWAVE-TVD


### Building FUNWAVE

FUNWAVE is now built with CMake. After obtaining the FUNWAVE, in the top directory of the distribution make a new directory and change into that directory before running CMake 

    $ mkdir build 
    $ cd build 
    $ cmake ..

##### Building with Unit Testing

    $ mkdir build 
    $ cd build 
    $ cmake .. -DBUILD_TESTING=ON -DCMAKE_PREFIX_PATH=path/to/pfUnit

