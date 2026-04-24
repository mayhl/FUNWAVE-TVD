# FUNWAVE-TVD

This version is a beta version. Please report any bugs if you find in your case.
The released version can be downloaded from http://fengyanshi.github.io/build/html/index.html

### Building with CMake

##### Cloning the FUNWAVE repository

![alt](doc/assets/build_part1.svg)

##### Creating a CMake build directory

![alt](doc/assets/build_part2.svg)

> [!NOTE]
> This example defaults to the `demo/build` directory; however, any alternative directory is acceptable. The general usage of the `cmake` command is as follows:  
> -S specifies the source directory to FUNWAVE-TVD repository  
> -B specifies the build directory where files will be generated

    cmake -S /path/to/source -B /path/to/build

##### Configuring FUNWAVE and optional modules

![alt](doc/assets/build_part3.svg)

> [!NOTE]
> Once variables are set, press 'c' to apply the configuration. You may need to do this multiple times until all changes are processed and the 'Generate' option becomes available.

##### Compiling FUNWAVE executable

![alt](doc/assets/build_part4.svg)
