# GNU gfortran
#
# Each compiler file sets funwave_base_flags (dialect/portability, applied to
# every build) and the five per-build-type sets; cmake/CMakeLists.txt owns
# propagation.  Runtime checks live in Debug ONLY -- carrying -fcheck=all in the
# base flags put bounds checking in every production build.

set(funwave_base_flags
    "-ffree-form -ffree-line-length-none -fimplicit-none -cpp")

set(funwave_warning_flags
    "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Warray-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons"
)

set(funwave_flags_release "-O3")
set(funwave_flags_relwithdebinfo "-O2 -g")
set(funwave_flags_debug
    "-O0 -g -fcheck=all -fbacktrace -frange-check -ffpe-trap=invalid,zero,overflow ${funwave_warning_flags}"
)
# Coverage: on-demand GNU-only diagnostic (lcov target in the root list); -O0
# keeps line attribution honest, never a timing build
set(funwave_flags_coverage "-O0 -g --coverage")
# Benchmark: this-machine peak + symbols for timing/profiler attribution --
# never for refs (native arch breaks cross-machine bitwise)
set(funwave_flags_benchmark "-O3 -march=native -g")
