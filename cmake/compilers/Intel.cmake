# Intel classic ifort (legacy -- ifx is the supported Intel path)

set(funwave_base_flags "-fpp -free -traceback")

set(funwave_flags_release "-O3")
set(funwave_flags_relwithdebinfo "-O2 -g")
set(funwave_flags_debug "-O0 -g -check all -fpe0")
set(funwave_flags_coverage "")
set(funwave_flags_benchmark "-O3 -xHost -g")
# ASan add-on (USE_ASAN): ifort takes -fsanitize=address, untested here
set(funwave_asan_flags "-fsanitize=address -fno-omit-frame-pointer -g")
