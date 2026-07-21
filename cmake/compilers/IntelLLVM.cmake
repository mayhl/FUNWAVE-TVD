# Intel ifx
#
# -axAVX2 stays on the optimized build types (NOT base): the wheat/barfoot refs
# were built with its dispatch paths, so RelWithDebInfo keeps it for bitwise
# stability; Debug/Coverage drop it with the rest of the optimizer.

set(funwave_base_flags "-fpp -free -stand -traceback")

set(funwave_flags_release "-O3 -axAVX2")
set(funwave_flags_relwithdebinfo "-O2 -g -axAVX2")
set(funwave_flags_debug "-O0 -g -check all -fpe0")
# Coverage is a GNU-only diagnostic for now (lcov pipeline); llvm-cov flags
# would go here if that path is ever stood up
set(funwave_flags_coverage "")
set(funwave_flags_benchmark "-O3 -xHost -g")
