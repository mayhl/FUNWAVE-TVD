# LLVM flang

set(funwave_base_flags "-cpp")

set(funwave_flags_release "-O3")
set(funwave_flags_relwithdebinfo "-O2 -g")
set(funwave_flags_debug "-O0 -g")
set(funwave_flags_coverage "")
set(funwave_flags_benchmark "-O3 -g")
# No ASan path stood up here -- USE_ASAN errors out rather than half-work
set(funwave_asan_flags "")
