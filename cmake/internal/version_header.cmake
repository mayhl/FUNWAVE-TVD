# Stamps the build identity consumed by core_version_mod (src/core/base/
# version.F90) into ${OUT} as preprocessor defines.  Runs in script mode at
# every build so the git state is never stale; the file is rewritten only when
# its content changes, so a no-op build recompiles nothing.
#
# Inputs (-D): GIT_EXECUTABLE SOURCE_DIR OUT VERSION COMPILER BUILD_TYPE DOUBLE
# MPI OPENMP NETCDF PNETCDF HYPRE

set(git_describe "unknown")
if(GIT_EXECUTABLE)
  execute_process(
    COMMAND "${GIT_EXECUTABLE}" -C "${SOURCE_DIR}" describe --tags --always
            --dirty
    RESULT_VARIABLE rc
    OUTPUT_VARIABLE out
    OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
  if(rc EQUAL 0 AND NOT out STREQUAL "")
    set(git_describe "${out}")
  endif()
endif()

function(on_off var flag)
  if(flag)
    set(${var}
        "on"
        PARENT_SCOPE)
  else()
    set(${var}
        "off"
        PARENT_SCOPE)
  endif()
endfunction()

if(DOUBLE)
  set(precision "double")
else()
  set(precision "single")
endif()
on_off(mpi "${MPI}")
on_off(openmp "${OPENMP}")
on_off(netcdf "${NETCDF}")
on_off(pnetcdf "${PNETCDF}")
on_off(hypre "${HYPRE}")

set(content
    "#define FUNWAVE_VERSION \"${VERSION}\"
#define FUNWAVE_GIT_DESCRIBE \"${git_describe}\"
#define FUNWAVE_COMPILER \"${COMPILER}\"
#define FUNWAVE_BUILD_TYPE \"${BUILD_TYPE}\"
#define FUNWAVE_PRECISION \"${precision}\"
#define FUNWAVE_MPI \"${mpi}\"
#define FUNWAVE_OPENMP \"${openmp}\"
#define FUNWAVE_NETCDF \"${netcdf}\"
#define FUNWAVE_PNETCDF \"${pnetcdf}\"
#define FUNWAVE_HYPRE \"${hypre}\"
")

set(old "")
if(EXISTS "${OUT}")
  file(READ "${OUT}" old)
endif()
if(NOT old STREQUAL content)
  file(WRITE "${OUT}" "${content}")
endif()
