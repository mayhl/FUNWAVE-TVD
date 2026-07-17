# Layered Core Build System Orchestrator

# Layer 0: Base Utilities
add_subdirectory(base)

# Layer 1: Infrastructure (MPI, logging, YAML, env, range)
add_subdirectory(infra)

# Layer 2: Engine (time, stepper)
add_subdirectory(engine)

# Layer 3: Numerics (grid, accumulators, interpolation, CRS)
add_subdirectory(numerics)

# Layer 4: Output (peer to engine; depends on numerics + engine)
add_subdirectory(output)

target_link_libraries(
  ${main_lib}_core INTERFACE libcore_base libcore_infra libcore_engine
                             libcore_numerics libcore_output)

list(
  APPEND
  all_targets
  libcore_base
  libcore_infra
  libcore_engine
  libcore_numerics
  libcore_output)

set(all_targets
    "${all_targets}"
    PARENT_SCOPE)
