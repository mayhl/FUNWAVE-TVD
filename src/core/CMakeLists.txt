# Layered Core Build System Orchestrator

# Layer 0: Base Utilities
add_subdirectory(base)

# Layer 1: Comms & I/O
add_subdirectory(transport)

# Layer 2: Time Management
add_subdirectory(engine)

# Layer 3: Basic Math/Physics
add_subdirectory(physics)

target_link_libraries(${main_lib}_core INTERFACE libcore_base libcore_transport
                                                 libcore_engine libcore_physics)

# target_link_libraries(libcore INTERFACE libcore_yaml)

list(APPEND all_targets libcore_base libcore_transport libcore_engine
     libcore_physics)
# list(APPEND all_targets libcore_yaml)

set(all_targets
    "${all_targets}"
    PARENT_SCOPE)
