# simul client build file.
# This file is part of Simul.

cmake_minimum_required(VERSION 3.20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
  message("Setting build type to release.")
else()
  set(CMAKE_BUILD_TYPE debug)
  set(SIMUL_TESTS 1)
  message("Build type is set to debug.")
endif()

project(simul)

# Fmtlib
add_subdirectory(external/fmt)
set(SIMUL_FMT_PATH external/fmt/include)

# Simul source directory.
add_subdirectory(src)

# Unit tests.
if(SIMUL_TESTS AND NOT NO_SIMUL_TESTS)
  add_subdirectory(test)
endif()


