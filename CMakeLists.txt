# This file is part of Hclang.

cmake_minimum_required(VERSION 3.20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
  message("Setting build type to release.")
else()
  set(CMAKE_BUILD_TYPE debug)
  set(HCLANG_TESTS 1)
  set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
  message("Build type is set to debug.")
endif()

project(hclang)

# Fmtlib
add_subdirectory(external/fmt)
set(HCLANG_FMT_PATH external/fmt/include)

# Hclang source directory.
add_subdirectory(src)

# Unit tests.
if(HCLANG_TESTS AND NOT NO_HCLANG_TESTS)
  add_subdirectory(test)
endif()


