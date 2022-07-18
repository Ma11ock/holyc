# slang client build file.
# This file is part of Slang.

cmake_minimum_required(VERSION 3.20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
  message("Setting build type to release.")
else()
  set(CMAKE_BUILD_TYPE debug)
  set(SLANG_TESTS 1)
  message("Build type is set to debug.")
endif()

project(hclang)

# Fmtlib
add_subdirectory(external/fmt)
set(SLANG_FMT_PATH external/fmt/include)

# Slang source directory.
add_subdirectory(src)

# Unit tests.
if(SLANG_TESTS AND NOT NO_SLANG_TESTS)
  add_subdirectory(test)
endif()


