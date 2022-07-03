# slang client source build file.
# This file is part of Slang.

# Separate main.cpp from everything else for testing purposes.
set(BINARY_FINAL ${CMAKE_PROJECT_NAME})
set(BINARY ${BINARY_FINAL}_lib)
enable_language(C)
enable_language(CXX)
set(CMAKE_C_VERSION 99)
set(CMAKE_CXX_VERSION 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
cmake_policy(SET CMP0075 NEW)

# Set the source files.
set(SRC_SOURCES
  slang.cpp
  config.cpp
  lexer/token.cpp
  parser/parser.cpp
  parser/ast.cpp
  # Header files.
  slang.hpp
  config.hpp
  lexer/token.hpp
  parser/parser.hpp
  parser/ast.hpp
  parser/llvm.cpp
  )

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(
  ${BINARY}
  ${SRC_SOURCES}
  )

# The main executable
add_executable(
  ${BINARY_FINAL}

  main.cpp
  )

# Dependencies
# LLVM
find_package(LLVM REQUIRED CONFIG)
# Link everything else to main.cpp

target_link_libraries(${BINARY_FINAL} PUBLIC ${BINARY})
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Linking

#llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(
  ${BINARY}
  PUBLIC
  ${CMAKE_REQUIRED_LIBRARIES}
  m
  fmt
  LLVM)

# Include directories.
target_include_directories(${BINARY} PUBLIC ${SLANG_FMT_PATH})
target_include_directories(${BINARY} PUBLIC ${LLVM_INCLUDE_DIRS})

set_target_properties(${BINARY} PROPERTIES LINK_FLAGS_RELEASE -s)
target_compile_features(${BINARY} PUBLIC cxx_std_17)
target_compile_features(${BINARY} PUBLIC c_std_99)

# Definitions
target_compile_definitions(${BINARY} PUBLIC ${LLVM_DEFINITIONS})


add_custom_target(run
  COMMAND ${BINARY_FINAL}
  DEPENDS ${BINARY_FINAL}
  WORKING_DIRECTORY ../)

if(CMAKE_BUILD_TYPE STREQUAL "debug")
target_compile_options(${BINARY_FINAL} PRIVATE
    $<$<OR:$<C_COMPILER_ID:Clang>,$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:GNU>>:
         -Wall -Wextra -Wconversion -Wsign-conversion>
         )
endif()

install(TARGETS ${BINARY}
  DESTINATION bin/
  PERMISSIONS
  OWNER_READ
  OWNER_WRITE
  OWNER_EXECUTE
  GROUP_READ
  GROUP_EXECUTE
  WORLD_READ
  WORLD_EXECUTE
  )

# For the install script
