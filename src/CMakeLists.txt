# This file is part of HClang.

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
  hclang.cpp
  config.cpp
  lexer/lexer.cpp
  parser/parser.cpp
  parser/ast.cpp
  parser/symbols.cpp
  parser/llvm.cpp
  parser/semantics.cpp
  util.cpp
  # Header files.
  hclang.hpp
  config.hpp
  lexer/lexer.hpp
  parser/parser.hpp
  parser/ast.hpp
  parser/symbols.hpp
  util.hpp
  log.hpp
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

# Doxygen
find_package(Doxygen OPTIONAL_COMPONENTS mscgen dia dot)

if(DOXYGEN_FOUND)
  message(STATUS "Doxygen version ${DOXYGEN_VERSION} found.")
  if(HCLANG_BUILD_DOCS)
    message(STATUS "Setting up docs...")
    doxygen_add_docs(${BINARY}_docs ${SRC_SOURCES} main.cpp)
  endif()
else()
  if(HCLANG_BUILD_DOCS)
    message(FATAL_ERROR "Doxygen not found, but HCLANG_BUILD_DOCS set to 1.")
  else()
    message(WARNING "Doxygen not found...skipping.")
  endif()
endif()

# Dependencies
# LLVM
find_package(LLVM REQUIRED CONFIG)
# Link everything else to main.cpp

target_link_libraries(${BINARY_FINAL} PUBLIC ${BINARY})
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Linking

llvm_map_components_to_libnames(llvm_libs support core irreader)

if(CMAKE_BUILD_TYPE STREQUAL "debug")
target_compile_options(${BINARY} PRIVATE
    $<$<OR:$<C_COMPILER_ID:Clang>,$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:GNU>>:
         -Wall -Wextra -Wconversion -Wsign-conversion>
         )
endif()

target_link_libraries(
  ${BINARY}
  PUBLIC
  ${CMAKE_REQUIRED_LIBRARIES}
  m
  fmt
  LLVM)

# Include directories.
target_include_directories(${BINARY} PUBLIC ${HCLANG_FMT_PATH})
target_include_directories(${BINARY} PUBLIC ${LLVM_INCLUDE_DIRS})

#set_target_properties(${BINARY} PROPERTIES LINK_FLAGS_RELEASE -s)
target_compile_features(${BINARY} PUBLIC cxx_std_17)
target_compile_features(${BINARY} PUBLIC c_std_99)

# Definitions
target_compile_definitions(${BINARY} PUBLIC ${LLVM_DEFINITIONS})


add_custom_target(run
  COMMAND ${BINARY_FINAL}
  DEPENDS ${BINARY_FINAL}
  WORKING_DIRECTORY ../)

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
