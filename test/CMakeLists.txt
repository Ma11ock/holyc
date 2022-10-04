# simul client source build file.
# This file is part of Simul.
enable_testing()

find_package(Threads REQUIRED)
find_package(GTest REQUIRED)


enable_language(CXX)
#include_directories("${PROJECT_SOURCE_DIR}/src/") It won't compile with this for some reason


set(BINARY_TEST ${CMAKE_PROJECT_NAME}_test)

project(${BINARY_TEST})

set(HCLANG_TEST_SRCS
  main.cpp
  lexer.cpp
  parser.cpp
  test.hpp
)

add_executable(
    ${BINARY_TEST}
    ${SRC_SOURCES}

    ${HCLANG_TEST_SRCS}
)

target_link_libraries(
    ${BINARY_TEST}
    ${CMAKE_PROJECT_NAME}_lib
    gtest
    gtest_main
    pthread
)

# "make check" to compile and run the tests.
add_test(
    NAME    ${CMAKE_PROJECT_NAME}
    COMMAND ${BINARY_TEST}
)

target_include_directories(${BINARY_TEST} PRIVATE ../src/)
target_compile_features(${BINARY_TEST} PRIVATE cxx_std_17)
target_compile_features(${BINARY_TEST} PRIVATE c_std_99)


# Auto run the test on make check
add_custom_target(check COMMAND ${BINARY_TEST}
                  DEPENDS ${BINARY_TEST})


if(CMAKE_BUILD_TYPE STREQUAL "debug")
target_compile_options(${BINARY_TEST} PRIVATE
    $<$<OR:$<C_COMPILER_ID:Clang>,$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:GNU>>:
         -Wall -Wextra -Wconversion -Wsign-conversion>
         )
endif()

string(TOUPPER ${CMAKE_PROJECT_NAME} PROJ_NAME_CAPS)
add_compile_definitions(${PROJ_NAME_CAPS}_TEST=1)
