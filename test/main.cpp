
#include <fmt/core.h>
#include <gtest/gtest-printers.h>
#include <gtest/gtest.h>

#include <iostream>

#include "../src/config.hpp"
#include "../src/lexer/lexer.hpp"

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
