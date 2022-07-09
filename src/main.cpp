
#include <fstream>
#include <fmt/core.h>
#include <fmt/color.h>
#include <stdexcept>
#include <memory>
#include "slang.hpp"
#include "lexer/token.hpp"
#include "parser/parser.hpp"
#include "config.hpp"

int main(int argc, const char * const * argv) {
    try {
        slang::Config programConfig(std::vector<std::string_view>(argv + 1, argv + argc));
        for(const auto &sourcePath : programConfig.getSourcePaths()) {
            auto parser = slang::ParseTree::parse(programConfig,
                                                  std::make_shared<slang::Lexer>(sourcePath));
            parser->compile(programConfig.getOutputPath());
        }
    } catch (std::exception &e) {
        fmt::print(fmt::emphasis::bold, "{}-{}: ", slang::NAME, slang::VERSION);
        fmt::print(fmt::emphasis::bold | fg(fmt::color::red), "fatal error: ");
        fmt::print("{}\n", e.what());
    }
    return 0;
}
