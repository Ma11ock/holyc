
#include <fstream>
#include <fmt/core.h>
#include <fmt/color.h>
#include <stdexcept>
#include <memory>
#include "hclang.hpp"
#include "lexer/token.hpp"
#include "parser/parser.hpp"
#include "config.hpp"

int main(int argc, const char * const * argv) {
    try {
        hclang::Config programConfig(std::vector<std::string_view>(argv + 1, argv + argc));
        for(const auto &sourcePath : programConfig.getSourcePaths()) {
            auto parser = hclang::ParseTree::parse(programConfig,
                                                  std::make_shared<hclang::Lexer>(sourcePath,
                                                                                 programConfig));
            parser->compile(programConfig.getOutputPath());
        }
    } catch (std::exception &e) {
        fmt::print(fmt::emphasis::bold, "{}-{}: ", hclang::NAME, hclang::VERSION);
        fmt::print(fmt::emphasis::bold | fg(fmt::color::red), "fatal error: ");
        fmt::print("{}\n", e.what());
    }
    return 0;
}
