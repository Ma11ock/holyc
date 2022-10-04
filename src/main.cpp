
#include <fmt/color.h>
#include <fmt/core.h>

#include <cstdlib>
#include <fstream>
#include <memory>
#include <stdexcept>

#include "config.hpp"
#include "hclang.hpp"
#include "lexer/lexer.hpp"
#include "log.hpp"
#include "parser/parser.hpp"

int main(int argc, const char *const *argv) {
    try {
        log("{} version {}", hclang::NAME, hclang::VERSION);
        hclang::Config programConfig(std::vector<std::string_view>(argv + 1, argv + argc));
        if (programConfig.wasHelp()) {
            return 0;
        }

        for (const auto &sourcePath : programConfig.getSourcePaths()) {
            auto parser = hclang::ParseTree::parseSyntax(
                programConfig, std::make_shared<hclang::Lexer>(sourcePath, programConfig));
            parser->parseSemantics();
            parser->compile(programConfig.getOutputPath());
        }
    } catch (std::exception &e) {
        fmt::print(stderr, "{}-{}: {} {}\n", fmt::styled(hclang::NAME, fmt::emphasis::bold),
                   fmt::styled(hclang::VERSION, fmt::emphasis::bold),
                   fmt::styled("fatal error: ", fmt::emphasis::bold | fg(fmt::color::red)),
                   e.what());
    }
    fmt::print("");
    return 0;
}
