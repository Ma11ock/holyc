#ifndef SLANG_PARSER_HPP
#define SLANG_PARSER_HPP

#include <filesystem>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <vector>

#include "../config.hpp"
#include "../lexer/lexer.hpp"
#include "ast.hpp"

namespace hclang {
namespace fs = std::filesystem;

class ParseTree {
    public:
    static std::shared_ptr<ParseTree> parseSyntax(const hclang::Config &config,
                                                  std::shared_ptr<hclang::Lexer> lexer);
    virtual ~ParseTree() = default;
    void compile(const fs::path &path = "") const;
    virtual void parseSemantics() = 0;

    protected:
    Program mProgram;
    std::shared_ptr<hclang::Lexer> mLexer;
    const hclang::Config &mConfig;

    ParseTree(std::shared_ptr<hclang::Lexer> lexer, const Config &config);
    void setupLineOffsetInfo();
};
}  // namespace hclang

#endif /* SLANG_PARSER_HPP */
