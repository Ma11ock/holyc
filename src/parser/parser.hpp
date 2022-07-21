#ifndef SLANG_PARSER_HPP
#define SLANG_PARSER_HPP

#include <string_view>
#include <filesystem>
#include <stdexcept>
#include <vector>
#include <memory>
#include "ast.hpp"
#include "../lexer/lexer.hpp"
#include "../config.hpp"

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
}

#endif /* SLANG_PARSER_HPP */
