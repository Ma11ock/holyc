#ifndef SLANG_PARSER_HPP
#define SLANG_PARSER_HPP

#include <string_view>
#include <filesystem>
#include <stdexcept>
#include <vector>
#include <memory>
#include "ast.hpp"
#include "../lexer/token.hpp"
#include "../config.hpp"

namespace slang {
    namespace fs = std::filesystem;
    class ParseTree {
    public:
        static std::shared_ptr<ParseTree> parse(const slang::Config &config,
                                                std::shared_ptr<slang::Lexer> lexer);
        virtual ~ParseTree() = default;
        void compile(const fs::path &path = "") const;
    protected:
        Program mProgram;
        std::shared_ptr<slang::Lexer> mLexer;
        const slang::Config &mConfig;

        ParseTree(std::shared_ptr<slang::Lexer> lexer, const Config &config);
        void setupLineOffsetInfo();
    };
}

#endif /* SLANG_PARSER_HPP */
