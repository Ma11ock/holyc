#ifndef SLANG_PARSER_HPP
#define SLANG_PARSER_HPP

#include <string_view>
#include <filesystem>
#include <stdexcept>
#include <vector>
#include "ast.hpp"

namespace slang {
    namespace fs = std::filesystem;
    class ParseTree {
    public:
        ~ParseTree() = default;
        static std::shared_ptr<ParseTree> parse(const fs::path &path);
        void compile(const fs::path &path = "") const;
    protected:
        ParseTree(const fs::path &path);
        std::string mSource;
        fs::path mSourcePath;
        std::vector<std::string::size_type> mLineOffsets;
        std::list<slang::GR> mExpressions;

        void setupLineOffsetInfo();
    };
}

#endif /* SLANG_PARSER_HPP */
