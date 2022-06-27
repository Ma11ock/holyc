
#include <fstream>
#include <filesystem>
#include <fmt/core.h>
#include <sstream>
#include "slang.hpp"
#include "lexer/token.hpp"

namespace fs = std::filesystem;

static std::string getSourceFile(const fs::path &path) {
    std::ifstream sourceFile(path);
    if(sourceFile.bad() || sourceFile.fail()) {
        throw std::invalid_argument("Not good :/");
    }

    std::stringstream buffer;
    buffer << sourceFile.rdbuf();
    return buffer.str();
}

int main(int argc, const char * const * argv) {
    using TT = slang::TokenType;
    std::tuple<slang::Lexeme, slang::lexemeLen> curMatch;
    std::string source = getSourceFile("file.s");
    std::string_view sourcePtr = source;
    while(std::get<0>((curMatch = slang::lexerPull(sourcePtr))) != TT::Eof) {
        auto &[curLexeme, curLexemeLen] = curMatch;

        if(curLexeme == TT::Error) {
            throw std::runtime_error("Invalid code.");
        }

        sourcePtr = std::string_view(sourcePtr.begin() + curLexemeLen,
                                     sourcePtr.size() - curLexemeLen);
    }
    return 0;
}
