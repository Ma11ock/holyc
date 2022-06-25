#ifndef SIMUL_TOKEN_HPP
#define SIMUL_TOKEN_HPP

#include <string>
#include <regex>
#include <array>
#include <tuple>
#include <string_view>

namespace smul {

    enum class TokenType {
        NoOp,
        Begin,
        End,
        If,
        Then,
        Else,
        Plus,
        Minus,
        Times,
        Divide,
        Identifier,
        Real,
        Integer,
        String,
        Eof,
        Error,
    };

    class Lexeme {
    protected:
        std::string mText;
        TokenType mType;
    public:
        Lexeme(TokenType type) : mText(""),mType(type) {
        }
        Lexeme(const std::string &text, TokenType type) : mText(text),mType(type) {
        }
        Lexeme() : mText(""),mType(TokenType::NoOp) {
        }

        std::string::size_type getTextLength() const {
            return mText.size();
        }

        std::string getText() const {
            return mText;
        }

        ~Lexeme() = default;
    };

    /**
     * Pull the next token and lexeme from the file.
     */
    std::tuple<Lexeme, std::string::size_type> lexerPull(std::string_view source);
}

#endif /* SIMUL_TOKEN_HPP */
