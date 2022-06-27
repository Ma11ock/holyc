#ifndef SLANG_TOKEN_HPP
#define SLANG_TOKEN_HPP

#include <string>
#include <regex>
#include <array>
#include <tuple>
#include <string_view>

#include "../slang.hpp"

namespace slang {

    using lexemeLen = std::string::size_type;

    class Lexeme {
    protected:
        std::string mText;
        TokenType mType;
    public:
        Lexeme(TokenType type) : mText(""),mType(type) {
        }
        Lexeme(const std::string &text, TokenType type) : mText(text),mType(type) {
        }
        Lexeme() : mText(""),mType(TokenType::Space) {
        }

        inline std::string::size_type getTextLength() const {
            return mText.size();
        }

        inline std::string getTextCopy() const {
            return mText;
        }

        inline std::string_view getText() const {
            return mText;
        }

        ~Lexeme() = default;

        inline bool operator==(TokenType type) {
            return mType == type;
        }

        inline bool operator!=(TokenType type) {
            return mType != type;
        }

        Lexeme operator=(const Lexeme &other) {
            if(this != &other) {
                mType = other.mType;
                mText = other.mText;
            }
            return *this;
        }

        bool hasText() const;

        std::string stringify() const;

        inline TokenType getTokenType() const {
            return mType;
        }
    };

    /**
     * Pull the next token and lexeme from the file.
     */
    std::tuple<Lexeme, lexemeLen> lexerPull(std::string_view source);

    std::string_view stringifyTokenType(TokenType type);
}

#endif /* SLANG_TOKEN_HPP */
