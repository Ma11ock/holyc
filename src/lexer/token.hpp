#ifndef SLANG_TOKEN_HPP
#define SLANG_TOKEN_HPP

#include <string>
#include <regex>
#include <array>
#include <string_view>

#include "../slang.hpp"

namespace slang {
    using lexemeLen = std::string_view::size_type;
    using linenoType = std::string::size_type;
    constexpr auto noLineNum = std::string::npos;

    class Lexeme {
    protected:
        std::string_view mText;
        std::string_view mLiteralText;
        linenoType mLineNo;
        TokenType mType;
    public:
        /**
         * Pull the next token and lexeme from the file.
         */
        static Lexeme pull(std::string_view source,
                           linenoType lineNo = noLineNum);

        Lexeme(TokenType type) : mText(""),mLiteralText(""),mType(type),mLineNo(0) {
        }
        Lexeme(const std::string &text, TokenType type)
            : mText(text),mLiteralText(""),mLineNo(0),mType(type) {
        }
        Lexeme(const std::string &text, TokenType type,
               std::string::size_type lineNo)
            : mText(text),mLiteralText(""),mLineNo(lineNo),mType(type) {
        }
        Lexeme(std::string_view text, TokenType type, linenoType lineNo = std::string::npos)
            : mText(text),mLiteralText(""),mType(type),mLineNo(lineNo) {}
        Lexeme() : mText(""),mLiteralText(""),mLineNo(0),mType(TokenType::Space) {
        }
        Lexeme(TokenType type, std::string_view lexeme, linenoType lineNumber = noLineNum)
            : mText(lexeme),mLiteralText(""),mLineNo(lineNumber),mType(type) {}
        ~Lexeme() = default;

        inline lexemeLen getTextLength() const {
            return mText.size();
        }

        inline std::string getTextCopy() const {
            return std::string(mText);
        }

        inline std::string_view getText() const {
            return mText;
        }

        inline bool operator==(TokenType type) {
            return mType == type;
        }

        inline bool operator!=(TokenType type) {
            return mType != type;
        }

        inline std::string::size_type getLineNumber() const {
            return mLineNo;
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

    std::string_view stringifyTokenType(TokenType type);
}

#endif /* SLANG_TOKEN_HPP */
