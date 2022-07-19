#ifndef SLANG_TOKEN_HPP
#define SLANG_TOKEN_HPP

#include <string>
#include <regex>
#include <array>
#include <string_view>
#include <filesystem>
#include <fmt/format.h>
#include <fmt/color.h>

#include "../hclang.hpp"
#include "../config.hpp"

namespace hclang {
    using lexemeLen = std::string_view::size_type;
    using fileposType = std::string::size_type;
    constexpr auto noLineNum = std::string::npos;
    namespace fs = std::filesystem;

    class Lexeme {
    protected:
        std::string_view mText;
        std::string_view mLiteralText;
        fileposType mLineNo;
        fileposType mEndLineNo;
        fileposType mColNo;
        fileposType mEndColNo;
        TokenType mType;
    public:

        Lexeme(TokenType type) : mText(""),mLiteralText(""),mLineNo(noLineNum),
                                 mEndLineNo(noLineNum),mColNo(noLineNum),mEndColNo(noLineNum),
                                 mType(type) {
        }
        Lexeme(const std::string &text, TokenType type)
            : mText(text),mLiteralText(""),mLineNo(noLineNum),mEndLineNo(noLineNum),
              mColNo(noLineNum),mEndColNo(noLineNum),
              mType(type) {
        }
        Lexeme(std::string_view text, TokenType type,
               fileposType lineNo, fileposType colNo, fileposType endLineNo,
               fileposType endColNo)
            : mText(text),mLiteralText(""),mLineNo(lineNo),mEndLineNo(endLineNo),
              mColNo(colNo),mEndColNo(endColNo),mType(type) {
        }
        Lexeme() : mText(""),mLiteralText(""),mLineNo(noLineNum),mColNo(noLineNum),
                   mType(TokenType::Space) {
        }
        virtual ~Lexeme() = default;

        inline lexemeLen getTextLength() const {
            return mText.size();
        }

        inline std::string getTextCopy() const {
            return std::string(mText);
        }

        inline std::string_view getText() const {
            return mText;
        }

        inline bool operator==(TokenType type) const {
            return mType == type;
        }

        inline bool operator!=(TokenType type) const {
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

        inline fileposType getStartLineNumber() const {
            return mLineNo;
        }

        inline fileposType getEndLineNumber() const {
            return mEndLineNo;
        }

        inline fileposType getStartColNumber() const {
            return mColNo;
        }

        inline fileposType getEndColNumber() const {
            return mEndColNo;
        }
    };

    class Lexer {
    public:
        Lexer(const fs::path &path, const Config &config);
        virtual ~Lexer() = default;
        virtual Lexeme pull();
    protected:
        std::string mSource;
        fs::path mSourcePath;
        std::vector<fileposType> mLineOffsets;
        std::vector<fileposType>::size_type mCurLineNoPtr;
        fileposType mCurPos;
        const Config &mConfig;
    };

    class Identifier {
    public:
        Identifier(std::string_view id) : mId(id) {}
        Identifier(const Identifier &other) : Identifier(other.mId) {}
        Identifier(const Lexeme &lexeme);
        ~Identifier() = default;
        std::string_view getId() const { return mId; }
        std::string getIdCopy() const { return std::string(mId); }
    protected:
        std::string_view mId;
    };

    std::string_view stringifyTokenType(TokenType type);
}

namespace fmt {
    template<>
    struct fmt::formatter<hclang::Identifier>
    {
        template<typename ParseContext>
        constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const hclang::Identifier &id, FormatContext &ctx) {
            return fmt::format_to(ctx.out(), "{}", id.getId());
        }
    };
}

/// Hack for printing file positions with npos positions as '?' easily.
struct __fileposPrinter {
    /// Filepos to wrap around.
    hclang::fileposType pos;
};

namespace fmt {
    template<>
    struct fmt::formatter<__fileposPrinter>
    {
        template<typename ParseContext>
        constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const __fileposPrinter &fp, FormatContext &ctx) {
            if(fp.pos == hclang::noLineNum) {
                return fmt::format_to(ctx.out(), "{}",
                                      fmt::styled('?', fg(fmt::color::gold)));
            }
            return
                fmt::format_to(ctx.out(), "{}",
                               fmt::styled(fp.pos, fg(fmt::color::gold)));
        }
    };
}

namespace fmt {
    template<>
    struct fmt::formatter<hclang::Lexeme>
    {
        template<typename ParseContext>
        constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const hclang::Lexeme &l, FormatContext &ctx) {
            return
                fmt::format_to(ctx.out(), "<line:{}:{},col:{}:{}>",
                               __fileposPrinter{l.getStartLineNumber()},
                               __fileposPrinter{l.getEndLineNumber()},
                               __fileposPrinter{l.getStartColNumber()},
                               __fileposPrinter{l.getEndColNumber()}
                    );
        }
    };
}

#endif /* SLANG_TOKEN_HPP */
