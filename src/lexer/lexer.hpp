#ifndef SLANG_TOKEN_HPP
#define SLANG_TOKEN_HPP

#include <fmt/color.h>
#include <fmt/format.h>

#include <array>
#include <filesystem>
#include <optional>
#include <regex>
#include <string>
#include <string_view>

#include "../config.hpp"
#include "../hclang.hpp"
#include "../util.hpp"

namespace hclang {
using lexemeLen = std::string_view::size_type;
using fileposType = std::string::size_type;
constexpr auto noLineNum = std::string::npos;
namespace fs = std::filesystem;

class Lexeme {
    public:
    Lexeme(TokenType type)
        : mText(""),
          mLiteralText(""),
          mLineNo(noLineNum),
          mEndLineNo(noLineNum),
          mColNo(noLineNum),
          mEndColNo(noLineNum),
          mType(type) {
    }

    Lexeme(const std::string &text, TokenType type)
        : mText(text),
          mLiteralText(""),
          mLineNo(noLineNum),
          mEndLineNo(noLineNum),
          mColNo(noLineNum),
          mEndColNo(noLineNum),
          mType(type) {
    }

    Lexeme(std::string_view text, TokenType type, fileposType lineNo, fileposType colNo,
        fileposType endLineNo, fileposType endColNo)
        : mText(text),
          mLiteralText(""),
          mLineNo(lineNo),
          mEndLineNo(endLineNo),
          mColNo(colNo),
          mEndColNo(endColNo),
          mType(type) {
    }

    Lexeme()
        : mText(""),
          mLiteralText(""),
          mLineNo(noLineNum),
          mColNo(noLineNum),
          mType(TokenType::Error) {
    }

    Lexeme(const Lexeme &l)
        : mText(l.mText),
          mLiteralText(l.mLiteralText),
          mLineNo(l.mLineNo),
          mEndLineNo(l.mEndLineNo),
          mColNo(l.mColNo),
          mEndColNo(l.mEndColNo),
          mType(l.mType) {
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

    Lexeme &operator=(const Lexeme &other) {
        if (this != &other) {
            mText = other.mText;
            mLiteralText = other.mLiteralText;
            mLineNo = other.mLineNo;
            mEndLineNo = other.mEndLineNo;
            mColNo = other.mColNo;
            mEndColNo = other.mEndColNo;
            mType = other.mType;
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

    inline Lexeme operator|(const Lexeme &l) {
        Lexeme newL(*this);
        newL.mEndColNo = l.mEndColNo;
        newL.mEndLineNo = l.mEndLineNo;
        return newL;
    }

    inline Lexeme &operator|=(const Lexeme &l) {
        mEndColNo = l.mEndColNo;
        mEndLineNo = l.mEndLineNo;
        return *this;
    }

    protected:
    std::string_view mText;
    std::string_view mLiteralText;
    fileposType mLineNo;
    fileposType mEndLineNo;
    fileposType mColNo;
    fileposType mEndColNo;
    TokenType mType;
};

class Lexer {
    public:
    Lexer(const fs::path &path, const Config &config);
    virtual ~Lexer() = default;
    virtual Lexeme pull();

    protected:
    std::string mSource;
    fs::path mSourcePath;
    fileposType mCurLine;
    fileposType mCurLineOffset;
    fileposType mCurPos;
    const Config &mConfig;
};

class Identifier {
    public:
    Identifier() = default;
    Identifier(std::string_view id) : mId(id) {
    }

    Identifier(const Identifier &other) : Identifier(other.mId) {
    }

    Identifier(const Lexeme &lexeme);
    ~Identifier() = default;

    inline std::string_view getId() const {
        return mId;
    }

    inline std::string getIdCopy() const {
        return std::string(mId);
    }

    inline bool operator==(const Identifier &id) const {
        return mId == id.mId;
    }

    protected:
    std::string_view mId;
};

std::string_view stringifyTokenType(TokenType type);

bool isKeyword(TokenType type);
bool isSpecifier(TokenType type);
bool isOperator(TokenType type);
bool isMaybeUnaryOperator(TokenType type);
bool isType(TokenType type);
}  // namespace hclang

namespace fmt {
template <>
struct fmt::formatter<hclang::Identifier> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const hclang::Identifier &id, FormatContext &ctx) {
        return fmt::format_to(ctx.out(), "{}", id.getId());
    }
};
}  // namespace fmt

/// Hack for printing file positions with npos positions as '?' easily.
struct __fileposPrinter {
    /// Filepos to wrap around.
    hclang::fileposType pos;
};

namespace fmt {
template <>
struct fmt::formatter<__fileposPrinter> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const __fileposPrinter &fp, FormatContext &ctx) {
        if (fp.pos == hclang::noLineNum) {
            return fmt::format_to(ctx.out(), "{}", fmt::styled('?', fg(fmt::color::gold)));
        }
        return fmt::format_to(ctx.out(), "{}", fmt::styled(fp.pos, fg(fmt::color::gold)));
    }
};

MAKE_FMT_STYLE_SPEC(hclang::Identifier)
}  // namespace fmt

namespace fmt {
template <>
struct fmt::formatter<hclang::Lexeme> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const hclang::Lexeme &l, FormatContext &ctx) {
        return fmt::format_to(ctx.out(), "<line:{}:{},col:{}:{}>",
            __fileposPrinter{l.getStartLineNumber()}, __fileposPrinter{l.getEndLineNumber()},
            __fileposPrinter{l.getStartColNumber()}, __fileposPrinter{l.getEndColNumber()});
    }
};

MAKE_FMT_STYLE_SPEC(hclang::Lexeme)
}  // namespace fmt

namespace fmt {
template <>
struct fmt::formatter<std::optional<hclang::Lexeme>> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const std::optional<hclang::Lexeme> &ol, FormatContext &ctx) {
        if (ol) {
            const auto &l = *ol;
            return fmt::format_to(ctx.out(), "{}", l);
        }
        return fmt::format_to(ctx.out(), "<line:{}:{},col:{}:{}>",
            __fileposPrinter{hclang::noLineNum}, __fileposPrinter{hclang::noLineNum},
            __fileposPrinter{hclang::noLineNum}, __fileposPrinter{hclang::noLineNum});
    }
};

MAKE_FMT_STYLE_SPEC(std::optional<hclang::Lexeme>)
}  // namespace fmt

namespace fmt {
template <>
struct fmt::formatter<hclang::TokenType> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const hclang::TokenType &t, FormatContext &ctx) {
        return fmt::format_to(ctx.out(), "{}", hclang::stringifyTokenType(t));
    }
};

MAKE_FMT_STYLE_SPEC(hclang::TokenType)
}  // namespace fmt

#endif /* SLANG_TOKEN_HPP */
