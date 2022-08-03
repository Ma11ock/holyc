#include "lexer.hpp"

#include <string>
#include <regex>
#include <tuple>
#include <string_view>
#include <sstream>
#include <fstream>
#include <fmt/core.h>
#include <algorithm>
#include <limits>
#include "../util.hpp"

using TT = hclang::TokenType;
using namespace std::string_view_literals;

namespace fs = std::filesystem;

class Token {
public:
    Token(const std::string &regex, hclang::TokenType type,
          bool canBeMultiline = false)
        : mRegex(regex, std::regex_constants::icase | std::regex_constants::ECMAScript),
        mType(type),mCanBeMultiline(canBeMultiline) { }
    ~Token() = default;

   /**
    *
    */
    std::tuple<bool, std::string_view> match(std::string_view source) const {
        using namespace std::literals;
        using svmatch = std::match_results<std::string_view::const_iterator>;
        svmatch sm;
        if(!std::regex_search(source.cbegin(), source.cend(), sm, mRegex)) {
            return std::make_tuple(false, ""sv);
        }

        const char *first = sm[0].first;
        const char *last = sm[0].second;
        return std::make_tuple(true,
                               std::string_view(first,
                                                static_cast<std::size_t>(last - first)));
    }

    hclang::TokenType getTokenType() const {
        return mType;
    }

    hclang::Lexeme makeLexeme(std::string_view text) const {

        switch(mType) {
        case TT::Identifier:
        case TT::IntegerConstant:
        case TT::StringConstant:
        case TT::CharacterConstant:
            return hclang::Lexeme(std::string(text), mType);
            break;
        default:
            return hclang::Lexeme(mType);
            break;
        }
    }

    inline bool maybeMultiline() const {
        return mCanBeMultiline;
    }
protected:
    std::regex mRegex;
    hclang::TokenType mType;
    bool mCanBeMultiline;
};


std::string_view hclang::stringifyTokenType(TokenType type) {
    switch(type) {
    case TT::Public:
        return "Public";
        break;
    case TT::Reg:
        return "Reg";
        break;
    case TT::Noreg:
        return "Noreg";
        break;
    case TT::Error:
        return "Error";
        break;
    case TT::Static:
        return "Static";
        break;
    case TT::Eof:
        return "EOF";
        break;
    case TT::LCurlyBracket:
        return "LCurlyBracket";
        break;
    case TT::RCurlyBracket:
        return "RCurlyBracket";
        break;
    case TT::If:
        return "If";
        break;
    case TT::Return:
        return "Return";
        break;
    case TT::Else:
        return "Else";
        break;
    case TT::ElseIf:
        return "ElseIf";
        break;
    case TT::Class:
        return "Class";
        break;
    case TT::Enum:
        return "Enum";
        break;
    case TT::Union:
        return "Union";
        break;
    case TT::While:
        return "While";
        break;
    case TT::For:
        return "For";
        break;
    case TT::Boolean:
        return "Boolean";
        break;
    case TT::I0i:
        return "I0i";
        break;
    case TT::I8i:
        return "I8i";
        break;
    case TT::I16i:
        return "I16i";
        break;
    case TT::I32i:
        return "I32i";
        break;
    case TT::I64i:
        return "I64i";
        break;
    case TT::U0i:
        return "U0i";
        break;
    case TT::U8i:
        return "U8i";
        break;
    case TT::U16i:
        return "U16i";
        break;
    case TT::U32i:
        return "U32i";
        break;
    case TT::U64i:
        return "U64i";
        break;
    case TT::F64:
        return "F64";
        break;
    case TT::Goto:
        return "Goto";
        break;
    case TT::Switch:
        return "Switch";
        break;
    case TT::Case:
        return "Case";
        break;
    case TT::Extern:
        return "Extern";
        break;
    case TT::Import:
        return "Import";
        break;
    case TT::_Extern:
        return "_Extern";
        break;
    case TT::_Import:
        return "_Import";
        break;
    case TT::Try:
        return "Try";
        break;
    case TT::Catch:
        return "Catch";
        break;
    case TT::Throw:
        return "Throw";
        break;
    case TT::Lastclass:
        return "Lastclass";
        break;
    case TT::Plus:
        return "Plus";
        break;
    case TT::Minus:
        return "Minus";
        break;
    case TT::Star:
        return "Star";
        break;
    case TT::Divide:
        return "Divide";
        break;
    case TT::Modulo:
        return "Modulo";
        break;
    case TT::Identifier:
        return "Identifier";
        break;
    case TT::Equals:
        return "Equals";
        break;
    case TT::LessThan:
        return "LessThan";
        break;
    case TT::LessThanEqual:
        return "LessThanEqual";
        break;
    case TT::GreaterThan:
        return "GreaterThan";
        break;
    case TT::GreaterThanEqual:
        return "GreaterThanEqual";
        break;
    case TT::Equality:
        return "Equality";
        break;
    case TT::Inequality:
        return "Inequality";
        break;
    case TT::Power:
        return "Power";
        break;
    case TT::LogicalAnd:
        return "LogicalAnd";
        break;
    case TT::LogicalOr:
        return "LogicalOr";
        break;
    case TT::LogicalNot:
        return "LogicalNot";
        break;
    case TT::Ampersand:
        return "Ampersand";
        break;
    case TT::BitwiseOr:
        return "BitwiseOr";
        break;
    case TT::BitwiseXor:
        return "BitwiseXor";
        break;
    case TT::BitwiseNot:
        return "BitwiseNot";
        break;
    case TT::BitshiftLeft:
        return "BitshiftLeft";
        break;
    case TT::BitshiftRight:
        return "BitshiftRight";
        break;
    case TT::QuestionMark:
        return "QuestionMark";
        break;
    case TT::Sizeof:
        return "Sizeof";
        break;
    case TT::PlusEqual:
        return "PlusEqual";
        break;
    case TT::MinusEqual:
        return "MinusEqual";
        break;
    case TT::TimesEqual:
        return "TimesEqual";
        break;
    case TT::DividedByEqual:
        return "DividedByEqual";
        break;
    case TT::LeftshiftEqual:
        return "LeftshiftEqual";
        break;
    case TT::RightshiftEqual:
        return "RightshiftEqual";
        break;
    case TT::ModuloEqual:
        return "ModuloEqual";
        break;
    case TT::XorEqual:
        return "XorEqual";
        break;
    case TT::OrEqual:
        return "OrEqual";
        break;
    case TT::AndEqual:
        return "AndEqual";
        break;
    case TT::PlusPlus:
        return "PlusPlus";
        break;
    case TT::MinusMinus:
        return "MinusMinus";
        break;
    case TT::Semicolon:
        return "Semicolon";
        break;
    case TT::Colon:
        return "Colon";
        break;
    case TT::Lparen:
        return "Lparen";
        break;
    case TT::Rparen:
        return "Rparen";
        break;
    case TT::Dot:
        return "Dot";
        break;
    case TT::Lbracket:
        return "Lbracket";
        break;
    case TT::Rbracket:
        return "Rbracket";
        break;
    case TT::Comma:
        return "Comma";
        break;
    case TT::DoubleQuote:
        return "DoubleQuote";
        break;
    case TT::SingleQuote:
        return "SingleQuote";
        break;
    case TT::TripleDot:
        return "TripleDot";
        break;
    case TT::CharacterConstant:
        return "CharacterConstant";
        break;
    case TT::FloatConstant:
        return "FloatConstant";
        break;
    case TT::IntegerConstant:
        return "IntegerConstant";
        break;
    case TT::StringConstant:
        return "StringConstant";
        break;
    case TT::Label:
        return "Label";
        break;
    case TT::Space:
        return "Space";
        break;
    default:
        break;
    }
    throw std::invalid_argument(fmt::format("Invalid token: (integer){}",
                                            static_cast<std::uint32_t>(type)));
}

bool hclang::Lexeme::hasText() const {
    switch(mType) {
    case TT::Identifier:
    case TT::IntegerConstant:
    case TT::StringConstant:
    case TT::CharacterConstant:
        return true;
        break;
    default:
        break;
    }
    return false;
}

std::string hclang::Lexeme::stringify() const {
    if(hasText()) {
        return fmt::format("{}: {}", hclang::stringifyTokenType(mType), mText);
    }
    return std::string(hclang::stringifyTokenType(mType));
}


// Lexer implementation.

hclang::Lexer::Lexer(const fs::path &path, const hclang::Config &config)
    : mSource(""),mSourcePath(path),mCurLine(1),mCurLineOffset(0),mCurPos(0),
      mConfig(config) {
    std::ifstream sourceFile(path);
    if(!sourceFile) {
        throw std::system_error(util::getError(), std::system_category(), path.string());
    }

    std::stringstream buffer;
    buffer << sourceFile.rdbuf();
    mSource = buffer.str();
}

hclang::Lexeme hclang::Lexer::pull() {
    const static std::array TOKENS = {
        // EOF
        Token("^$", TT::Eof),
        // Whitespace, comments, etc.
        Token("^(\\s+|/\\*(?:[\\s\\S])*?\\*/|//.*)", TT::Space, true),
        // Keywords.
        Token("^\\{", TT::LCurlyBracket),
        Token("^\\}", TT::RCurlyBracket),
        Token("^if", TT::If),
        Token("^return", TT::Return),
        Token("^else", TT::Else),
        Token("^else\\s+if", TT::ElseIf, true),
        Token("^class", TT::Class),
        Token("^while", TT::While),
        Token("^do", TT::Do),
        Token("^for", TT::For),
        Token("^boolean", TT::Boolean),
        Token("^I8i", TT::I8i),
        Token("^I16i", TT::I16i),
        Token("^I32i", TT::I32i),
        Token("^I64i", TT::I64i),
        Token("^U0i|^I0i", TT::U0i),
        Token("^U8i", TT::U8i),
        Token("^U16i", TT::U16i),
        Token("^U32i", TT::U32i),
        Token("^U64i", TT::U64i),
        Token("^goto", TT::Goto),
        Token("^switch", TT::Switch),
        Token("^default", TT::Default),
        Token("^start", TT::Start),
        Token("^end", TT::End),
        Token("^case", TT::Case),
        Token("^import", TT::Import),
        Token("^extern", TT::Extern),
        Token("^\\_import", TT::_Import),
        Token("^\\_extern", TT::_Extern),
        Token("^try", TT::Try),
        Token("^catch", TT::Catch),
        Token("^throw", TT::Throw),
        Token("^lastclass", TT::Lastclass),
        // Operators and misc. grammar symbols.
        Token("^sizeof", TT::Sizeof),
        Token("^\\?", TT::QuestionMark),
        Token("^\\&", TT::Ampersand),
        Token("^\\|", TT::BitwiseOr),
        Token("^\\~", TT::BitwiseNot),
        Token("^\\^", TT::BitwiseXor),
        Token("^\\:", TT::Colon),
        Token("^\\*", TT::Star),
        Token("^\\+", TT::Plus),
        Token("^\\-", TT::Minus),
        Token("^\\+\\+", TT::PlusPlus),
        Token("^\\-\\-", TT::MinusMinus),
        Token("^\\/", TT::Divide),
        Token("^\\`", TT::Power),
        Token("^\\=", TT::Equals),
        Token("^\\;", TT::Semicolon),
        Token("^\\(", TT::Lparen),
        Token("^\\)", TT::Rparen),
        Token("^\\.", TT::Dot),
        Token("^\\[", TT::Lbracket),
        Token("^\\]", TT::Rbracket),
        Token("^,", TT::Comma),
        Token("^\\<", TT::LessThan),
        Token("^\\<\\=", TT::LessThanEqual),
        Token("^\\>", TT::GreaterThan),
        Token("^\\>\\=", TT::GreaterThanEqual),
        Token("^\\=\\=", TT::Equality),
        Token("^\\!\\=", TT::Inequality),
        Token("^\\&\\&", TT::LogicalAnd),
        Token("^\\|\\|", TT::LogicalOr),
        Token("^\\!", TT::LogicalNot),
        Token("^\\>\\>", TT::BitshiftRight),
        Token("^\\<\\<", TT::BitshiftLeft),
        Token("^\\+\\=", TT::PlusEqual),
        Token("^\\*\\=", TT::TimesEqual),
        Token("^\\-\\=", TT::MinusEqual),
        Token("^\\/\\=", TT::DividedByEqual),
        Token("^\\%\\=", TT::ModuloEqual),
        Token("^\\<\\<\\=", TT::LeftshiftEqual),
        Token("^\\>\\>\\=", TT::RightshiftEqual),
        Token("^\\|\\=", TT::OrEqual),
        Token("^\\&\\=", TT::AndEqual),
        Token("^\\^\\=", TT::XorEqual),
        // Constants.
        Token("^'.'", TT::CharacterConstant),
        Token("^\".*\"", TT::StringConstant), // Maybe multiline?
        Token("^(0x[0-9A-Fa-f]+|[0-9]+)(U8|I8|U16|I16|U32|I32|U64|I64)?", TT::IntegerConstant),
        Token("^([0-9](\\.[0-9](e[-0-9])?)?)+", TT::FloatConstant), // TODO HC might allow hex float constants.
        Token("^[_a-zA-Z][_\\w]*", TT::Identifier),
        Token("^[_a-zA-Z][_\\w]*:", TT::Label),
    };

    auto sourcePtr = std::string_view(mSource.data() + mCurPos,
                                      mSource.size() - mCurPos);

    // Get matched tokens and store them in a lexeme.
    hclang::Lexeme maxLexeme;
    auto maxEndLineOffset = mCurLineOffset;
    for(const auto &token : TOKENS) {
        auto [matched, match] = token.match(sourcePtr);
        auto tokSize = match.size();
        if(matched && tokSize > maxLexeme.getTextLength()) {
            auto endPos = mCurPos + tokSize;
            auto endLineNo = mCurLine;
            if(token.maybeMultiline()) {
                // Check matched lexeme for any newline characters.
                // Update the line offset information if a newline is detected.
                maxEndLineOffset = mCurPos;
                for(auto i = match.begin();
                    (i = std::find(i, match.end(), '\n')) != match.end(); i++) {
                    endLineNo++;
                    maxEndLineOffset += (i - match.begin()) + 1;
                }
                if(maxEndLineOffset == mCurPos) {
                    maxEndLineOffset = mCurLineOffset;
                }
            }
            maxLexeme = hclang::Lexeme(match, token.getTokenType(),
                                       mCurLine, mCurPos - mCurLineOffset,
                                       endLineNo, endPos - maxEndLineOffset);
        } else if(matched && token.getTokenType() == TT::Eof) {
            return hclang::Lexeme("", TT::Eof);
        }
    }

    // Make sure there was a match.
    if(maxLexeme == TT::Error) {
        throw std::invalid_argument("No match");
    }

    mCurLine = maxLexeme.getEndLineNumber();
    mCurLineOffset = maxEndLineOffset;
    mCurPos += maxLexeme.getTextLength();

    if(maxLexeme == TT::Space)
        return pull();
    return maxLexeme;
}

// Identifier implementation.

hclang::Identifier::Identifier(const hclang::Lexeme &lexeme) : mId("") {
    if(lexeme != TT::Identifier) {
        throw std::invalid_argument("Not an id");
    }
    mId = lexeme.getText();
}

bool hclang::isType(hclang::TokenType type) {
    switch(type) {
    case TT::I0i:
    case TT::I8i:
    case TT::I16i:
    case TT::I32i:
    case TT::I64i:
    case TT::U0i:
    case TT::U8i:
    case TT::U16i:
    case TT::U32i:
    case TT::U64i:
    case TT::F64:
        return true;
        break;
    default:
        break;
    }
    return false;
}

bool hclang::isOperator(hclang::TokenType type) {
    switch(type) {
    case TT::Plus:
    case TT::Minus:
    case TT::Star:
    case TT::Divide:
    case TT::Modulo:
    case TT::Identifier:
    case TT::Equals:
    case TT::LessThan:
    case TT::LessThanEqual:
    case TT::GreaterThan:
    case TT::GreaterThanEqual:
    case TT::Equality:
    case TT::Inequality:
    case TT::Power:
    case TT::LogicalAnd:
    case TT::LogicalOr:
    case TT::LogicalNot:
    case TT::Ampersand:
    case TT::BitwiseOr:
    case TT::BitwiseXor:
    case TT::BitwiseNot:
    case TT::BitshiftLeft:
    case TT::BitshiftRight:
    case TT::QuestionMark:
    case TT::Sizeof:
    case TT::XorEqual:
    case TT::PlusEqual:
    case TT::MinusEqual:
    case TT::TimesEqual:
    case TT::DividedByEqual:
    case TT::LeftshiftEqual:
    case TT::RightshiftEqual:
    case TT::ModuloEqual:
    case TT::OrEqual:
    case TT::AndEqual:
    case TT::PlusPlus:
    case TT::MinusMinus:
        return true;
        break;
    default:
        break;
    }
    return false;
}

bool hclang::isMaybeUnaryOperator(TokenType type) {
    switch(type) {
    case TT::Plus:
    case TT::Minus:
    case TT::Star:
    case TT::Identifier:
    case TT::Ampersand:
    case TT::BitwiseNot:
    case TT::Sizeof:
    case TT::PlusPlus:
    case TT::MinusMinus:
        return true;
        break;
    default:
        break;
    }
    return false;
}

bool hclang::isKeyword(hclang::TokenType type) {
    switch(type) {
    case TT::IntegerConstant:
    case TT::StringConstant:
    case TT::CharacterConstant:
    case TT::FloatConstant:
        return false;
    default:
        break;
    }
    return !isOperator(type);
}

bool hclang::isSpecifier(hclang::TokenType type) {
    switch(type) {
    case TT::Static:
    case TT::_Extern:
    case TT::Extern:
    case TT::Reg:
    case TT::Noreg:
    case TT::Public:
        return true;
        break;
    default:
        break;
    }
    return false;
}
