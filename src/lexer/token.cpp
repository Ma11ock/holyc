#include "token.hpp"

#include <string>
#include <regex>
#include <tuple>
#include <string_view>
#include <sstream>
#include <fstream>
#include <fmt/core.h>
#include <algorithm>
#include <limits>

using TT = slang::TokenType;
using namespace std::string_view_literals;

namespace fs = std::filesystem;

class Token {
protected:
    std::regex mRegex;
    slang::TokenType mType;
public:
    Token(const std::string &regex, slang::TokenType type) :
        mRegex(regex,
               std::regex_constants::icase | std::regex_constants::ECMAScript),
        mType(type) {
    }
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

    slang::TokenType getTokenType() const {
        return mType;
    }

    slang::Lexeme makeLexeme(std::string_view text) const {

        switch(mType) {
        case TT::Identifier:
        case TT::IntegerConstant:
        case TT::StringConstant:
        case TT::CharacterConstant:
            return slang::Lexeme(std::string(text), mType);
            break;
        default:
            return slang::Lexeme(mType);
            break;
        }
    }
};


std::string_view slang::stringifyTokenType(TokenType type) {
    switch(type) {
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
    case TT::HexadecimalConstant:
        return "HexadecimalConstant";
        break;
    case TT::OctalConstant:
        return "OctalConstant";
        break;
    case TT::Space:
        return "Space";
        break;
    }
    throw std::invalid_argument(fmt::format("Invalid token: (integer){}",
                                            static_cast<std::uint32_t>(type)));
}

bool slang::Lexeme::hasText() const {
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

std::string slang::Lexeme::stringify() const {
    if(hasText()) {
        return fmt::format("{}: {}", slang::stringifyTokenType(mType), mText);
    }
    return std::string(slang::stringifyTokenType(mType));
}


// Lexer implementation.

slang::Lexer::Lexer(const fs::path &path) : mSource(""),mSourcePath(path),
                                            mLineOffsets({}),mCurLineNoPtr(0),mCurPos(0) {
    std::ifstream sourceFile(path);
    if(sourceFile.bad() || sourceFile.fail()) {
        throw std::invalid_argument("TODO");
    }

    std::stringstream buffer;
    buffer << sourceFile.rdbuf();
    mSource = buffer.str();

    // Line offset information.
    const static std::regex newlineRegex("\\n");
    using namespace std::literals;
    using svmatch = std::match_results<std::string_view::const_iterator>;
    svmatch sm;
    std::string_view sourcePtr = mSource;
    std::regex_search(sourcePtr.cbegin(), sourcePtr.cend(), sm, newlineRegex);

    mLineOffsets.resize(sm.size() + 1);

    for(std::size_t i = 0; i < sm.size(); i++) {
        mLineOffsets[i] = sm.position(i);
    }
    // Dummy line. Trick that makes calculating line numbers from its relative
    // position to the read head easier.
    mLineOffsets.back() = std::numeric_limits<fileposType>::max();
}

slang::Lexeme slang::Lexer::pull(const slang::Config &config) {
    const static std::array TOKENS = {
        // EOF
        Token("^$", TT::Eof),
        // Whitespace, comments, etc.
        Token("^\\s+|\\/\\/.*|\\/\\/*(\\S\\s)*\\*\\/", TT::Space),
        // Keywords.
        Token("^\\{", TT::LCurlyBracket),
        Token("^\\}", TT::RCurlyBracket),
        Token("^if", TT::If),
        Token("^else", TT::Else),
        Token("^else\\s+if", TT::ElseIf),
        Token("^class", TT::Class),
        Token("^while", TT::While),
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
        // Constants.
        Token("^'.'", TT::CharacterConstant),
        Token("^\".*\"", TT::StringConstant),
        Token("^[-0-9]+", TT::IntegerConstant),
        Token("^[-.0-9]+", TT::FloatConstant),
        Token("^[_a-zA-Z][_\\w]*", TT::Identifier),
        Token("^[_a-zA-Z][_\\w]*:", TT::Label),
        Token("^0x[-0-9A-Fa-f]+", TT::HexadecimalConstant),
        Token("^0[-0-7]+", TT::OctalConstant),
    };

    auto sourcePtr = std::string_view(mSource.data() + mCurPos,
                                      mSource.size() - mCurPos);

    // Get matched tokens and store them in a lexeme.
    std::array<slang::Lexeme, TOKENS.size()> matchedTokens;
    std::size_t i = 0;
    auto curLine = mLineOffsets[mCurLineNoPtr];
    for(const auto &token : TOKENS) {
        auto [matched, match] = token.match(sourcePtr);
        if(matched) {
            auto endPos = mCurPos + match.size();
            auto tmpLinePtr = mCurLineNoPtr;
            while(mLineOffsets[tmpLinePtr + 1] < endPos) {
                tmpLinePtr++;
            }
            fileposType endLineNo = mLineOffsets[tmpLinePtr];
            matchedTokens[i++] = slang::Lexeme(match, token.getTokenType(),
                                               curLine, mCurPos - curLine,
                                               endLineNo, endPos - endLineNo);
        }
    }

    // Make sure there was a match.
    if(i == 0) {
        throw std::invalid_argument("No match");
    }

    // Choose which token to use. The token that ate the most characters should be
    // chosen. If multiple tokens ate the same number of characters, choose the one
    // defined first.

    const auto &maxLexeme = *std::max_element(matchedTokens.begin(), matchedTokens.end(),
                                              [=] (const slang::Lexeme &l1,
                                                   const slang::Lexeme &l2) -> bool {
                                                  return l1.getTextLength() < l2.getTextLength();
                                              });
    auto lexemeLen = maxLexeme.getTextLength();

    // Increment current line number (if necessary).
    while(mLineOffsets[mCurLineNoPtr + 1] < mCurPos) {
        mCurLineNoPtr++;
    }
    mCurPos += lexemeLen;

    if(maxLexeme == TT::Space)
        return pull(config);
    return maxLexeme;
}

// Identifier implementation.

slang::Identifier::Identifier(const slang::Lexeme &lexeme) : mId("") {
    if(lexeme != TT::Identifier) {
        throw std::invalid_argument("Not an id");
    }
    mId = lexeme.getText();
}
