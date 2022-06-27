#include "token.hpp"

#include <string>
#include <regex>
#include <tuple>
#include <string_view>

#include <fmt/core.h>

using TT = slang::TokenType;

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
        case TT::RealConstant:
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


std::tuple<slang::Lexeme, slang::lexemeLen> slang::lexerPull(std::string_view source) {

    const static std::array TOKENS = {
        // EOF
        Token("^$", TT::Eof),
        // Whitespace, comments, etc.
        Token("^\\s+|^(\\!|comment)[^;]*;", TT::Space),
        // Keywords.
        Token("^begin", TT::Begin),
        Token("^end", TT::End),
        Token("^if", TT::If),
        Token("^then", TT::Then),
        Token("^else", TT::Else),
        Token("^slangation", TT::Slangation),
        Token("^class", TT::Class),
        Token("^virtual", TT::Virtual),
        Token("^is", TT::Is),
        Token("^ref", TT::Ref),
        Token("^new", TT::New),
        Token("^array", TT::Array),
        Token("^do", TT::Do),
        Token("^step", TT::Step),
        Token("^until", TT::Until),
        Token("^activate", TT::Activate),
        Token("^while", TT::While),
        Token("^for", TT::For),
        Token("^true", TT::True),
        Token("^false", TT::False),
        Token("^boolean", TT::Boolean),
        Token("^integer", TT::Integer),
        Token("^real", TT::Real),
        Token("^text", TT::Text),
        Token("^name", TT::Name),
        Token("^go to", TT::Goto),
        // Operators and grammars.
        Token("^\\:", TT::Colon),
        Token("^\\*", TT::Times),
        Token("^\\+", TT::Plus),
        Token("^\\-", TT::Minus),
        Token("^\\%", TT::Divide),
        Token("^\\^", TT::Power),
        Token("^\\&", TT::Strcat),
        Token("^:\\=", TT::Assign),
        Token("^\\;", TT::Semicolon),
        Token("^\\(", TT::Lparen),
        Token("^\\)", TT::Rparen),
        Token("^\\.", TT::Dot),
        Token("^\\[", TT::Lbracket),
        Token("^\\]", TT::Rbracket),
        Token("^\\<", TT::LessThan),
        Token("^\\<\\=", TT::LessThanEqual),
        Token("^\\>", TT::GreaterThan),
        Token("^\\>\\=", TT::GreaterThanEqual),
        Token("^\\=", TT::Equality),
        Token("^\\<\\>", TT::Inequality),
        Token("^,", TT::Comma),
        // Constants.
        Token("^'.'", TT::CharacterConstant),
        Token("^\".*\"", TT::StringConstant),
        Token("^[-0-9]+", TT::IntegerConstant),
        Token("^[-.0-9]+", TT::RealConstant),
        Token("^[_a-zA-Z][_\\w]*", TT::Identifier),
    };

    // Get matched tokens and store them in a lexeme. We use a tuple with a
    // string_view just to avoid allocations.
    std::array<std::tuple<slang::Lexeme, std::string_view>, TOKENS.size()> matchedTokens;
    std::size_t i = 0;
    for(const auto &token : TOKENS) {
        auto [matched, match] = token.match(source);
        if(matched) {
            matchedTokens[i++] = std::make_tuple(slang::Lexeme(token.getTokenType()),
                                                 match);
        }
    }

    // Make sure there was a match.
    if(i == 0) {
        throw std::invalid_argument("No match");
    }

    // Choose which token to use. The token that ate the most characters should be
    // chosen. If multiple tokens ate the same number of characters, choose the one
    // defined first.

    slang::lexemeLen longestTokenLen = 0;
    slang::Lexeme longestToken = slang::Lexeme("", slang::TokenType::Error);
    std::string_view longestMatchStr;

    for(auto it = matchedTokens.rbegin() + matchedTokens.size() - i;
        it < matchedTokens.rend(); it++) {
        auto &[r, s] = *it;
        if(auto rlen = s.size();
           rlen >= longestTokenLen) {
            longestTokenLen = rlen;
            longestToken = r;
            longestMatchStr = s;
        }
    }

    if(longestToken.hasText()) {
        longestToken = slang::Lexeme(std::string(longestMatchStr),
                                     longestToken.getTokenType());
    }

    return std::make_tuple(longestToken, longestTokenLen);
}



std::string_view slang::stringifyTokenType(TokenType type) {
    switch(type) {
    case TT::Space:
        return "Space";
        break;
    case TT::Eof:
        return "EOF";
        break;
    case TT::Error:
        return "Error";
        break;
    case TT::Begin:
        return "Begin";
        break;
    case TT::End:
        return "End";
        break;
    case TT::If:
        return "If";
        break;
    case TT::Then:
        return "Then";
        break;
    case TT::Else:
        return "Else";
        break;
    case TT::Procedure:
        return "Procedure";
        break;
    case TT::Slangation:
        return "Slangation";
        break;
    case TT::Class:
        return "Class";
        break;
    case TT::Virtual:
        return "Virtual";
        break;
    case TT::Is:
        return "Is";
        break;
    case TT::Ref:
        return "Ref";
        break;
    case TT::New:
        return "New";
        break;
    case TT::Array:
        return "Array";
        break;
    case TT::Do:
        return "Do";
        break;
    case TT::Step:
        return "Step";
        break;
    case TT::Name:
        return "Name";
        break;
    case TT::Until:
        return "Until";
        break;
    case TT::Activate:
        return "Activate";
        break;
    case TT::While:
        return "While";
        break;
    case TT::For:
        return "For";
        break;
    case TT::True:
        return "True";
        break;
    case TT::False:
        return "False";
        break;
    case TT::Boolean:
        return "Boolean";
        break;
    case TT::Integer:
        return "Integer";
        break;
    case TT::Real:
        return "Real";
        break;
    case TT::Text:
        return "Text";
        break;
    case TT::Goto:
        return "Go to";
        break;
    case TT::Plus:
        return "Plus";
        break;
    case TT::Minus:
        return "Minus";
        break;
    case TT::Times:
        return "Times";
        break;
    case TT::Divide:
        return "Divide";
        break;
    case TT::Power:
        return "Power";
        break;
    case TT::Identifier:
        return "Identifier";
        break;
    case TT::Assign:
        return "Assign";
        break;
    case TT::Strcat:
        return "Strcat";
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
    case TT::CharacterConstant:
        return "CharacterConstant";
        break;
    case TT::RealConstant:
        return "RealConstant";
        break;
    case TT::IntegerConstant:
        return "IntegerConstant";
        break;
    case TT::StringConstant:
        return "StringConstant";
        break;
    }
    throw std::invalid_argument("Kek!");
}

bool slang::Lexeme::hasText() const {
    switch(mType) {
    case TT::Identifier:
    case TT::RealConstant:
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
