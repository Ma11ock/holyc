#include "token.hpp"

#include <string>
#include <regex>
#include <tuple>
#include <string_view>

#include <fmt/core.h>

using svmatch = std::match_results<std::string_view::const_iterator>;
using svsub_match = std::sub_match<std::string_view::const_iterator>;


using TT = simul::TokenType;

class Token {
protected:
    std::regex mRegex;
    simul::TokenType mType;
public:
    Token(const std::string &regex, simul::TokenType type) :
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
        svmatch sm;
        if(!std::regex_search(source.cbegin(), source.cend(), sm, mRegex)) {
            return std::make_tuple(false, ""sv);
        }

        const char *first = sm[0].first;
        const char *last = sm[0].second;
        return std::make_tuple(true, std::string_view(first,
                                                      static_cast<std::size_t>(last - first)));
    }

    simul::TokenType getTokenType() const {
        return mType;
    }

    simul::Lexeme makeLexeme(std::string_view text) const {

        switch(mType) {
        case TT::Identifier:
        case TT::RealConstant:
        case TT::IntegerConstant:
        case TT::StringConstant:
        case TT::CharacterConstant:
            return simul::Lexeme(std::string(text), mType);
            break;
        default:
            return simul::Lexeme(mType);
            break;
        }
    }
};


std::tuple<simul::Lexeme, simul::lexemeLen> simul::lexerPull(std::string_view source) {

    const static std::array TOKENS = {
        // Whitespace, comments, etc.
        Token("^\\s+|^\\{([\\s\\S]*)\\}", simul::TokenType::Space),
        // Keywords.
        Token("^begin", simul::TokenType::Begin),
        Token("^end", simul::TokenType::End),
        Token("^if", simul::TokenType::If),
        Token("^then", simul::TokenType::Then),
        Token("^else", simul::TokenType::Else),
        Token("^simulation", simul::TokenType::Simulation),
        Token("^class", simul::TokenType::Class),
        Token("^virtual", simul::TokenType::Virtual),
        Token("^is", simul::TokenType::Is),
        Token("^ref", simul::TokenType::Ref),
        Token("^new", simul::TokenType::New),
        Token("^array", simul::TokenType::Array),
        Token("^do", simul::TokenType::Do),
        Token("^step", simul::TokenType::Step),
        Token("^until", simul::TokenType::Until),
        Token("^activate", simul::TokenType::Activate),
        Token("^while", simul::TokenType::While),
        Token("^for", simul::TokenType::For),
        Token("^true", simul::TokenType::True),
        Token("^false", simul::TokenType::False),
        Token("^boolean", simul::TokenType::Boolean),
        Token("^integer", simul::TokenType::Integer),
        Token("^real", simul::TokenType::Real),
        Token("^text", simul::TokenType::Text),
        Token("^name", simul::TokenType::Name),
        // Operators and grammars.
        Token("^\\:", simul::TokenType::Colon),
        Token("^\\*", simul::TokenType::Times),
        Token("^\\+", simul::TokenType::Plus),
        Token("^\\-", simul::TokenType::Minus),
        Token("^\\/", simul::TokenType::Divide),
        Token("^\\&", simul::TokenType::Strcat),
        Token("^:\\=", simul::TokenType::Assign),
        Token("^\\;", simul::TokenType::Semicolon),
        Token("^\\(", simul::TokenType::Lparen),
        Token("^\\)", simul::TokenType::Rparen),
        Token("^\\.", simul::TokenType::Dot),
        Token("^\\[", simul::TokenType::Lbracket),
        Token("^\\]", simul::TokenType::Rbracket),
        Token("^\\<", simul::TokenType::LessThan),
        Token("^\\<\\=", simul::TokenType::LessThanEqual),
        Token("^\\>", simul::TokenType::GreaterThan),
        Token("^\\>\\=", simul::TokenType::GreaterThanEqual),
        Token("^,", simul::TokenType::Comma),
        // Constants.
        Token("^'.'", simul::TokenType::CharacterConstant),
        Token("^\".*\"", simul::TokenType::StringConstant),
        Token("^[-0-9]+", simul::TokenType::IntegerConstant),
        Token("^[-.0-9]+", simul::TokenType::RealConstant),
        Token("^[_a-zA-Z][_\\w]*", simul::TokenType::Identifier),
    };


    if(source.empty()) {
        return std::make_tuple(simul::Lexeme(TokenType::Eof), 0);
    }

    std::array<std::tuple<simul::Lexeme, std::string_view>, TOKENS.size()> matchedTokens;
    std::size_t i = 0;
    for(const auto &token : TOKENS) {
        auto [matched, match] = token.match(source);
        if(matched) {
            matchedTokens[i++] = std::make_tuple(simul::Lexeme(token.getTokenType()),
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

    simul::lexemeLen longestTokenLen = 0;
    simul::Lexeme longestToken = simul::Lexeme("", simul::TokenType::Error);
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
        longestToken = simul::Lexeme(std::string(longestMatchStr),
                                     longestToken.getTokenType());
    }

    return std::make_tuple(longestToken, longestTokenLen);
}



std::string_view simul::stringifyTokenType(TokenType type) {
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
    case TT::Simulation:
        return "Simulation";
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

bool simul::Lexeme::hasText() const {
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

std::string simul::Lexeme::stringify() const {
    if(hasText()) {
        return fmt::format("{}: {}", simul::stringifyTokenType(mType), mText);
    }
    return std::string(simul::stringifyTokenType(mType));
}
