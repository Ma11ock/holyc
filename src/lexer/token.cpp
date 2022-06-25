#include "token.hpp"

#include <string>
#include <regex>
#include <tuple>
#include <string_view>

using svmatch = std::match_results<std::string_view::const_iterator>;
using svsub_match = std::sub_match<std::string_view::const_iterator>;

class Token {
protected:
    std::regex mRegex;
    smul::TokenType mType;
public:
    Token(const std::string &regex, smul::TokenType type) : mRegex(regex),mType(type) {
    }
    ~Token() = default;

   /**
    * 
    */
    std::tuple<bool, std::string_view> match(std::string_view source) const {
        using namespace std::literals;
        svmatch sm;
        if(!std::regex_match(source.begin(), source.end(), sm, mRegex)) {
            return std::make_tuple(false, ""sv);
        }

        return std::make_tuple(true, sm.str());
    }

    smul::TokenType getTokenType() const {
        return mType;
    }

    smul::Lexeme makeLexeme(std::string_view text) const {
        using T = smul::TokenType;

        switch(mType) {
        case T::Identifier:
        case T::Real:
        case T::Integer:
        case T::String:
            return smul::Lexeme(mType);
            break;
        default:
            return smul::Lexeme(std::string(text), mType);
            break;
        }
    }
};

const inline static std::array TOKENS = {
    Token("\\s+", smul::TokenType::NoOp),
    Token("\\*", smul::TokenType::Times),
};



std::tuple<smul::Lexeme, std::string::size_type> smul::lexerPull(std::string_view source) {
    std::array<smul::Lexeme, TOKENS.size()> matchedTokens;
    std::size_t i = 0;
    for(const auto &token : TOKENS) {
        auto [matched, match] = token.match(source);
        if(matched) {
            matchedTokens[i++] = smul::Lexeme(token.getTokenType());
        }
    }

    // Choose which token to use. The token that ate the most characters should be
    // chosen. If multiple tokens ate the same number of characters, choose the one
    // defined first.

    std::string::size_type longestTokenLen = 0;

    smul::Lexeme longestToken = smul::Lexeme("", smul::TokenType::Error);
    for(auto r = matchedTokens.rbegin(); r < matchedTokens.rend(); r++) {
        if(auto rlen = r->getTextLength();
           rlen >= longestTokenLen) {
            longestTokenLen = rlen;
            longestToken = *r;
        }
    }

    return std::make_tuple(longestToken, longestTokenLen);
}



