#include "parser.hpp"
#include "ast.hpp"

#include "../lexer/token.hpp"

#include <list>
#include <variant>
#include <regex>
#include <string_view>
#include <fstream>

enum class ParserState {
    Error,
    Program,
    Block,
    UnlabelledBlock,
    BlockHead,
    CompoundStatement,
    UnlabelledCompound,
    CompoundTail,

    Accept,
};

using TT = slang::TokenType;
using PS = ParserState;
using ParseObject = std::variant<slang::Lexeme, slang::Exp>;
using ParseStack = std::list<ParseObject>;
namespace fs = slang::fs;

static class ParseTreeImpl : public slang::ParseTree {
public:
    ParseTreeImpl(const fs::path &path)
        : slang::ParseTree(path),mParseStack({}),mLookAhead(),
          mParserState(PS::Program),mSourcePtr("") {
    }
    virtual ~ParseTreeImpl() = default;
    slang::GR getNextState();

protected:
    ParseStack mParseStack;
    slang::Lexeme mLookAhead;
    ParserState mParserState;
    std::string_view mSourcePtr;

    // Private parsing functions.
    slang::Lexeme getNextLookahead();
    slang::Exp parseBoolexpr();
    slang::StatementList parseStatementList();
};

// ParseTree implementation.

slang::ParseTree::ParseTree(const fs::path &path)
    : mSource(""),mSourcePath(path),mLineOffsets({}),mExpressions({}) {
    std::ifstream sourceFile(path);
    if(sourceFile.bad() || sourceFile.fail()) {
        throw std::invalid_argument("Not good :/");
    }

    std::stringstream buffer;
    buffer << sourceFile.rdbuf();
    mSource = buffer.str();
}

std::shared_ptr<slang::ParseTree> slang::ParseTree::parse(const fs::path &path) {
    // Look ahead token.
    auto result = std::make_shared<ParseTreeImpl>(path);
    slang::GR obj = nullptr;
    std::string_view sourcePtr = result->mSource;
    while((obj = result->getNextState()) != nullptr) {
        // if(parserState == PS::Error) {
        //     throw std::runtime_error("TODO"); // TODO make a custom exception class for this.
        // }
        result->mExpressions.push_back(obj);
    }
    return result;
}

void slang::ParseTree::setupLineOffsetInfo() {
    const static std::regex newlineRegex("\\n");
    using namespace std::literals;
    using svmatch = std::match_results<std::string_view::const_iterator>;
    svmatch sm;
    std::string_view sourcePtr = mSource;
    std::regex_search(sourcePtr.cbegin(), sourcePtr.cend(), sm, newlineRegex);

    mLineOffsets.resize(sm.size());

    for(std::size_t i = 0; i < sm.size(); i++) {
        mLineOffsets[i] = sm.position(i);
    }
}

// ParseTreeImpl implementation.

slang::Lexeme ParseTreeImpl::getNextLookahead() {
    auto lookahead = slang::Lexeme::pull(mSourcePtr);
    auto lexemeLen = lookahead.getTextLength();

    if(lookahead == TT::Error) {
        throw std::runtime_error("TODO pull"); // TODO make a custom exception class for this.
    }

    mSourcePtr = std::string_view(mSourcePtr.begin() + lexemeLen,
                                  mSourcePtr.size() - lexemeLen);
    mLookAhead = lookahead;
    return lookahead;
}


slang::Exp ParseTreeImpl::parseBoolexpr() {
    switch(mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
        return std::make_shared<slang::IntegerConstant>(mLookAhead.getText());
        break;
    default:
        break;
    }
    throw std::runtime_error("TODO boolexpr");
}

slang::StatementList ParseTreeImpl::parseStatementList() {
    return {};
}

slang::GR ParseTreeImpl::getNextState() {
    switch(mLookAhead.getTokenType()) {
    case TT::If:
        return std::make_shared<slang::If>(parseBoolexpr(),
                                           parseStatementList());
        break;
    default:
        break;
    }
    return nullptr;
}
