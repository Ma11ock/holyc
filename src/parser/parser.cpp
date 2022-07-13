/**
 * A recursive ascent parser for Holy C.
 */
#include "parser.hpp"
#include "ast.hpp"

#include "../lexer/token.hpp"

#include <list>
#include <variant>
#include <string_view>
#include <vector>

using TT = slang::TokenType;

using namespace std::string_view_literals;

class ParseTreeImpl : public slang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<slang::Lexer> lexer, const slang::Config &config)
        : slang::ParseTree(lexer),mConfig(config),mLookAhead(),mReduceQueue({}),
          mOperatorStack({}) { }
    virtual ~ParseTreeImpl() = default;

    void parseTokens();

protected:
    const slang::Config &mConfig;
    /// Current lookahead object.
    slang::Lexeme mLookAhead;
    /// Queue used for lookahead tokens read when reducing.
    std::list<slang::Lexeme> mReduceQueue;
    /// Stack to help converting infix notation to postfix notation
    /// for the operators.
    std::list<slang::Operator> mOperatorStack;

    // Private parsing functions.
    inline void pushTokenToQueue() { mReduceQueue.push_back(mLookAhead); }
    slang::Lexeme getNextLookahead();
    slang::GR programStart();
    slang::decl declarationStart(slang::typeInfo info);
    slang::decl declarationSpecifiers(slang::typeInfo info, slang::StorageClass sclass);
    slang::decl declarationIdentifier(slang::typeInfo info, slang::StorageClass sclass,
                                      slang::Identifier id);
    slang::decl declarationInitializationEqual(slang::typeInfo info, slang::StorageClass sclass,
                                               slang::Identifier id);

};

static bool isUnary(slang::Operator op);
static bool isControl(slang::Operator op);
static bool isBinary(slang::Operator op);
static bool isPrefix(slang::Operator op);
static bool isPostfix(slang::Operator op);

// ParseTree implementation.

slang::ParseTree::ParseTree(std::shared_ptr<slang::Lexer> lexer)
    : mProgram(),mLexer(lexer) {
}

std::shared_ptr<slang::ParseTree> slang::ParseTree::parse(const slang::Config &config,
                                                          std::shared_ptr<slang::Lexer> lexer) {
    // Look ahead token.
    auto result = std::make_shared<ParseTreeImpl>(lexer, config);
    result->parseTokens();

    return result;
}

// ParseTreeImpl implementation.

slang::Lexeme ParseTreeImpl::getNextLookahead() {
    if(!mReduceQueue.empty()) {
        mLookAhead = mReduceQueue.back();
        mReduceQueue.pop_back();
    } else {
        mLookAhead = mLexer->pull();
    }

    if(mLookAhead == TT::Error) {
        throw std::runtime_error("TODO pull"); // TODO make a custom exception class for this.
    }

    return mLookAhead;
}

void ParseTreeImpl::parseTokens() {
    for(auto gr = programStart(); gr != nullptr; gr = programStart()) {
        mProgram.add(gr);
    }
}

slang::GR ParseTreeImpl::programStart() {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::U64i:
        return declarationStart(slang::typeInfo { ""sv, slang::HCType::U64i });
        break;
    case TT::Eof:
    case TT::Semicolon:
        return nullptr;
        break;
    default:
        throw std::runtime_error("Error");
        break;
    }
}

slang::decl ParseTreeImpl::declarationStart(slang::typeInfo info) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Identifier:
        return declarationIdentifier(info, slang::StorageClass::Default,
                                     slang::Identifier(mLookAhead));
        break;
    case TT::Reg:
        return declarationSpecifiers(info, slang::StorageClass::Reg);
        break;
    case TT::Noreg:
        return declarationSpecifiers(info, slang::StorageClass::Noreg);
        break;
    case TT::Public:
        return declarationSpecifiers(info, slang::StorageClass::Public);
        break;
    case TT::Extern:
        return declarationSpecifiers(info, slang::StorageClass::Extern);
        break;
    case TT::_Extern:
        return declarationSpecifiers(info, slang::StorageClass::_Extern);
        break;
    default:
        break;
    }
    throw std::runtime_error("declstart");
}

slang::decl ParseTreeImpl::declarationSpecifiers(slang::typeInfo info, slang::StorageClass sclass) {
    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::Reg:
        return declarationSpecifiers(info, sclass | slang::StorageClass::Reg);
        break;
    case TT::Noreg:
        return declarationSpecifiers(info, sclass | slang::StorageClass::Noreg);
        break;
    case TT::Public:
        return declarationSpecifiers(info, sclass | slang::StorageClass::Public);
        break;
    case TT::Extern:
        return declarationSpecifiers(info, sclass | slang::StorageClass::Extern);
        break;
    case TT::_Extern:
        return declarationSpecifiers(info, sclass | slang::StorageClass::_Extern);
        break;
    case TT::Identifier:
        return declarationIdentifier(info, sclass, slang::Identifier(mLookAhead));
        break;
    default:
        break;
    }
    throw std::runtime_error("declspec");
}

slang::decl ParseTreeImpl::declarationIdentifier(slang::typeInfo info, slang::StorageClass sclass,
                                                 slang::Identifier id) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Semicolon:
        pushTokenToQueue();
        return std::make_shared<slang::VariableDeclaration>(id);
        break;
    case TT::Equals:
        return declarationInitializationEqual(info, sclass, id);
        break;
    case TT::Comma:
        break;
    default:
        break;
    }
    throw std::invalid_argument("declid");
}

slang::decl ParseTreeImpl::declarationInitializationEqual(slang::typeInfo info,
                                                          slang::StorageClass sclass,
                                                          slang::Identifier id) {

    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
    {
        auto i = std::make_shared<slang::IntegerConstant>(mLookAhead.getText());
        return std::make_shared<slang::VariableInitialization>(id, i);
    }
        break;
    default:
        break;
    }
    throw std::invalid_argument("decliniteq");
}

// Static functions.

bool isUnary(slang::Operator op) {
    using O = slang::Operator;
    switch(op) {
    case O::LogicalNot:
    case O::Negative:
    case O::Postivie:
    case O::Dereference:
    case O::AddressOf:
    case O::BitwiseNot:
        return true;
        break;
    default:
        break;
    }

    return false;
}

bool isControl(slang::Operator op) {
    using O =slang::Operator;
    switch(op) {
    case O::Lparen:
    case O::Rparen:
        return true;
        break;
    default:
        break;
    }
    return false;
}

bool isBinary(slang::Operator op) {
    return !isUnary(op) && !isControl(op);
}

bool isPrefix(slang::Operator op) {
    return !isPostfix(op);
}

bool isPostfix(slang::Operator op) {
    using O =slang::Operator;
    switch(op) {
    case O::PostfixPlusPlus:
    case O::PostfixMinusMinus:
        return true;
        break;
    default:
        break;
    }
    return false;
}
