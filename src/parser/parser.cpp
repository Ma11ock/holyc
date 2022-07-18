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

using TT = hclang::TokenType;

using namespace std::string_view_literals;

class ParseTreeImpl : public hclang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<hclang::Lexer> lexer, const hclang::Config &config)
        : hclang::ParseTree(lexer, config),mLookAhead(),mReduceQueue({}),
          mOperatorStack({}) { }
    virtual ~ParseTreeImpl() = default;

    void parseTokens();

protected:
    /// Current lookahead object.
    hclang::Lexeme mLookAhead;
    /// Queue used for lookahead tokens read when reducing.
    std::list<hclang::Lexeme> mReduceQueue;
    /// Stack to help converting infix notation to postfix notation
    /// for the operators.
    std::list<hclang::Operator> mOperatorStack;

    // Private parsing functions.
    inline void pushTokenToQueue() { mReduceQueue.push_back(mLookAhead); }
    hclang::Lexeme getNextLookahead();
    hclang::GR programStart();
    hclang::decl declarationStart(hclang::typeInfo info);
    hclang::decl declarationSpecifiers(hclang::typeInfo info, hclang::StorageClass sclass);
    hclang::decl declarationIdentifier(hclang::typeInfo info, hclang::StorageClass sclass,
                                      hclang::Identifier id);
    hclang::decl declarationInitializationEqual(hclang::typeInfo info, hclang::StorageClass sclass,
                                               hclang::Identifier id);

};

static bool isUnary(hclang::Operator op);
static bool isControl(hclang::Operator op);
static bool isBinary(hclang::Operator op);
static bool isPrefix(hclang::Operator op);
static bool isPostfix(hclang::Operator op);

// ParseTree implementation.

hclang::ParseTree::ParseTree(std::shared_ptr<hclang::Lexer> lexer,
                            const hclang::Config &config)
    : mProgram(),mLexer(lexer),mConfig(config) {
}

std::shared_ptr<hclang::ParseTree> hclang::ParseTree::parse(const hclang::Config &config,
                                                          std::shared_ptr<hclang::Lexer> lexer) {
    // Look ahead token.
    auto result = std::make_shared<ParseTreeImpl>(lexer, config);
    result->parseTokens();

    return result;
}

// ParseTreeImpl implementation.

hclang::Lexeme ParseTreeImpl::getNextLookahead() {
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

    if(mConfig.shouldDumpAst()) {
        mProgram.pprint();
    }
}

hclang::GR ParseTreeImpl::programStart() {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::U64i:
        return declarationStart(hclang::typeInfo { ""sv, hclang::HCType::U64i });
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

hclang::decl ParseTreeImpl::declarationStart(hclang::typeInfo info) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Identifier:
        return declarationIdentifier(info, hclang::StorageClass::Default,
                                     hclang::Identifier(mLookAhead));
        break;
    case TT::Reg:
        return declarationSpecifiers(info, hclang::StorageClass::Reg);
        break;
    case TT::Noreg:
        return declarationSpecifiers(info, hclang::StorageClass::Noreg);
        break;
    case TT::Public:
        return declarationSpecifiers(info, hclang::StorageClass::Public);
        break;
    case TT::Extern:
        return declarationSpecifiers(info, hclang::StorageClass::Extern);
        break;
    case TT::_Extern:
        return declarationSpecifiers(info, hclang::StorageClass::_Extern);
        break;
    default:
        break;
    }
    throw std::runtime_error("declstart");
}

hclang::decl ParseTreeImpl::declarationSpecifiers(hclang::typeInfo info, hclang::StorageClass sclass) {
    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::Reg:
        return declarationSpecifiers(info, sclass | hclang::StorageClass::Reg);
        break;
    case TT::Noreg:
        return declarationSpecifiers(info, sclass | hclang::StorageClass::Noreg);
        break;
    case TT::Public:
        return declarationSpecifiers(info, sclass | hclang::StorageClass::Public);
        break;
    case TT::Extern:
        return declarationSpecifiers(info, sclass | hclang::StorageClass::Extern);
        break;
    case TT::_Extern:
        return declarationSpecifiers(info, sclass | hclang::StorageClass::_Extern);
        break;
    case TT::Identifier:
        return declarationIdentifier(info, sclass, hclang::Identifier(mLookAhead));
        break;
    default:
        break;
    }
    throw std::runtime_error("declspec");
}

hclang::decl ParseTreeImpl::declarationIdentifier(hclang::typeInfo info, hclang::StorageClass sclass,
                                                 hclang::Identifier id) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Semicolon:
        pushTokenToQueue();
        return std::make_shared<hclang::VariableDeclaration>(id);
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

hclang::decl ParseTreeImpl::declarationInitializationEqual(hclang::typeInfo info,
                                                          hclang::StorageClass sclass,
                                                          hclang::Identifier id) {
    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
    {
        auto i = std::make_shared<hclang::IntegerConstant>(mLookAhead.getText());
        return std::make_shared<hclang::VariableInitialization>(id, i);
    }
        break;
    default:
        break;
    }
    throw std::invalid_argument("decliniteq");
}

// Static functions.

bool isUnary(hclang::Operator op) {
    using O = hclang::Operator;
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

bool isControl(hclang::Operator op) {
    using O = hclang::Operator;
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

bool isBinary(hclang::Operator op) {
    return !isUnary(op) && !isControl(op);
}

bool isPrefix(hclang::Operator op) {
    return !isPostfix(op);
}

bool isPostfix(hclang::Operator op) {
    using O = hclang::Operator;
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
