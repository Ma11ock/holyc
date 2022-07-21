/**
 * A recursive ascent parser for Holy C.
 */
#include "parser.hpp"
#include "ast.hpp"

#include "../lexer/lexer.hpp"

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

    virtual void parseSemantics();

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
    hclang::decl declarationSpecifiers(hclang::typeInfo info,
                                       hclang::StorageClass sclass = hclang::StorageClass::Default);
    hclang::decl declarationIdentifier(hclang::typeInfo info, hclang::StorageClass sclass,
                                       hclang::Identifier id);
    hclang::decl declarationInitializationEqual(hclang::typeInfo info, hclang::StorageClass sclass,
                                                hclang::Identifier id);

};

#define parseRet(funcall) {                     \
        auto mCurLex = mLookAhead;              \
        auto result = funcall;                  \
        result->setLexeme(mCurLex);             \
        return result;                          \
    }

static bool isUnary(hclang::Operator op);
static bool isControl(hclang::Operator op);
static bool isBinary(hclang::Operator op);
static bool isPrefix(hclang::Operator op);
static bool isPostfix(hclang::Operator op);
static hclang::typeInfo getTypeFrom(const hclang::Lexeme &l);

// ParseTree implementation.

hclang::ParseTree::ParseTree(std::shared_ptr<hclang::Lexer> lexer,
                             const hclang::Config &config)
    : mProgram(),mLexer(lexer),mConfig(config) {
}

std::shared_ptr<hclang::ParseTree> hclang::ParseTree::parseSyntax(const hclang::Config &config,
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
    case TT::U32i:
    case TT::U16i:
    case TT::U8i:
    case TT::U0i:
    case TT::I64i:
    case TT::I32i:
    case TT::I16i:
    case TT::I8i:
    case TT::I0i:
        parseRet(declarationSpecifiers(getTypeFrom(mLookAhead)));
    case TT::Eof:
    case TT::Semicolon:
        return nullptr;
        break;
    default:
        throw std::runtime_error(
            fmt::format("Error got a {}: {}",
                        hclang::stringifyTokenType(mLookAhead.getTokenType()),
                        mLookAhead.getText()));
        break;
    }
}

hclang::decl ParseTreeImpl::declarationSpecifiers(hclang::typeInfo info,
                                                  hclang::StorageClass sclass) {
    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::Reg:
        parseRet(declarationSpecifiers(info, sclass | hclang::StorageClass::Reg));
        break;
    case TT::Noreg:
        parseRet(declarationSpecifiers(info, sclass | hclang::StorageClass::Noreg));
        break;
    case TT::Public:
        parseRet(declarationSpecifiers(info, sclass | hclang::StorageClass::Public));
        break;
    case TT::Extern:
        parseRet(declarationSpecifiers(info, sclass | hclang::StorageClass::Extern));
        break;
    case TT::_Extern:
        parseRet(declarationSpecifiers(info, sclass | hclang::StorageClass::_Extern));
        break;
    case TT::Identifier:
        // TODO check if type.
        if(auto ti = getTypeFrom(mLookAhead);
           ti.type == hclang::HCType::Invalid) {
            parseRet(declarationIdentifier(info, sclass, hclang::Identifier(mLookAhead)));
        } else if(info.type != hclang::HCType::Typeless) {
            throw std::runtime_error(fmt::format("Expected id, got type '{}'",
                                                 mLookAhead.getText()));
        } else {
            parseRet(declarationSpecifiers(ti, sclass));
        }
        break;
    default:
        break;
    }
    throw std::runtime_error("declspec");
}

hclang::decl ParseTreeImpl::declarationIdentifier(hclang::typeInfo info,
                                                  hclang::StorageClass sclass,
                                                  hclang::Identifier id) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Semicolon:
        pushTokenToQueue();
        return std::make_shared<hclang::VariableDeclaration>(id, info);
        break;
    case TT::Equals:
        parseRet(declarationInitializationEqual(info, sclass, id));
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
        auto i = std::make_shared<hclang::IntegerConstant>(mLookAhead);
        return std::make_shared<hclang::VariableInitialization>(id, info, i);
    }
    break;
    default:
        break;
    }
    throw std::invalid_argument("decliniteq");
}

void ParseTreeImpl::parseSemantics() {
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


hclang::typeInfo getTypeFrom(const hclang::Lexeme &l) {
    using ti = hclang::typeInfo;
    using hct = hclang::HCType;
    switch(l.getTokenType()) {
    case TT::U64i:
        return ti { ""sv, nullptr, hct::U64i };
        break;
    case TT::U32i:
        return ti { ""sv, nullptr, hct::U32i };
        break;
    case TT::U16i:
        return ti { ""sv, nullptr, hct::U16i };
        break;
    case TT::U8i:
        return ti { ""sv, nullptr, hct::U8i };
        break;
    case TT::U0i:
        return ti { ""sv, nullptr, hct::U0i };
        break;
    case TT::I64i:
        return ti { ""sv, nullptr, hct::I64i };
        break;
    case TT::I32i:
        return ti { ""sv, nullptr, hct::I32i };
        break;
    case TT::I16i:
        return ti { ""sv, nullptr, hct::I16i };
        break;
    case TT::I8i:
        return ti { ""sv, nullptr, hct::I8i };
        break;
    case TT::I0i:
        return ti { ""sv, nullptr, hct::I0i };
        break;
        // TODO pointers and user types.
    default:
        break;
    }

    return ti { ""sv, nullptr, hct::Invalid };
}
