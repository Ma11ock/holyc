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
#include <optional>
#include <stack>
#include <queue>

using TT = hclang::TokenType;
using O = hclang::Operator;

using namespace std::string_view_literals;

class ParseTreeImpl : public hclang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<hclang::Lexer> lexer, const hclang::Config &config)
        : hclang::ParseTree(lexer, config),mLookAhead(),mReduceQueue({})
          { }
    virtual ~ParseTreeImpl() = default;

    void parseTokens();

    virtual void parseSemantics();

protected:
    struct operatorLex {
        hclang::Lexeme lex;
        hclang::Operator op;

        operatorLex(const hclang::Lexeme &l, hclang::Operator op) : lex(l),op(op) {}
        ~operatorLex() = default;
    };
    /**
     * Cache class for expression parsing with the Shunting Yard algorithm.
     */
    class YardShunter {
    public:
        YardShunter() : mOperatorStack(),mExpressionQueue(),mLastObjWasOp(false) {}
        ~YardShunter() = default;
        void push(hclang::Operator op, const hclang::Lexeme &l);
        void push(hclang::exp expr);

        bool lastObjWasOp() const { return mLastObjWasOp; }
        /**
         *
         * Note: assumes that the expression fed to the parser is valid.
         */
        hclang::exp reduce();
    protected:
        /// Stack to help converting infix notation to postfix notation
        /// for the operators.
        std::stack<operatorLex> mOperatorStack;
        /// Stack of expressions reduced with shunting yard.
        std::queue<std::variant<hclang::exp, operatorLex>> mExpressionQueue;
        /// True if the last pushed value was an operator (except for postifx operators).
        bool mLastObjWasOp;

        std::optional<operatorLex> pushOp(hclang::Operator op, const hclang::Lexeme &l);
        void flushOperators();
    };
    /// Current lookahead object.
    hclang::Lexeme mLookAhead;
    /// Queue used for lookahead tokens read when reducing.
    std::list<hclang::Lexeme> mReduceQueue;

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
    hclang::declStmnt declarationStatementStart(hclang::typeInfo info,
                                                hclang::StorageClass sclass = hclang::StorageClass::Default);

    hclang::exp expressionStart(hclang::TokenType endToken,
                                bool pushEndToken = true);

};

#define parseRet(funcall) {                     \
        auto mCurLex = mLookAhead;              \
        auto result = funcall;                  \
        result->setLexeme(mCurLex);             \
        return result;                          \
    }

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
        parseRet(declarationStatementStart(getTypeFrom(mLookAhead)));
    case TT::Semicolon:
        return programStart();
        break;
    case TT::Eof:
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
        return std::make_shared<hclang::VariableDeclaration>(id, info, mLookAhead);
        break;
    case TT::Equals:
        parseRet(declarationInitializationEqual(info, sclass, id));
        break;
    case TT::Comma:
        pushTokenToQueue();
        return std::make_shared<hclang::VariableDeclaration>(id, info, mLookAhead);
        break;
    default:
        break;
    }
    throw std::invalid_argument("declid");
}

hclang::decl ParseTreeImpl::declarationInitializationEqual(hclang::typeInfo info,
                                                           hclang::StorageClass sclass,
                                                           hclang::Identifier id) {
    return std::make_shared<hclang::VariableInitialization>(id, info,
                                                            expressionStart(TT::Semicolon, true));
}

hclang::declStmnt ParseTreeImpl::declarationStatementStart(hclang::typeInfo info,
                                                           hclang::StorageClass sclass) {
    auto result = std::make_shared<hclang::DeclarationStatement>(declarationSpecifiers(info, sclass));

    // Left recursion.
    while(mLookAhead == TT::Comma) {
        // TODO sclass might not be passed here, or only certain specifiers are.
        result->push(declarationSpecifiers(info, sclass));
    }

    switch(mLookAhead.getTokenType()) {
    case TT::Semicolon:
        return result;
        break;
    default:
        break;
    }
    throw std::runtime_error(fmt::format("Got token {}", mLookAhead.getText()));
}

hclang::exp ParseTreeImpl::expressionStart(hclang::TokenType endToken,
                                           bool pushEndToken) {
    YardShunter ys;
    // Left recursion.
    while(getNextLookahead() != endToken) {
        switch(mLookAhead.getTokenType()) {
        case TT::IntegerConstant:
            ys.push(std::make_shared<hclang::IntegerConstant>(mLookAhead));
            break;
        case TT::Star:
            // TODO unary *.
            ys.push(ys.lastObjWasOp() ? O::Dereference : O::Multiply, mLookAhead);
            break;
        case TT::Divide:
            ys.push(O::Divide, mLookAhead);
            break;
        case TT::Plus:
            if(!ys.lastObjWasOp()) {
                ys.push(O::Add, mLookAhead);
            }
            // Ignore positive.
            break;
        case TT::Minus:
            ys.push(ys.lastObjWasOp() ? O::Negative : O::Subtract, mLookAhead);
            break;
        case TT::MinusMinus:
            ys.push(ys.lastObjWasOp() ? O::PrefixMinusMinus : O::PostfixMinusMinus, mLookAhead);
            break;
        case TT::PlusPlus:
            ys.push(ys.lastObjWasOp() ? O::PrefixPlusPlus : O::PostfixPlusPlus, mLookAhead);
            break;
        case TT::Ampersand:
            ys.push(ys.lastObjWasOp() ? O::AddressOf : O::BitwiseAnd, mLookAhead);
            break;
        case TT::BitwiseOr:
            ys.push(O::BitwiseOr, mLookAhead);
            break;
        case TT::BitwiseNot:
            ys.push(O::BitwiseNot, mLookAhead);
            break;
        case TT::AndEqual:
            ys.push(O::AndAssignment, mLookAhead);
            break;
        case TT::OrEqual:
            ys.push(O::OrAssignment, mLookAhead);
            break;
        case TT::XorEqual:
            ys.push(O::XorAssignment, mLookAhead);
            break;
        case TT::LeftshiftEqual:
            ys.push(O::LeftshiftAssignment, mLookAhead);
            break;
        case TT::RightshiftEqual:
            ys.push(O::RightshiftAssignment, mLookAhead);
            break;
        case TT::PlusEqual:
            ys.push(O::AddAssignment, mLookAhead);
            break;
        case TT::MinusEqual:
            ys.push(O::SubtractAssignment, mLookAhead);
            break;
        case TT::TimesEqual:
            ys.push(O::MultiplyAssignment, mLookAhead);
            break;
        case TT::DividedByEqual:
            ys.push(O::DivideAssignment, mLookAhead);
            break;
        case TT::ModuloEqual:
            ys.push(O::ModuloAssignment, mLookAhead);
            break;
        default:
            break;
        }
    }

    if(pushEndToken) {
        pushTokenToQueue();
    }

    // Reduce remaining operations.
    return ys.reduce();
}

void ParseTreeImpl::parseSemantics() {
}

// YardShunter implementation.

void ParseTreeImpl::YardShunter::push(hclang::exp expr) {
    mExpressionQueue.push(expr);
    mLastObjWasOp = false;
}

void ParseTreeImpl::YardShunter::push(hclang::Operator op,
                                      const hclang::Lexeme &l) {
    auto maybePoppedOperator = pushOp(op, l);

    if(!maybePoppedOperator) {
        return;
    }

    auto poppedOperator = maybePoppedOperator.value();
    mExpressionQueue.emplace(ParseTreeImpl::operatorLex(maybePoppedOperator.value()));
    mLastObjWasOp = !hclang::operatorIsPostfix(op);
}

std::optional<ParseTreeImpl::operatorLex>
ParseTreeImpl::YardShunter::pushOp(hclang::Operator op,
                                   const hclang::Lexeme &l) {
    if(mOperatorStack.empty()) {
        mOperatorStack.emplace(l, op);
        return std::nullopt;
    }
    // TODO associativity.
    if(auto topOp = mOperatorStack.top();
       hclang::getPrecedence(topOp.op) >= hclang::getPrecedence(op)) {
        mOperatorStack.pop();
        mOperatorStack.emplace(l, op);
        return std::make_optional<ParseTreeImpl::operatorLex>(topOp);
    }
    mOperatorStack.emplace(l, op);
    return std::nullopt;
}

hclang::exp ParseTreeImpl::YardShunter::reduce() {
    flushOperators();
    std::stack<hclang::exp> postfixEvalStack;

    while(!mExpressionQueue.empty()) {
        auto expOrOp = mExpressionQueue.front();
        mExpressionQueue.pop();


        if(std::holds_alternative<hclang::exp>(expOrOp)) {
            postfixEvalStack.push(std::get<hclang::exp>(expOrOp));
            continue;
        }

        auto o = std::get<operatorLex>(expOrOp);
        switch(hclang::operatorArgs(o.op)) {
        case 0:
            break;
        case 1:
        {
            auto expr = postfixEvalStack.top();
            postfixEvalStack.pop();
            postfixEvalStack.push(std::make_shared<hclang::UnaryOperator>(o.op, expr, o.lex));
        }
            break;
        case 2:
        {
            auto rhs = postfixEvalStack.top();
            postfixEvalStack.pop();
            auto lhs = postfixEvalStack.top();
            postfixEvalStack.pop();
            postfixEvalStack.push(std::make_shared<hclang::BinaryOperator>(o.op, lhs, rhs, o.lex));
        }
            break;
        case 3:
            break;
        default:
            return nullptr;
            break;
        }

    }

    return postfixEvalStack.top();
}

void ParseTreeImpl::YardShunter::flushOperators() {
    while(!mOperatorStack.empty()) {
        auto o = mOperatorStack.top();
        mOperatorStack.pop();

        mExpressionQueue.push(o);
    }
}

// Static functions.

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
