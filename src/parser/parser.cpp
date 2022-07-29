/**
 * A recursive ascent parser for Holy C.
 */
#include "parser.hpp"
#include "ast.hpp"

#include "../lexer/lexer.hpp"
#include "symbols.hpp"

#include <list>
#include <variant>
#include <string_view>
#include <vector>
#include <optional>
#include <stack>
#include <queue>
#include <deque>

using TT = hclang::TokenType;
using O = hclang::Operator;
using hct = hclang::HCType;

using namespace std::string_view_literals;

class ParseTreeImpl : public hclang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<hclang::Lexer> lexer, const hclang::Config &config)
        : hclang::ParseTree(lexer, config),mLookAhead(),mReduceQueue()
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
        void push(hclang::declRef expr);

        bool lastObjWasOp() const { return mLastObjWasOp; }
        /**
         *
         * Note: assumes that the expression fed to the parser is valid.
         */
        hclang::exp reduce();

        bool isEmpty() const {
            return mOperatorStack.empty() && mExpressionQueue.empty();
        }
    protected:
        /// Stack to help converting infix notation to postfix notation
        /// for the operators.
        std::stack<operatorLex> mOperatorStack;
        /// Stack of expressions reduced with shunting yard.
        std::queue<std::variant<hclang::declRef, hclang::exp, operatorLex>> mExpressionQueue;
        /// True if the last pushed value was an operator (except for postifx operators).
        bool mLastObjWasOp;

        std::optional<operatorLex> pushOp(hclang::Operator op, const hclang::Lexeme &l);
        void flushOperators();
    };
    /// Current lookahead object.
    hclang::Lexeme mLookAhead;
    /// Queue used for lookahead tokens read when reducing.
    std::deque<hclang::Lexeme> mReduceQueue;
    /// Symbol table generated during parsing.
    hclang::SymbolTable<hclang::decl> mSymbolTable;

    // Private parsing functions.
    inline void pushTokenToBack(const hclang::Lexeme &l) { mReduceQueue.push_back(l); }
    inline void pushTokenToBack() { pushTokenToBack(mLookAhead); }
    inline void pushTokenToFront(const hclang::Lexeme &l) { mReduceQueue.push_front(l); }
    inline void pushTokenToFront() { pushTokenToFront(mLookAhead); }
    inline hclang::Lexeme getRealNextLookahead() {
        mLookAhead = mLexer->pull();
        return mLookAhead;
    }
    hclang::Lexeme getNextLookahead();
    hclang::GR programStart();
    hclang::decl declarationSpecifiers(std::optional<hclang::typeInfo> info = std::nullopt,
                                       hclang::StorageClass sclass = hclang::StorageClass::Default);
    hclang::decl declarationIdentifier(hclang::typeInfo info, hclang::StorageClass sclass,
                                       hclang::Identifier id);
    hclang::varInit declarationInitializationEqual(hclang::typeInfo info, hclang::StorageClass sclass,
                                                hclang::Identifier id);
    hclang::declStmnt declarationStatementStart();
    hclang::cmpdStmnt compoundStatementStart(bool &nextIsFunc);

    hclang::exp expressionCompoundStart();
    hclang::exp expressionArgumentStart();
    void expressionStart(YardShunter &ys);

    hclang::ret returnStart();

    hclang::ifStmnt ifStart();
    std::list<hclang::elIf> elseIfStart();
    hclang::cmpdStmnt elseStart();
};

#define parseRet(funcall) {                     \
        auto mCurLex = mLookAhead;              \
        auto result = funcall;                  \
        result->setLexeme(mCurLex);             \
        return result;                          \
    }

#define pushTableRet(funcall) {                     \
        auto dec = funcall;                         \
        mSymbolTable.add(dec->getIdRef(), dec);     \
        return dec;                                 \
    }

static std::optional<hclang::typeInfo> getTypeFrom(const hclang::Lexeme &l);

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
        mLookAhead = mReduceQueue.front();
        mReduceQueue.pop_front();
    } else {
        mLookAhead = mLexer->pull();
    }

    if(mLookAhead == TT::Error) {
        throw std::runtime_error("TODO pull"); // TODO make a custom exception class for this.
    }

    return mLookAhead;
}

hclang::ret ParseTreeImpl::returnStart() {
    getNextLookahead();
    switch(mLookAhead.getTokenType()) {
    case TT::Return:
        return hclang::makeRet(expressionCompoundStart(), mLookAhead);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::ifStmnt ParseTreeImpl::ifStart() {
    getNextLookahead();
    bool nextIsFunc = false;
    switch(mLookAhead.getTokenType()) {
    case TT::If:
        return hclang::makeIf(expressionArgumentStart(), compoundStatementStart(nextIsFunc),
                              elseIfStart(), elseStart(), mLookAhead);
        break;
    default:
        break;
    }
    return nullptr;
}

std::list<hclang::elIf> ParseTreeImpl::elseIfStart() {
    getNextLookahead();
    bool nextIsFunc = false;
    std::list<hclang::elIf> result;
    while(mLookAhead.getTokenType() == TT::ElseIf) {
        result.push_back(hclang::makeElIf(expressionArgumentStart(), compoundStatementStart(nextIsFunc)));
    }

    pushTokenToFront();
    return result;
}

hclang::cmpdStmnt ParseTreeImpl::elseStart() {
    getNextLookahead();
    bool nextIsFunc = false;
    switch(mLookAhead.getTokenType()) {
    case TT::Else:
        return compoundStatementStart(nextIsFunc);
        break;
    default:
        // "Else" is optional, so if it doesn't appear we push it to the front.
        pushTokenToFront();
        break;
    }
    return nullptr;
}

hclang::cmpdStmnt ParseTreeImpl::compoundStatementStart(bool &nextIsFunc) {
    auto result = hclang::makeCmpdStmnt();
    bool shouldContinue = true;
    bool bracketed = false;
    mSymbolTable.pushTable();

    if(getNextLookahead() == TT::LCurlyBracket) {
        bracketed = true;
    } else {
        pushTokenToFront();
    }

    while(shouldContinue) {
        getNextLookahead();
        auto type = mLookAhead.getTokenType();
        if(getTypeFrom(mLookAhead) || hclang::isSpecifier(type)) {
            pushTokenToFront();
            for(hclang::Lexeme curLex;
                (curLex = getRealNextLookahead()) != TT::LCurlyBracket;
                pushTokenToBack(curLex)) {
                switch(curLex.getTokenType()) {
                case TT::Equals:
                case TT::Semicolon:
                    pushTokenToBack(curLex);
                    result->add(declarationStatementStart());
                    goto end_decl_loop;
                    break;
                default:
                    break;
                }
            }
            nextIsFunc = true;
            shouldContinue = false;
        end_decl_loop:
            continue;
        }

        switch(type) {
        case TT::RCurlyBracket:
            if(!bracketed) {
                // TODO error
            } else {
                shouldContinue = false;
            }
            break;
        break;
        case TT::Semicolon:
            break;
        case TT::If:
            pushTokenToFront();
            result->add(ifStart());
            break;
        case TT::Identifier:
            // Check if variable or function.
            if(auto sym = mSymbolTable.find(mLookAhead.getText());
               sym) {
                pushTokenToFront();
                result->add(expressionCompoundStart());
            }
            break;
        case TT::Eof:
            shouldContinue = false;
            break;
        case TT::Return:
            pushTokenToFront();
            result->add(returnStart());
            break;
        default:
            throw std::runtime_error(fmt::format("cmpstmt: {}, {}", mLookAhead.getTokenType(), mLookAhead.getText()));
            break;
        }
    }

    mSymbolTable.popTable();
    return result;
}

void ParseTreeImpl::parseTokens() {
    programStart();

}


hclang::GR ParseTreeImpl::programStart() {

    mSymbolTable.pushTable();
    mSymbolTable["result"] = hclang::makeVarDecl(hclang::Identifier("reuslt"),
                                                 hclang::typeInfo {});
    bool shouldContinue = true;
    while(shouldContinue) {
        bool nextIsFunc = false;
        auto statements = compoundStatementStart(nextIsFunc);
        mProgram.add(statements);
        if(!statements || statements->isEmpty()) {
            if(nextIsFunc) {
                // TODO
                // mProgram.add(func);
            } else {
                shouldContinue = false;
            }
        }
    }

    mSymbolTable.popTable();
    return nullptr;
}

hclang::decl ParseTreeImpl::declarationSpecifiers(std::optional<hclang::typeInfo> info,
                                                  hclang::StorageClass sclass) {
    getNextLookahead();
    // Check for type.
    if(auto ti = getTypeFrom(mLookAhead); ti) {
        parseRet(declarationSpecifiers(ti, sclass));
    }
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
        // Variable name.
        if(!info) {
            throw std::runtime_error("no type");
        }
        parseRet(declarationIdentifier(*info, sclass, hclang::Identifier(mLookAhead)));
        // TODO check if type for better error message.
        break;
    default:
        break;
    }
    throw std::runtime_error(fmt::format("declspec: {}\n", mLookAhead.getTokenType()));
}

hclang::decl ParseTreeImpl::declarationIdentifier(hclang::typeInfo info,
                                                  hclang::StorageClass sclass,
                                                  hclang::Identifier id) {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Semicolon:
        pushTokenToFront();
        pushTableRet(hclang::makeVarDecl(id, info, sclass, mLookAhead));
        break;
    case TT::Equals:
        parseRet(declarationInitializationEqual(info, sclass, id));
        break;
    case TT::Comma:
        // TODO
        pushTokenToFront();
        pushTableRet(hclang::makeVarDecl(id, info, sclass, mLookAhead));
        break;
    default:
        break;
    }
    throw std::invalid_argument("declid");
}

hclang::varInit ParseTreeImpl::declarationInitializationEqual(hclang::typeInfo info,
                                                              hclang::StorageClass sclass,
                                                              hclang::Identifier id) {
    pushTableRet(hclang::makeVarInit(id, info, expressionCompoundStart()));
}

hclang::declStmnt ParseTreeImpl::declarationStatementStart() {

    auto result = std::make_shared<hclang::DeclarationStatement>(declarationSpecifiers());

    auto info = result->getType().value();
    auto sclass = result->getStorageClass().value_or(hclang::StorageClass::Default);
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

void ParseTreeImpl::expressionStart(ParseTreeImpl::YardShunter &ys) {
    switch(mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
        ys.push(hclang::makeIntConst(mLookAhead));
        break;
    case TT::Identifier: // TODO could be function, need further parsing.
        ys.push(hclang::makeDeclRef(hclang::Identifier(mLookAhead),
                                    hclang::DeclarationReference::Type::LValue,
                                    mSymbolTable, mLookAhead));
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
    case TT::Equals:
        ys.push(O::Assignment, mLookAhead);
        break;
    case TT::Lparen:
        ys.push(O::Leftparen, mLookAhead);
        break;
    case TT::Rparen:
        ys.push(O::Rightparen, mLookAhead);
        break;
    default:
        break;
    }
}

hclang::exp ParseTreeImpl::expressionArgumentStart() {
    YardShunter ys;
    // Left recursion.
    while(getNextLookahead() != TT::Rparen) {
        expressionStart(ys);
    }

    ys.push(O::Rightparen, mLookAhead);

    // Reduce remaining operations.
    return ys.reduce();
}

hclang::exp ParseTreeImpl::expressionCompoundStart() {
    YardShunter ys;
    // Left recursion.
    while(getNextLookahead() != TT::Semicolon) {
        expressionStart(ys);
    }

    pushTokenToFront();

    // Reduce remaining operations.
    return ys.reduce();
}

void ParseTreeImpl::parseSemantics() {
    hclang::semanticContext sc = { std::nullopt };
    mProgram.parseSemantics(sc);
    if(mConfig.shouldDumpAst()) {
        mProgram.pprint();
    }
}

// YardShunter implementation.

void ParseTreeImpl::YardShunter::push(hclang::declRef expr) {
    mExpressionQueue.push(expr);
    mLastObjWasOp = false;
}

void ParseTreeImpl::YardShunter::push(hclang::exp expr) {
    mExpressionQueue.push(expr);
    mLastObjWasOp = false;
}

void ParseTreeImpl::YardShunter::push(hclang::Operator op,
                                      const hclang::Lexeme &l) {
    if(op == O::Rightparen) {
        if(mOperatorStack.empty()) {
            return;
        }
        operatorLex o = mOperatorStack.top();
        while(o.op != O::Leftparen) {
            mOperatorStack.pop();
            if(mOperatorStack.empty()) {
                throw std::runtime_error("paren pop");
            }
            mExpressionQueue.push(o);
            o = mOperatorStack.top();
        }
        return;
    }

    auto maybePoppedOperator = pushOp(op, l);

    if(!maybePoppedOperator) {
        return;
    }

    auto poppedOperator = maybePoppedOperator.value();
    mExpressionQueue.push(ParseTreeImpl::operatorLex(maybePoppedOperator.value()));
    mLastObjWasOp = !hclang::operatorIsPostfix(op);
}

std::optional<ParseTreeImpl::operatorLex>
ParseTreeImpl::YardShunter::pushOp(hclang::Operator op,
                                   const hclang::Lexeme &l) {
    if(mOperatorStack.empty() || op == O::Leftparen) {
        mOperatorStack.emplace(l, op);
        return std::nullopt;
    }
    // TODO associativity.
    if(auto topOp = mOperatorStack.top();
       hclang::getPrecedence(topOp.op) >= hclang::getPrecedence(op) &&
       topOp.op != O::Leftparen) {
        mOperatorStack.pop();
        mOperatorStack.emplace(l, op);
        return std::make_optional<ParseTreeImpl::operatorLex>(topOp);
    }
    mOperatorStack.emplace(l, op);
    return std::nullopt;
}


hclang::exp ParseTreeImpl::YardShunter::reduce() {
    flushOperators();
    std::stack<std::variant<hclang::declRef, hclang::exp>> postfixEvalStack;

    bool onlyLvalue = false;
    while(!mExpressionQueue.empty()) {
        auto expOrOp = mExpressionQueue.front();
        mExpressionQueue.pop();


        if(std::holds_alternative<hclang::exp>(expOrOp)) {
            postfixEvalStack.push(std::get<hclang::exp>(expOrOp));
            continue;
        }
        else if(std::holds_alternative<hclang::declRef>(expOrOp)) {
            if(postfixEvalStack.empty()) {
                onlyLvalue = true;
            }
            postfixEvalStack.push(std::get<hclang::declRef>(expOrOp));
            continue;
        }

        onlyLvalue = false;
        auto o = std::get<operatorLex>(expOrOp);
        switch(hclang::operatorArgs(o.op)) {
        case 0:
            break;
        case 1:
        {
            auto texpr = postfixEvalStack.top();
            hclang::exp expr = nullptr;
            if(std::holds_alternative<hclang::declRef>(texpr)) {
                expr = hclang::makeL2Rval(std::get<hclang::declRef>(texpr));
            } else {
                expr = std::get<hclang::exp>(texpr);
            }
            postfixEvalStack.pop();
            postfixEvalStack.push(std::make_shared<hclang::UnaryOperator>(o.op, expr, o.lex));
        }
            break;
        case 2:
        {
            auto trhs = postfixEvalStack.top();
            hclang::exp rhs = nullptr;
            // Convert Lvalues to Rvalues.
            if(std::holds_alternative<hclang::declRef>(trhs)) {
                rhs = hclang::makeL2Rval(std::get<hclang::declRef>(trhs));
            } else {
                rhs = std::get<hclang::exp>(trhs);
            }
            postfixEvalStack.pop();

            hclang::exp lhs = nullptr;
            auto tlhs = postfixEvalStack.top();
            if(std::holds_alternative<hclang::declRef>(tlhs)) {
                if(hclang::isAssignment(o.op)) {
                    lhs = std::get<hclang::declRef>(tlhs);
                } else {
                    lhs = hclang::makeL2Rval(std::get<hclang::declRef>(tlhs));
                }
            } else {
                lhs = std::get<hclang::exp>(tlhs);
            }

            postfixEvalStack.pop();
            postfixEvalStack.push(hclang::makeBinOp(o.op, lhs, rhs, o.lex));
        }
            break;
        case 3:
            break;
        default:
            return nullptr;
            break;
        }

    }

    if(postfixEvalStack.empty()) {
        return nullptr;
    }

    auto result = postfixEvalStack.top();
    if(std::holds_alternative<hclang::declRef>(result)) {
        if(onlyLvalue) {
            return hclang::makeL2Rval(std::get<hclang::declRef>(result));
        }
        return std::get<hclang::declRef>(result);
    }
    return std::get<hclang::exp>(result);
}

void ParseTreeImpl::YardShunter::flushOperators() {
    while(!mOperatorStack.empty()) {
        auto o = mOperatorStack.top();
        mOperatorStack.pop();

        mExpressionQueue.push(o);
    }
}

// Static functions.

std::optional<hclang::typeInfo> getTypeFrom(const hclang::Lexeme &l) {
    using ti = hclang::typeInfo;
    switch(l.getTokenType()) {
    case TT::U64i:
        return ti { ""sv, nullptr, hct::U64i };
        break;
    case TT::U32i:
        return ti { ""sv, nullptr, hct::U32i };
        break;
    case TT::U16i:
        return  ti { ""sv, nullptr, hct::U16i };
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

    return std::nullopt;
}
