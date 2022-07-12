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

enum class ParserState {
    Error,
    Program,
    CompoundStatement,
    Selection, // If, else if, else.
    Accept,
    Declaration,
    DeclarationInitialization,

    // Expression states.
    Expression,
    Assign,
    Cast,
    Additive,
    Multiplicative,
    Shift,
    Relational,
    Equality,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalOr,
    LogicalAnd,
    Ternary,
};

using TT = slang::TokenType;
using PS = ParserState;

struct parseObject {
    ParserState state;
    std::variant<slang::Lexeme, slang::GR> object;
};

using ParseStack = std::list<parseObject>;
namespace fs = slang::fs;

class ParseTreeImpl : public slang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<slang::Lexer> lexer, const slang::Config &config)
        : slang::ParseTree(lexer),mParseStack({}),mLookAhead(),
          mParserState(PS::Program),mConfig(config),mOperatorStack({}),
          mLastLexemeWasOperator(false) {
    }
    virtual ~ParseTreeImpl() = default;
    void createParseTree();

protected:
    ParseStack mParseStack;
    slang::Lexeme mLookAhead;
    ParserState mParserState;
    const slang::Config &mConfig;
    /// Stack to help converting infix notation to postfix notation
    /// for the operators.
    std::list<slang::Operator> mOperatorStack;
    bool mLastLexemeWasOperator;

    // Private parsing functions.
    slang::programData step();
    parseObject popTopToken();
    parseObject popTopExp();
    slang::Lexeme getNextLookahead();
    void pushLookahead();
    slang::programData parseProgram();
    slang::cmpdStmnt parseCompoundStatement();
    inline ParserState peekState(ParserState alt = PS::CompoundStatement) {
        if(mParseStack.empty()) {
            return alt;
        }
        return mParseStack.back().state;
    }

    void reduceDeclarationInitialization();
    void reduceDeclarationStatement();
    void parseDeclarationInitialization();
    void parseDeclaration();
    void reduceRHSMathExpr();
    void reduceVarInit();
    void parseExpression();
    void reduceVarDecl();
    void pushOperator(slang::Operator op);
};

// Static function declarations.

static bool isUnary(slang::Operator op);
static bool isControl(slang::Operator op);
static bool isBinary(slang::Operator op);
static bool isPrefix(slang::Operator op);
static bool isPostfix(slang::Operator op);
static inline parseObject makeParseObject(ParserState ps, slang::GR obj) {
    return parseObject { ps, obj };
}
static inline parseObject makeParseObject(ParserState ps, slang::Lexeme l) {
    return parseObject { ps, l };
}

template<class T, class ... Args>
static inline parseObject makeParseObject<T>(ParserState ps, Args && ...args) {
    return parseObject { ps, std::make_shared<T>(args) };
}

// ParseTree implementation.

slang::ParseTree::ParseTree(std::shared_ptr<slang::Lexer> lexer)
    : mProgram(),mLexer(lexer) {
}

std::shared_ptr<slang::ParseTree> slang::ParseTree::parse(const slang::Config &config,
                                                          std::shared_ptr<slang::Lexer> lexer) {
    // Look ahead token.
    auto result = std::make_shared<ParseTreeImpl>(lexer, config);
    result->createParseTree();

    return result;
}

// ParseTreeImpl implementation.

slang::Lexeme ParseTreeImpl::getNextLookahead() {
    auto lookahead = mLexer->pull(mConfig);

    if(lookahead == TT::Error) {
        throw std::runtime_error("TODO pull"); // TODO make a custom exception class for this.
    }

    mLookAhead = lookahead;
    return lookahead;
}

// slang::Exp ParseTreeImpl::reduceExpr(const slang::Config &config) {
//     // Parse stack of objects we will push onto the stack at the end.
//     ParseStack pushStack = {};
//     // Number of objects to pop off the stack.
//     ParseStack::size_type mNumPop = 0;
//     for(auto it = mParseStack.begin(); it != mParseStack.end(); it++, mNumPop++) {
//         if(std::holds_alternative<slang::Lexeme>(*it)) {
//             const auto &curLexeme = std::get<0>(*it);
//             switch(curLexeme.getTokenType()) {
//             case TT::IntegerConstant:
//                 pushStack.push_back(std::make_shared<slang::IntegerConstant>(curLexeme.getText()));
//                 break;
//             case TT::Equals:
//                 mNumPop++;
//                 // Make an assign with whatever is before the =.
//                 auto frontExp = mParseStack.front();
//                 mParseStack.pop_front();
//                 //pushStack.push_front(std::make_shared<slang::Assign>(, frontExp));
//                 goto end_loop;
//                 break;
//             default:
//                 break;
//             }
//         } else { // Grammar type.

//         }
//     }

// end_loop:
//     mParseStack.erase(mParseStack.begin(), mParseStack.begin() + mNumPop);
//     mParseStack.splice(mParseStack.begin(), pushStack);
// }

void ParseTreeImpl::reduceRHSMathExpr() {
    ParserState retState = peekState();
    using O = slang::Operator;
    for(const auto o : mOperatorStack) {
        auto [_, topExp1] = popTopExp();
        auto [__, topExp2] = popTopExp();
        mParseStack.push_front(makeParseObject<slang::BinaryOperator>(mParserState,
                                                                     o, topExp1, topExp2));
    }

    mParserState = retState;
    mOperatorStack.clear();
    mLastLexemeWasOperator = false;
}

slang::exp ParseTreeImpl::parseExpression() {
    // Uses shunting ford algorithm to convert infix to postfix.
    getNextLookahead();

#define UB_OPERATOR (token, binary, unary) case op: \
    if(mLastLexemeWasOperator) {                    \
        pushOperator(unary);                        \
    } else {                                        \
        pushOperator(binary);                       \
    }                                               \
    break;

    switch(mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
        mParseStack.push_front(makeParseObject<slang::IntegerConstant>(peekState(),
                                                                       mLookAhead));
        mLastLexemeWasOperator = false;
        break;
    case TT::Identifier:
        // TODO push lvalue read.
        mLastLexemeWasOperator = false;
        break;
        UB_OPERATOR(TT::Star, slang::Operator::Multiply, slang::Operator::Dereference);
        UB_OPERATOR(TT::Plus, slang::Operator::Add, slang::Operator::Postivie);
        UB_OPERATOR(TT::Minus, slang::Operator::Minus, slang::Operator::Negative);
    case TT::Divide:
        mOperatorStack.push_back(slang::Operator::Divide);
        break;
    case TT::Modulo:
        mOperatorStack.push_back(slang::Operator::Modulo);
        break;
    case TT::Lparen:
        mOperatorStack.push_back(slang::Operator::Lparen);
        break;
    case TT::Rparen:
        break;
    case TT::Semicolon:
        // Reduce.
        reduceRHSMathExpr();
        {
        }
        break;
    default:
        throw std::runtime_error("Parse expression");
        break;
    }

    return nullptr;
}

void ParseTreeImpl::reduceVarInit() {
    auto [_, exp] = popTopToken();
    auto [retState, decl] = popTopToken();
    mParseStack.push_front(makeParseObject<slang::VariableInitialization>(mParserState, exp, decl));
    mParserState = retState;
}

void ParseTreeImpl::reduceVarDecl() {
    // Reduce from the last tokens.
    auto [_, id] = popTopToken();
    auto [retState, type] = popTopToken();
    mParseStack.push_front(makeParseObject<slang::VariableDeclaration>(mParserState, id));
    mParserState = retState;
}

void ParseTreeImpl::reduceDeclarationStatement() {
    //auto [_, declOrInit] = popTopExp();
}

void ParseTreeImpl::parseDeclaration() {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Identifier:
        pushLookahead();
        break;
    case TT::Equals:
        reduceVarDecl();
        mParserState = PS::DeclarationInitialization;
        pushLookahead();
        mParserState = PS::Expression;
        break;
    case TT::Comma:
        reduceVarDecl();
        break;
    case TT::Semicolon:
        // Reduce VariableDeclaration.
        reduceVarDecl();
        break;
    default:
        throw std::runtime_error("Decl");
        break;
    }
}

void ParseTreeImpl::reduceDeclarationInitialization() {
    auto [_, expr] = popTopToken();
    auto [retState, id] = popTopToken();
    mParseStack.push_front(makeParseObject<slang::VariableDeclaration>(mParserState, id, expr));
    mParserState = retState;
}

void ParseTreeImpl::parseDeclarationInitialization() {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    default:
        throw std::runtime_error("declinit");
        break;
    }
}

slang::programData ParseTreeImpl::parseProgram() {
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
        pushLookahead();
        mParserState = PS::Declaration;
        break;
    case TT::Identifier:
        pushLookahead();
        mParserState = PS::Program;
        break;
    case TT::Eof:
        mParserState = PS::Accept;
        auto [_, topObj] = popTopExp();
        return std::get<slang::GR>(topObj);
        break;
    default:
        break;
    }
    return nullptr;
}

slang::programData ParseTreeImpl::step() {
    switch(mParserState) {
    case PS::Program:
        return parseProgram();
        break;
    case PS::CompoundStatement:
        return parseCompoundStatement();
        break;
    case PS::Declaration:
        parseDeclaration();
        break;
    default:
        throw std::runtime_error("step");
        break;
    }

    return nullptr;
}

void ParseTreeImpl::createParseTree() {
    for(slang::programData pd = step(); mParserState != PS::Accept; pd = step()) {
        if(pd != nullptr) {
            mProgram.add(pd);
        }
    }

    if(mConfig.shouldDumpAst()) {
        // TODO
    }
}

parseObject ParseTreeImpl::popTopToken() {
    auto grammarObj = mParseStack.front();
    mParseStack.pop_front();

    if(grammarObj.object.index() == 0) {
        return grammarObj;
    }
    throw std::runtime_error("poptoptoken");
}

parseObject ParseTreeImpl::popTopExp() {
    auto grammarObj = mParseStack.front();
    mParseStack.pop_front();

    if(grammarObj.object.index() == 1) {
        return grammarObj;
    }
    throw std::runtime_error("poptopexp");
}


void ParseTreeImpl::pushOperator(slang::Operator op) {
    if(mOperatorStack.empty()) {
        mOperatorStack.push_back(op);
    }
    else if(isUnary(mOperatorStack.back())) {
        if(!isUnary(op)) {
            throw std::invalid_argument("unary before binary");
        } else if (isPostfix(op)) {
            throw std::invalid_argument("postfix operator");
        } else if (isControl(op)) {
            throw std::invalid_argument("unary and then control");
        }
    }
    else if(isBinary(mOperatorStack.back())) {
        if(isBinary(op)) {
            throw std::invalid_argument("binary and then binary");
        } else if (isControl(op)) {
            throw std::invalid_argument("binary and then control");
        }
    }
    // TODO handle control.
    mOperatorStack.push_back(op);
    mLastLexemeWasOperator = true;
}

void ParseTreeImpl::pushLookahead() {
    mParseStack.push_front(makeParseObject(mParserState, mLookAhead));
}

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
    using O == slang::Operator;
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
    using O == slang::Operator;
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
