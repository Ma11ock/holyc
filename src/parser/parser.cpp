/**
 * A recursive ascent parser for Holy C.
 */
#include "parser.hpp"

#include <deque>
#include <list>
#include <optional>
#include <queue>
#include <stack>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "../lexer/lexer.hpp"
#include "ast.hpp"
#include "symbols.hpp"

using TT = hclang::TokenType;
using O = hclang::Operator;
using hct = hclang::HCType;

using namespace std::string_view_literals;

class ParseTreeImpl : public hclang::ParseTree {
    public:
    ParseTreeImpl(std::shared_ptr<hclang::Lexer> lexer, const hclang::Config &config)
        : hclang::ParseTree(lexer, config),
          mLookAhead(),
          mReduceQueue(),
          mSymbolTable(),
          mLabels() {
    }

    virtual ~ParseTreeImpl() = default;

    void parseTokens();

    virtual void parseSemantics();

    protected:
    struct operatorLex {
        hclang::Lexeme lex;
        hclang::Operator op;

        operatorLex(const hclang::Lexeme &l, hclang::Operator op) : lex(l), op(op) {
        }

        ~operatorLex() = default;
    };

    /**
     * Cache class for expression parsing with the Shunting Yard algorithm.
     */
    class YardShunter {
        public:
        YardShunter() : mOperatorStack(), mExpressionQueue(), mLastObjWasOp(true) {
        }

        ~YardShunter() = default;
        void push(hclang::Operator op, const hclang::Lexeme &l);
        void push(hclang::exp expr);
        void push(hclang::declRef expr);

        inline bool lastObjWasOp() const {
            return mLastObjWasOp;
        }

        /**
         *
         * Note: assumes that the expression fed to the parser is valid.
         */
        hclang::exp reduce();

        inline bool isEmpty() const {
            return mOperatorStack.empty() && mExpressionQueue.empty();
        }

        protected:
        /// Stack to help converting infix notation to postfix notation
        /// for the operators.
        std::stack<operatorLex> mOperatorStack;
        /// Stack of expressions reduced with shunting yard.
        std::queue<std::variant<hclang::declRef, hclang::exp, operatorLex>> mExpressionQueue;
        /// True if the last pushed value was an operator (except for postifx
        /// operators).
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
    /// Symbol table for goto labels.
    std::unordered_map<hclang::Identifier, hclang::label, hclang::identifierHashFun> mLabels;

    // Private parsing functions.
    inline void pushTokenToBack(const hclang::Lexeme &l) {
        mReduceQueue.push_back(l);
    }

    inline void pushTokenToBack() {
        pushTokenToBack(mLookAhead);
    }

    inline void pushTokenToFront(const hclang::Lexeme &l) {
        mReduceQueue.push_front(l);
    }

    inline void pushTokenToFront() {
        pushTokenToFront(mLookAhead);
    }

    inline hclang::Lexeme getRealNextLookahead() {
        mLookAhead = mLexer->pull();
        return mLookAhead;
    }

    hclang::Lexeme getNextLookahead();
    hclang::GR programStart();
    hclang::decl declarationSpecifiers(std::optional<hclang::typeInfo> info = std::nullopt,
                                       hclang::StorageClass sclass = hclang::StorageClass::Default,
                                       bool isFuncArg = false);
    hclang::decl declarationIdentifier(hclang::typeInfo info, hclang::StorageClass sclass,
                                       hclang::Identifier id);
    hclang::varInit declarationInitializationEqual(hclang::typeInfo info,
                                                   hclang::StorageClass sclass,
                                                   hclang::Identifier id);
    hclang::declStmnt declarationStatementStart();
    hclang::cmpdStmnt compoundStatementStart();

    hclang::exp expressionCompoundStart(bool stopComma = false);
    hclang::exp expressionArgumentStart();
    hclang::expList expressionList(bool semicolonEnds = false, bool lparenStarts = true);

    void expressionStart(YardShunter &ys);

    hclang::ret returnStart();

    hclang::ifStmnt ifStart();
    std::list<hclang::elIf> elseIfStart();
    hclang::cmpdStmnt elseStart();

    hclang::whileStmnt whileStart();
    hclang::forStmnt forStart();

    hclang::gotoStmnt gotoStart();
    hclang::label labelStart(bool fromGoto = false);

    std::list<hclang::varDecl> funcDeclList();
    hclang::funcDecl funcDeclStart(hclang::typeInfo info, hclang::StorageClass sclass,
                                   hclang::Identifier id);
};

#define parseRet(funcall)           \
    {                               \
        auto mCurLex = mLookAhead;  \
        auto result = funcall;      \
        result->setLexeme(mCurLex); \
        return result;              \
    }

#define pushTableRet(funcall)                   \
    {                                           \
        auto dec = funcall;                     \
        mSymbolTable.add(dec->getIdRef(), dec); \
        return dec;                             \
    }

static std::optional<hclang::typeInfo> getTypeFrom(const hclang::Lexeme &l);

// ParseTree implementation.

hclang::ParseTree::ParseTree(std::shared_ptr<hclang::Lexer> lexer, const hclang::Config &config)
    : mProgram(), mLexer(lexer), mConfig(config) {
}

std::shared_ptr<hclang::ParseTree> hclang::ParseTree::parseSyntax(
    const hclang::Config &config, std::shared_ptr<hclang::Lexer> lexer) {
    // Look ahead token.
    auto result = std::make_shared<ParseTreeImpl>(lexer, config);
    result->parseTokens();

    return result;
}

// ParseTreeImpl implementation.

hclang::Lexeme ParseTreeImpl::getNextLookahead() {
    if (!mReduceQueue.empty()) {
        mLookAhead = mReduceQueue.front();
        mReduceQueue.pop_front();
    } else {
        mLookAhead = mLexer->pull();
    }

    if (mLookAhead == TT::Error) {
        throw std::runtime_error("TODO pull");  // TODO make a custom exception class for this.
    }

    return mLookAhead;
}

hclang::ret ParseTreeImpl::returnStart() {
    getNextLookahead();
    switch (mLookAhead.getTokenType()) {
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
    switch (mLookAhead.getTokenType()) {
    case TT::If:
        return hclang::makeIf(expressionArgumentStart(), compoundStatementStart(), elseIfStart(),
                              elseStart(), mLookAhead);
        break;
    default:
        pushTokenToFront();
        break;
    }
    return nullptr;
}

std::list<hclang::elIf> ParseTreeImpl::elseIfStart() {
    std::list<hclang::elIf> result;
    while (getNextLookahead() == TT::ElseIf) {
        result.push_back(hclang::makeElIf(expressionArgumentStart(), compoundStatementStart()));
    }

    pushTokenToFront();
    return result;
}

hclang::cmpdStmnt ParseTreeImpl::elseStart() {
    getNextLookahead();
    switch (mLookAhead.getTokenType()) {
    case TT::Else:
        return compoundStatementStart();
        break;
    default:
        pushTokenToFront();
        break;
    }
    return nullptr;
}

hclang::whileStmnt ParseTreeImpl::whileStart() {
    getNextLookahead();
    switch (mLookAhead.getTokenType()) {
    case TT::While:
        return hclang::makeWhile(expressionArgumentStart(), compoundStatementStart(), false,
                                 mLookAhead);
        break;
    case TT::Do: {
        auto startLex = mLookAhead;
        auto body = compoundStatementStart();
        if (getNextLookahead() == TT::While) {
            auto conditional = expressionArgumentStart();
            if (getNextLookahead() != TT::Semicolon) {
                throw std::invalid_argument("do while syntax (semicolon)");
            }
            pushTokenToFront();
            return hclang::makeWhile(conditional, body, true, startLex);
        } else {
            throw std::invalid_argument("do while syntax error");
        }
    } break;
    default:
        pushTokenToFront();
        break;
    }
    return nullptr;
}

hclang::forStmnt ParseTreeImpl::forStart() {
    getNextLookahead();
    switch (mLookAhead.getTokenType()) {
    case TT::For:
        return hclang::makeFor(expressionList(true, true), expressionCompoundStart(),
                               expressionList(false, false), compoundStatementStart(), mLookAhead);
        break;
        break;
    default:
        pushTokenToFront();
        break;
    }
    return nullptr;
}

hclang::gotoStmnt ParseTreeImpl::gotoStart() {
    getNextLookahead();
    auto startLex = mLookAhead;
    switch (startLex.getTokenType()) {
    case TT::Goto:
        if (auto label = labelStart(true); label) {
            if (getNextLookahead() != TT::Semicolon) {
                throw std::invalid_argument("goto: expected semicolon");
            }
            return hclang::makeGoto(label, startLex);
        } else {
            throw std::invalid_argument("goto: bad label");
        }
        break;
    default:
        break;
    }

    return nullptr;
}

hclang::label ParseTreeImpl::labelStart(bool fromGoto) {
    auto startLex = getNextLookahead();
    switch (startLex.getTokenType()) {
    case TT::Identifier:
        getNextLookahead();
        pushTokenToFront();
        if (mLookAhead == (fromGoto ? TT::Semicolon : TT::Colon)) {
            auto result = hclang::makeLabel(hclang::Identifier(startLex), startLex);
            // Add self to the symbol table.
            mLabels[startLex] = result;
            return result;
        } else {
            pushTokenToFront(startLex);  // Push the identifier.
        }
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::cmpdStmnt ParseTreeImpl::compoundStatementStart() {
    auto result = hclang::makeCmpdStmnt();
    bool bracketed = false;
    if (getNextLookahead() == TT::LCurlyBracket) {
        bracketed = true;
    } else {
        pushTokenToFront();
    }
    // Left recursion for compound statement.
    for (bool shouldContinue = true, expectSemiColon = false; shouldContinue;) {
        getNextLookahead();

        if (expectSemiColon) {
            if (mLookAhead != TT::Semicolon) {
                throw std::invalid_argument(
                    fmt::format("expect semicolon, got {}({})", mLookAhead.getText(), mLookAhead));
            } else {
                expectSemiColon = false;
                continue;
            }
        }

        // Check if a declaration or a function definition.
        auto typeLex = mLookAhead;
        auto type = typeLex.getTokenType();
        if (getTypeFrom(typeLex) || hclang::isSpecifier(type)) {
            pushTokenToFront();
            auto decl = declarationStatementStart();
            bool isFunction = *(decl->getDeclType()) == hclang::Declaration::Type::Function;
            expectSemiColon = !isFunction;
            result->add(decl);
            // TODO check if top level. If not, throw. If so, continue.
            continue;
        }

        // Check if an expression.
        if (hclang::isMaybeUnaryOperator(type) && type != TT::Identifier) {
            pushTokenToFront();
            result->add(expressionCompoundStart());
            // Left recursion.
            while (getNextLookahead() == TT::Comma) {
                result->add(expressionCompoundStart());
            }
            pushTokenToFront();
            continue;
        }

        switch (type) {
        case TT::RCurlyBracket:
            if (!bracketed) {
                throw std::invalid_argument("No {");
            } else {
                shouldContinue = false;
            }
            break;
            break;
        case TT::Switch:
            break;
        case TT::If:
            pushTokenToFront();
            result->add(ifStart());
            break;
        case TT::While:
            pushTokenToFront();
            result->add(whileStart());
            break;
        case TT::For:
            pushTokenToFront();
            result->add(forStart());
            break;
        case TT::Semicolon:
            break;
        case TT::Identifier:
            // Check if variable or function.
            if (mSymbolTable.find(mLookAhead.getText())) {
                pushTokenToFront();
                result->add(expressionCompoundStart());
                expectSemiColon = false;
            } else {
                // Check if a colon (a goto label).
                auto startLex = mLookAhead;
                if (getNextLookahead() == TT::Colon) {
                    pushTokenToFront();
                    pushTokenToFront(startLex);
                    result->add(labelStart());
                } else {
                    throw std::invalid_argument(
                        fmt::format("Invalid symbol: {}", mLookAhead.getText()));
                }
            }
            break;
        case TT::IntegerConstant:
        case TT::FloatConstant:
        case TT::Lparen:
            pushTokenToFront();
            result->add(expressionCompoundStart(true));
            // Left recursion.
            while (getNextLookahead() == TT::Comma) {
                result->add(expressionCompoundStart());
            }
            pushTokenToFront();
            break;
        case TT::Eof:
            shouldContinue = false;
            break;
        case TT::Return:
            pushTokenToFront();
            result->add(returnStart());
            expectSemiColon = true;
            break;
        case TT::Goto:
            pushTokenToFront();
            result->add(gotoStart());
            expectSemiColon = true;
            break;
        default:
            throw std::runtime_error(
                fmt::format("cmpstmt: {}, {}", mLookAhead.getTokenType(), mLookAhead.getText()));
            break;
        }
    }

    return result;
}

void ParseTreeImpl::parseTokens() {
    programStart();
}

hclang::GR ParseTreeImpl::programStart() {
    hclang::SymTableCtx ctx(mSymbolTable);  // Object ensures symbol table is popped.

    bool shouldContinue = true;
    mProgram.add(compoundStatementStart());

    return nullptr;
}

hclang::decl ParseTreeImpl::declarationSpecifiers(std::optional<hclang::typeInfo> info,
                                                  hclang::StorageClass sclass, bool isFunc) {
    getNextLookahead();
    // Check for type.
    if (auto ti = getTypeFrom(mLookAhead); ti) {
        parseRet(declarationSpecifiers(ti, sclass));
    }
    switch (mLookAhead.getTokenType()) {
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
    case TT::Star:
        parseRet(declarationSpecifiers(info->pointerTo(), sclass));
        break;
    case TT::Identifier:
        // Variable name.
        if (!info) {
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

    switch (mLookAhead.getTokenType()) {
    case TT::Lparen:
        pushTokenToFront();
        parseRet(funcDeclStart(info, sclass, id));
        break;
    case TT::Semicolon:
    case TT::Comma:
        pushTokenToFront();
        pushTableRet(hclang::makeVarDecl(id, info, sclass, mLookAhead));
        break;
    case TT::Equals:
        parseRet(declarationInitializationEqual(info, sclass, id));
        break;
    default:
        break;
    }
    throw std::invalid_argument("declid");
}

std::list<hclang::varDecl> ParseTreeImpl::funcDeclList() {
    // Consume (
    if (getNextLookahead() != TT::Lparen) {
        throw std::invalid_argument("funcdecllist: expected(");
    }

    std::list<hclang::varDecl> result;
    std::optional<hclang::typeInfo> info;
    hclang::Lexeme varStartLexeme;
    // Left-recursive list of variables.
    while ((varStartLexeme = getNextLookahead()) != TT::Rparen) {
        switch (mLookAhead.getTokenType()) {
        case TT::U64i:
        case TT::U32i:
        case TT::U16i:
        case TT::U8i:
        case TT::I64i:
        case TT::I32i:
        case TT::I16i:
        case TT::I8i:
        case TT::Identifier:
            if (info || (info = getTypeFrom(mLookAhead))) {
                hclang::Lexeme idLex;
                bool keepParsing = true;
                while (keepParsing) {
                    switch (getNextLookahead().getTokenType()) {
                    case TT::Identifier:
                        idLex = mLookAhead;
                        break;
                    case TT::Star:
                        info = info->pointerTo();
                        break;
                    case TT::Rparen:
                        pushTokenToFront();
                    case TT::Comma: {
                        // Determine pointer information if any.
                        auto inVar = hclang::makeVarDecl(
                            idLex, *info, hclang::StorageClass::Default, varStartLexeme);
                        result.push_back(inVar);
                        mSymbolTable.add(inVar->getIdRef(), inVar);
                        keepParsing = false;
                    } break;
                    default:
                        break;
                    }
                }
            } else {
                throw std::invalid_argument(
                    fmt::format("Use of {}, which is not a valid type", mLookAhead.getText()));
            }
            break;
        default:
            break;
        }
    }
    return result;
}

hclang::funcDecl ParseTreeImpl::funcDeclStart(hclang::typeInfo info, hclang::StorageClass sclass,
                                              hclang::Identifier id) {
    auto args = funcDeclList();

    hclang::funcDefn definition = nullptr;

    switch (getNextLookahead().getTokenType()) {
    case TT::Semicolon:
        pushTokenToFront();
        break;
    case TT::LCurlyBracket: {
        pushTokenToFront();
        definition = hclang::makeFuncDefn(info, compoundStatementStart());
        // Push the RCurlyBracket back to the top of the stack.
        pushTokenToFront();
    } break;
    default:
        throw std::invalid_argument(
            fmt::format("Expecetd ; or lcurly, got {}", mLookAhead.getText()));
        break;
    }

    auto symbol = hclang::makeFuncDecl(info, id, definition, args, sclass, mLookAhead);
    mSymbolTable[id] = symbol;
    return symbol;
}

hclang::varInit ParseTreeImpl::declarationInitializationEqual(hclang::typeInfo info,
                                                              hclang::StorageClass sclass,
                                                              hclang::Identifier id) {
    pushTableRet(hclang::makeVarInit(id, info, expressionCompoundStart(true)));
}

hclang::declStmnt ParseTreeImpl::declarationStatementStart() {
    auto result = std::make_shared<hclang::DeclarationStatement>(declarationSpecifiers());

    auto info = result->getType().value();
    auto sclass = result->getStorageClass().value_or(hclang::StorageClass::Default);
    // Left recursion.
    while (getNextLookahead() == TT::Comma) {
        // TODO sclass might not be passed here, or only certain specifiers are.
        result->push(declarationSpecifiers(info, sclass));
    }

    switch (mLookAhead.getTokenType()) {
    case TT::Semicolon:
        pushTokenToFront();
    case TT::RCurlyBracket:
        return result;
        break;
    default:
        break;
    }
    throw std::runtime_error(fmt::format("Got token {}", mLookAhead.getText()));
}

void ParseTreeImpl::expressionStart(ParseTreeImpl::YardShunter &ys) {
    switch (mLookAhead.getTokenType()) {
    case TT::IntegerConstant:
        ys.push(hclang::makeIntConst(mLookAhead));
        break;
    case TT::StringConstant:
        ys.push(hclang::makeStringConst(mLookAhead));
        break;
    case TT::Identifier:
        if (auto sym = mSymbolTable.find(mLookAhead.getText());
            sym && sym->getDeclType() == hclang::Declaration::Type::Variable) {
            ys.push(hclang::makeDeclRef(hclang::Identifier(mLookAhead),
                                        hclang::DeclarationReference::Type::LValue, mSymbolTable,
                                        mLookAhead));
        } else if (sym && sym->getDeclType() == hclang::Declaration::Type::Function) {
            // Parse function call.
            ys.push(hclang::makeFuncCall(std::static_pointer_cast<hclang::FunctionDeclaration>(sym),
                                         expressionList()));
        } else {
            throw std::invalid_argument(
                fmt::format("Undefined identifier: {}", mLookAhead.getText()));
        }
        break;
    case TT::Star:
        // TODO unary *.
        ys.push(ys.lastObjWasOp() ? O::Dereference : O::Multiply, mLookAhead);
        break;
    case TT::Divide:
        ys.push(O::Divide, mLookAhead);
        break;
    case TT::Plus:
        ys.push(ys.lastObjWasOp() ? O::Positive : O::Add, mLookAhead);
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
    case TT::LessThan:
        ys.push(O::LessThan, mLookAhead);
        break;
    case TT::GreaterThan:
        ys.push(O::GreaterThan, mLookAhead);
        break;
    case TT::LessThanEqual:
        ys.push(O::LessThanEqual, mLookAhead);
        break;
    case TT::GreaterThanEqual:
        ys.push(O::GreaterThanEqual, mLookAhead);
        break;
    case TT::Equality:
        ys.push(O::Equals, mLookAhead);
        break;
    case TT::Inequality:
        ys.push(O::NotEquals, mLookAhead);
        break;
    case TT::LogicalAnd:
        ys.push(O::LogicalAnd, mLookAhead);
        break;
    case TT::LogicalOr:
        ys.push(O::LogicalOr, mLookAhead);
        break;
    default:
        break;
    }
}

hclang::expList ParseTreeImpl::expressionList(bool semicolonEnds, bool lparenStarts) {
    // Ignore (
    if (lparenStarts && getNextLookahead() != TT::Lparen) {
        throw std::invalid_argument("need (");
    }

    // Determine ending delimiter.
    auto endTok = semicolonEnds ? TT::Semicolon : TT::Rparen;
    hclang::expList result;

    // Loop until until ending delimiter. Make a new expression for each ','.
    for (getNextLookahead(); mLookAhead != endTok; getNextLookahead()) {
        YardShunter ys;

        for (; mLookAhead != TT::Comma && mLookAhead != endTok; getNextLookahead()) {
            expressionStart(ys);
        }
        if (mLookAhead == endTok) {
            pushTokenToFront();
        }

        result.push_back(ys.reduce());
    }

    return result;
}

hclang::exp ParseTreeImpl::expressionArgumentStart() {
    YardShunter ys;
    // Eat the ( token.
    if (getNextLookahead() != TT::Lparen) {
        throw std::invalid_argument("need (");
    }
    // Left recursion.
    while (getNextLookahead() != TT::Rparen) {
        expressionStart(ys);
    }

    // Reduce remaining operations.
    return ys.reduce();
}

hclang::exp ParseTreeImpl::expressionCompoundStart(bool stopComma) {
    YardShunter ys;
    // Left recursion.
    while (getNextLookahead() != TT::Semicolon && !(stopComma && (mLookAhead == TT::Comma))) {
        expressionStart(ys);
    }

    pushTokenToFront();

    // Reduce remaining operations.
    return ys.reduce();
}

void ParseTreeImpl::parseSemantics() {
    hclang::semanticContext sc = {std::nullopt};
    mProgram.parseSemantics(sc);
    if (mConfig.shouldDumpAst()) {
        mProgram.pprint();
    } else {
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

void ParseTreeImpl::YardShunter::push(hclang::Operator op, const hclang::Lexeme &l) {
    if (op == O::Rightparen) {
        if (mOperatorStack.empty()) {
            return;
        }
        operatorLex o = mOperatorStack.top();
        while (o.op != O::Leftparen) {
            mOperatorStack.pop();
            if (mOperatorStack.empty()) {
                throw std::runtime_error("paren pop");
            }
            mExpressionQueue.push(o);
            o = mOperatorStack.top();
        }
        return;
    }

    auto maybePoppedOperator = pushOp(op, l);

    mLastObjWasOp = !hclang::operatorIsPostfix(op);
    if (!maybePoppedOperator) {
        return;
    }

    auto poppedOperator = maybePoppedOperator.value();
    mExpressionQueue.push(ParseTreeImpl::operatorLex(maybePoppedOperator.value()));
}

std::optional<ParseTreeImpl::operatorLex> ParseTreeImpl::YardShunter::pushOp(
    hclang::Operator op, const hclang::Lexeme &l) {
    if (mOperatorStack.empty() || op == O::Leftparen) {
        mOperatorStack.emplace(l, op);
        return std::nullopt;
    }
    // TODO associativity.
    if (auto topOp = mOperatorStack.top();
        hclang::getPrecedence(topOp.op) >= hclang::getPrecedence(op) && topOp.op != O::Leftparen) {
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
    while (!mExpressionQueue.empty()) {
        auto expOrOp = mExpressionQueue.front();
        mExpressionQueue.pop();

        if (std::holds_alternative<hclang::exp>(expOrOp)) {
            postfixEvalStack.push(std::get<hclang::exp>(expOrOp));
            continue;
        } else if (std::holds_alternative<hclang::declRef>(expOrOp)) {
            if (postfixEvalStack.empty()) {
                onlyLvalue = true;
            }
            postfixEvalStack.push(std::get<hclang::declRef>(expOrOp));
            continue;
        }

        onlyLvalue = false;
        auto o = std::get<operatorLex>(expOrOp);
        switch (hclang::operatorArgs(o.op)) {
        case 0:
            break;
        case 1: {
            auto texpr = postfixEvalStack.top();
            postfixEvalStack.pop();
            hclang::exp expr = nullptr;
            if (std::holds_alternative<hclang::declRef>(texpr)) {
                if (hclang::isAssignment(o.op)) {
                    postfixEvalStack.push(
                        hclang::makeUnAsgn(o.op, std::get<hclang::declRef>(texpr), o.lex));
                    break;
                } else if (o.op == hclang::Operator::AddressOf ||
                           o.op == hclang::Operator::Dereference) {
                    postfixEvalStack.push(
                        hclang::makeUnOp(o.op, std::get<hclang::declRef>(texpr), o.lex));
                    break;
                } else {
                    expr = hclang::makeL2Rval(std::get<hclang::declRef>(texpr));
                }
            } else {
                expr = std::get<hclang::exp>(texpr);
            }
            postfixEvalStack.push(hclang::makeUnOp(o.op, expr, o.lex));
        } break;
        case 2: {
            auto trhs = postfixEvalStack.top();
            postfixEvalStack.pop();
            hclang::exp rhs = nullptr;
            // Convert Lvalues to Rvalues.
            if (std::holds_alternative<hclang::declRef>(trhs)) {
                rhs = hclang::makeL2Rval(std::get<hclang::declRef>(trhs));
            } else {
                rhs = std::get<hclang::exp>(trhs);
            }

            hclang::exp lhs = nullptr;
            auto tlhs = postfixEvalStack.top();
            postfixEvalStack.pop();
            if (std::holds_alternative<hclang::declRef>(tlhs)) {
                if (hclang::isAssignment(o.op)) {
                    postfixEvalStack.push(
                        hclang::makeBinAsgn(o.op, std::get<hclang::declRef>(tlhs), rhs, o.lex));
                    break;

                } else {
                    lhs = hclang::makeL2Rval(std::get<hclang::declRef>(tlhs));
                }
            } else {
                lhs = std::get<hclang::exp>(tlhs);
            }

            postfixEvalStack.push(hclang::makeBinOp(o.op, lhs, rhs, o.lex));
        } break;
        case 3:
            break;
        default:
            return nullptr;
            break;
        }
    }

    if (postfixEvalStack.empty()) {
        return nullptr;
    }

    auto result = postfixEvalStack.top();
    if (std::holds_alternative<hclang::declRef>(result)) {
        if (onlyLvalue) {
            return hclang::makeL2Rval(std::get<hclang::declRef>(result));
        }
        return std::get<hclang::declRef>(result);
    }
    return std::get<hclang::exp>(result);
}

void ParseTreeImpl::YardShunter::flushOperators() {
    while (!mOperatorStack.empty()) {
        auto o = mOperatorStack.top();
        mOperatorStack.pop();

        mExpressionQueue.push(o);
    }
}

// Static functions.

std::optional<hclang::typeInfo> getTypeFrom(const hclang::Lexeme &l) {
    using ti = hclang::typeInfo;
    switch (l.getTokenType()) {
    case TT::U64i:
        return ti{""sv, nullptr, hct::U64i};
        break;
    case TT::U32i:
        return ti{""sv, nullptr, hct::U32i};
        break;
    case TT::U16i:
        return ti{""sv, nullptr, hct::U16i};
        break;
    case TT::U8i:
        return ti{""sv, nullptr, hct::U8i};
        break;
    case TT::U0i:
        return ti{""sv, nullptr, hct::U0i};
        break;
    case TT::I64i:
        return ti{""sv, nullptr, hct::I64i};
        break;
    case TT::I32i:
        return ti{""sv, nullptr, hct::I32i};
        break;
    case TT::I16i:
        return ti{""sv, nullptr, hct::I16i};
        break;
    case TT::I8i:
        return ti{""sv, nullptr, hct::I8i};
        break;
    case TT::I0i:
        return ti{""sv, nullptr, hct::I0i};
        break;
        // TODO pointers and user types.
    default:
        break;
    }

    return std::nullopt;
}
