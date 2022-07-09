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
    Assign,
    Expression,
    Accept,
    Declaration,
};

// TODO need a buffer of previous tokens/expressions, just like NPDAs.

using TT = slang::TokenType;
using PS = ParserState;
using ParseObject = std::variant<slang::Lexeme, slang::GR>;
using ParseStack = std::list<ParseObject>;
namespace fs = slang::fs;

class ParseTreeImpl : public slang::ParseTree {
public:
    ParseTreeImpl(std::shared_ptr<slang::Lexer> lexer, const slang::Config &config)
        : slang::ParseTree(lexer),mParseStack({}),mLookAhead(),
          mParserState(PS::Program),mConfig(config) {
    }
    virtual ~ParseTreeImpl() = default;
    void createParseTree();

protected:
    ParseStack mParseStack;
    slang::Lexeme mLookAhead;
    ParserState mParserState;
    const slang::Config &mConfig;

    // Private parsing functions.
    slang::Lexeme popTopToken();

    slang::programData step();
    slang::exp parseExpression();
    slang::programData parseProgram();
    slang::varDecl reduceVarDecl();
    slang::decl parseDeclaration();
    slang::Lexeme getNextLookahead();
};

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

slang::exp ParseTreeImpl::parseExpression() {
    return nullptr;
}

slang::varDecl ParseTreeImpl::reduceVarDecl() {
    // Reduce from the last tokens.
    auto id = popTopToken();
    auto type = popTopToken();
    return std::make_shared<slang::VariableDeclaration>(id);
}

slang::decl ParseTreeImpl::parseDeclaration() {
    getNextLookahead();

    switch(mLookAhead.getTokenType()) {
    case TT::Identifier:
        mParseStack.push_front(mLookAhead);
        break;
    case TT::Semicolon:
        // Reduce VariableDeclaration.
        return reduceVarDecl();
        break;
    default:
        throw std::runtime_error("Decl");
        break;
    }

    return nullptr;
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
        mParseStack.push_front(mLookAhead);
        mParserState = PS::Declaration;
        break;
    case TT::Identifier:
        mParseStack.push_front(mLookAhead);
        mParserState = PS::Program;
        break;
    case TT::Eof:
        mParserState = PS::Accept;
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
        break;
    case PS::Declaration:
        return parseDeclaration();
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

slang::Lexeme ParseTreeImpl::popTopToken() {
    auto grammarObj = mParseStack.front();
    mParseStack.pop_front();

    if(grammarObj.index() == 0) {
        return std::get<slang::Lexeme>(grammarObj);
    }
    throw std::runtime_error("poptoptoken");
}
