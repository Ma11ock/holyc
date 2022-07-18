#include "ast.hpp"

#include <fmt/core.h>
#include <fmt/color.h>
#include <regex>
#include <algorithm>
#include "../util.hpp"

using TT = slang::TokenType;
using O = slang::Operator;

// GrammarRule.

void slang::GrammarRule::printDefault() const {
    fmt::print(fg(fmt::color::green), "{} ", getClassName());
    slang::GrammarRule::pprint();
}

void slang::GrammarRule::pprint() const {
    fmt::print("0x{} <line:{}:{},col:{}:{}>",
               reinterpret_cast<std::size_t>(this),
               fmt::styled(mLexeme.getStartLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getEndLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getStartLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getEndColNumber(), fg(fmt::color::gold)));
}

// IntegerConstant.

slang::IntegerConstant::IntegerConstant(std::string_view source) : mValue(0),mIsSigned(false) {
    auto [ptr, ec] = std::from_chars(source.begin(), source.end(), mValue);

    if(ptr != source.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid int64",
                                                source));
    }

    switch(ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid int64",
                                                source));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 64 bit integer",
                                           source));
        break;
    default:
        break;
    }
}

void slang::IntegerConstant::pprint() const {
    printDefault();
    if(mIsSigned) {
        fmt::print(" 'U64' {}", static_cast<std::int64_t>(mValue));
    } else {
        fmt::print(" 'U64' {}", mValue);
    }
}

std::string_view slang::IntegerConstant::getClassName() const {
    return "IntegerConstant";
}

std::list<slang::programData> slang::IntegerConstant::getChildren() const {
    return {};
}

// Variable declaration

void slang::VariableDeclaration::pprint() const {
    printDefault();
    fmt::print(" Var:{} Type:{}", mId, "U64");
}

std::string_view slang::VariableDeclaration::getClassName() const {
    return "VariableDeclaration";
}

std::list<slang::programData> slang::VariableDeclaration::getChildren() const {
    return {};
}

// Variable initialization.

void slang::VariableInitialization::pprint() const {
    printDefault();
    fmt::print(" {} `{}`", "U64", mId.getId());
}

std::string_view slang::VariableInitialization::getClassName() const {
    return "VariableInitialization";
}

std::list<slang::programData> slang::VariableInitialization::getChildren() const {
    return { mRhs };
}

// Program.

void slang::Program::add(slang::programData pd) {
    mStatements.push_back(pd);
}

void slang::Program::pprint() const {
    for(int i = 0; i < 10; i++) {
        fmt::print("+---------");
    }
    fmt::print("+\n");
    struct pdPrintData {
        slang::programData pd;
        std::uint32_t indentLevel;

        pdPrintData(slang::programData pd, std::uint32_t indentLevel)
            : pd(pd),indentLevel(indentLevel) {}
    };

    if(mStatements.empty()) {
        return;
    }

    std::list<pdPrintData> stack = { pdPrintData(mStatements.front(), UINT32_C(1)) };
    std::list<slang::programData> visited = { mStatements.front() };

    std::uint32_t lastLevel = 0;
    // Depth first traversal of the AST. Print nodes as we pop them.
    while(!stack.empty()) {
        auto [curObj, indentLevel] = stack.front();
        stack.pop_front();

        // Print indentation and tree formatting.
        fmt::print("{:{}}{}", "", indentLevel * 2 - 1, (indentLevel != lastLevel) ? '`' : '|');
        curObj->pprint();
        fmt::print("\n");
        lastLevel = indentLevel;

        auto children = curObj->getChildren();
        for(const auto &child : children) {
            if(std::find(visited.begin(), visited.end(), child) == visited.end()) {
                visited.push_front(child);
                stack.emplace_front(child, indentLevel + 1);
            }
        }
    }
}

std::string_view slang::Program::getClassName() const {
    return "Program";
}

std::list<slang::programData> slang::Program::getChildren() const {
    return mStatements;
}

// CompoundStatement.


void slang::CompoundStatement::add(slang::stmnt statement) {
    mStatementList.push_back(statement);
}

void slang::CompoundStatement::pprint() const {
    printDefault();
}

std::string_view slang::CompoundStatement::getClassName() const {
    return "CompoundStatement";
}

std::list<slang::programData> slang::CompoundStatement::getChildren() const {
    return {};
}

// BinaryOperator.

void slang::BinaryOperator::pprint() const {
    printDefault();
    fmt::print(" {}", slang::operatorToLexeme(mOp));
}

std::string_view slang::BinaryOperator::getClassName() const {
    return "BinaryOperator";
}

std::list<slang::programData> slang::BinaryOperator::getChildren() const {
    return {};
}


// DeclarationStatement


void slang::DeclarationStatement::pprint() const {
    printDefault();
}

std::string_view slang::DeclarationStatement::getClassName() const {
    return "DeclarationStatement";
}

std::list<slang::programData> slang::DeclarationStatement::getChildren() const {
    return {};
}

// Functions.

std::string_view slang::operatorToLexeme(Operator op) {
    switch(op) {
    case O::Add:
        return "+";
    default:
        break;
    }
    return "";
}
