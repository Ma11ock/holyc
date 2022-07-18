/***************************************************************************
 * @file ast.cpp                                                           *
 * @brief Abstract syntax tree objects (implementation).                   *
 ***************************************************************************/
#include "ast.hpp"

#include <fmt/core.h>
#include <fmt/color.h>
#include <regex>
#include <algorithm>
#include "../util.hpp"

using TT = hclang::TokenType;
using O = hclang::Operator;

// GrammarRule.

void hclang::GrammarRule::printDefault() const {
    fmt::print(fg(fmt::color::green), "{} ", getClassName());
    hclang::GrammarRule::pprint();
}

void hclang::GrammarRule::pprint() const {
    fmt::print("0x{} <line:{}:{},col:{}:{}>",
               reinterpret_cast<std::size_t>(this),
               fmt::styled(mLexeme.getStartLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getEndLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getStartLineNumber(), fg(fmt::color::gold)),
               fmt::styled(mLexeme.getEndColNumber(), fg(fmt::color::gold)));
}

// IntegerConstant.

hclang::IntegerConstant::IntegerConstant(std::uint64_t value, hclang::HCType type,
                                         bool isSigned, const hclang::Lexeme &l)
    : Constant(l),mValue(value),mType(type),mIsSigned(isSigned) {
    // Test intrinsic type.
    using hct = hclang::HCType;
    switch(type) {
    case hct::Class:
    case hct::Enum:
    case hct::Union:
        throw std::invalid_argument(fmt::format("{} is not an integer type",
                                                typeToString(type)));
        break;
    case hct::I0i:
    case hct::U0i:
        throw std::invalid_argument(fmt::format("{} cannot have constants",
                                                typeToString(type)));
        break;
    default:
        break;
    }
}

hclang::IntegerConstant::IntegerConstant(std::string_view source, bool isSigned)
    : mValue(0),mIsSigned(isSigned) {
    if(isSigned) {
        std::int64_t value = 0;
        auto [ptr, ec] = std::from_chars(source.begin(), source.end(), value);

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
        mValue = static_cast<std::uint64_t>(value);
        return;
    }
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

void hclang::IntegerConstant::pprint() const {
    printDefault();
    if(mIsSigned) {
        fmt::print(" 'U64' {}", static_cast<std::int64_t>(mValue));
    } else {
        fmt::print(" 'U64' {}", mValue);
    }
}

std::string_view hclang::IntegerConstant::getClassName() const {
    return "IntegerConstant";
}

std::list<hclang::programData> hclang::IntegerConstant::getChildren() const {
    return {};
}

// Variable declaration

void hclang::VariableDeclaration::pprint() const {
    printDefault();
    fmt::print(" Var:{} Type:{}", mId, "U64");
}

std::string_view hclang::VariableDeclaration::getClassName() const {
    return "VariableDeclaration";
}

std::list<hclang::programData> hclang::VariableDeclaration::getChildren() const {
    return {};
}

// Variable initialization.

void hclang::VariableInitialization::pprint() const {
    printDefault();
    fmt::print(" {} `{}`", "U64", mId.getId());
}

std::string_view hclang::VariableInitialization::getClassName() const {
    return "VariableInitialization";
}

std::list<hclang::programData> hclang::VariableInitialization::getChildren() const {
    return { mRhs };
}

// Program.

void hclang::Program::add(hclang::programData pd) {
    mStatements.push_back(pd);
}

void hclang::Program::pprint() const {
    for(int i = 0; i < 10; i++) {
        fmt::print("+---------");
    }
    fmt::print("+\n");
    struct pdPrintData {
        hclang::programData pd;
        std::uint32_t indentLevel;

        pdPrintData(hclang::programData pd, std::uint32_t indentLevel)
            : pd(pd),indentLevel(indentLevel) {}
    };

    if(mStatements.empty()) {
        return;
    }

    std::list<pdPrintData> stack = { pdPrintData(mStatements.front(), UINT32_C(1)) };
    std::list<hclang::programData> visited = { mStatements.front() };

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

std::string_view hclang::Program::getClassName() const {
    return "Program";
}

std::list<hclang::programData> hclang::Program::getChildren() const {
    return mStatements;
}

// CompoundStatement.


void hclang::CompoundStatement::add(hclang::stmnt statement) {
    mStatementList.push_back(statement);
}

void hclang::CompoundStatement::pprint() const {
    printDefault();
}

std::string_view hclang::CompoundStatement::getClassName() const {
    return "CompoundStatement";
}

std::list<hclang::programData> hclang::CompoundStatement::getChildren() const {
    return {};
}

// BinaryOperator.

void hclang::BinaryOperator::pprint() const {
    printDefault();
    fmt::print(" {}", hclang::operatorToString(mOp));
}

std::string_view hclang::BinaryOperator::getClassName() const {
    return "BinaryOperator";
}

std::list<hclang::programData> hclang::BinaryOperator::getChildren() const {
    return {};
}


// DeclarationStatement


void hclang::DeclarationStatement::pprint() const {
    printDefault();
}

std::string_view hclang::DeclarationStatement::getClassName() const {
    return "DeclarationStatement";
}

std::list<hclang::programData> hclang::DeclarationStatement::getChildren() const {
    return {};
}

// Functions.

std::string_view hclang::operatorToString(hclang::Operator op) {
    switch(op) {
    case O::Add:
        return "+";
    default:
        break;
    }
    return "";
}

std::string_view hclang::typeToString(hclang::HCType type) {
    using hct = hclang::HCType;
    switch(type) {
    case hct::U0i:
        return "U0i";
        break;
    case hct::U8i:
        return "U8i";
        break;
    case hct::U16i:
        return "U16i";
        break;
    case hct::U32i:
        return "U32i";
        break;
    case hct::U64i:
        return "U64i";
        break;
    case hct::I0i:
        return "I0i";
        break;
    case hct::I8i:
        return "I8i";
        break;
    case hct::I16i:
        return "I16i";
        break;
    case hct::I32i:
        return "I32i";
        break;
    case hct::I64i:
        return "I64i";
        break;
    case hct::Pointer:
        return "Pointer";
        break;
    case hct::Class:
        return "Class";
        break;
    case hct::Enum:
        return "Enum";
        break;
    case hct::Union:
        return "Union";
    default:
        break;
    }
    throw std::invalid_argument(fmt::format("{} is not a valid HCType",
                                            static_cast<int>(type)));
}
