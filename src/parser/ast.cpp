/***************************************************************************
 * @file ast.cpp                                                           *
 * @brief Abstract syntax tree objects (implementation).                   *
 ***************************************************************************/
#include "ast.hpp"

#include <fmt/core.h>
#include <fmt/color.h>
#include <regex>
#include <algorithm>
#include <limits>
#include <stack>
#include "../util.hpp"
#include "symbols.hpp"

using TT = hclang::TokenType;
using O = hclang::Operator;
using namespace std::string_view_literals;

#define PRIMARY(what) (fmt::styled(what, fg(fmt::color::magenta) | fmt::emphasis::bold))
#define SECONDARY(what) (fmt::styled(what, fg(fmt::color::cyan)))
#define TERTIARY(what) (fmt::styled(what, fg(fmt::color::light_blue)))

// GrammarRule.

void hclang::GrammarRule::printDefault() const {
    fmt::print(fg(fmt::color::green), "{} ", getClassName());
    hclang::GrammarRule::pprint();
}

void hclang::GrammarRule::pprint() const {
    fmt::print("0x{:x} {}", reinterpret_cast<std::size_t>(this), mLexeme);
}

void hclang::GrammarRule::setLexeme(const hclang::Lexeme &l) {
    mLexeme = l;
}

// IntegerConstant.

hclang::IntegerConstant::IntegerConstant(std::uint64_t value, hclang::typeInfo t,
                                         const hclang::Lexeme &l)
    : Constant(t, l),mValue(value),mIsSigned(false) {
    // Test intrinsic type.
    using hct = hclang::HCType;
    auto type = t.type;
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

hclang::IntegerConstant::IntegerConstant(std::string_view source,
                                         const hclang::Lexeme &l)
    : Constant(hclang::typeInfo{ ""sv, nullptr, HCType::U64i }, l),mValue(0),
      mIsSigned(false) {
    // Check base.
    int base = 10;
    if(util::prefix(source, "0x") || util::prefix(source, "0X")) {
        source = std::string_view(source.data() + 2, source.size() - 2);
        base = 16;
    } else if(util::prefix(source, "0")) {
        source = std::string_view(source.data() + 1, source.size() - 1);
        base = 8;
    }

    mIsSigned = util::prefix(source, "-");

    if(mIsSigned) {
        // Signed constant.
        mType.type = HCType::I64i;
        std::int64_t value = 0;
        auto [ptr, ec] = std::from_chars(source.begin(), source.end(), value, base);

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
    // Unsigned constant.
    auto [ptr, ec] = std::from_chars(source.begin(), source.end(), mValue, base);

    if(ptr != source.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid integer",
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
        fmt::print(" {} {}",
                   SECONDARY(mType),
                   fmt::styled(static_cast<std::int64_t>(mValue),
                               fg(fmt::color::cyan)));
    } else {
        fmt::print(" {} {}",
                   SECONDARY(mType),
                   fmt::styled(mValue, fg(fmt::color::cyan)));
    }
}

std::string_view hclang::IntegerConstant::getClassName() const {
    return "IntegerConstant";
}

std::list<hclang::programData> hclang::IntegerConstant::getChildren() const {
    return {};
}

// Assignment

void hclang::Assignment::pprint() const {
    printDefault();
    fmt::print(" {} {}",  SECONDARY(mType), PRIMARY(mLhs));
}

std::string_view hclang::Assignment::getClassName() const {
    return "Assignment";
}

std::list<hclang::programData> hclang::Assignment::getChildren() const {
    return { mRhs };
}

// Variable declaration

void hclang::VariableDeclaration::pprint() const {
    printDefault();
    fmt::print(" {} {}",  SECONDARY(mType), PRIMARY(mId));
}

std::string_view hclang::VariableDeclaration::getClassName() const {
    return "VariableDeclaration";
}

std::list<hclang::programData> hclang::VariableDeclaration::getChildren() const {
    return {};
}

// DeclarationReference

hclang::DeclarationReference::DeclarationReference(const hclang::Identifier &id,
                                                   Type type,
                                                   const hclang::SymbolTable<decl> &table,
                                                   const Lexeme &l)
    : Expression(l),mType(type),mDeclRef(table.find(id))
{
}

void hclang::DeclarationReference::pprint() const {
    printDefault();
    std::size_t address = 0;
    if(mDeclRef) {
        address = reinterpret_cast<std::size_t>(mDeclRef.get());
    }
    fmt::print(" {} {}{:x}", PRIMARY(stringifyType()), SECONDARY("0x"), SECONDARY(address));
}

std::string_view hclang::DeclarationReference::getClassName() const {
    return "DeclarationReference";
}

std::list<hclang::programData> hclang::DeclarationReference::getChildren() const {
    return {  };
}

// Cast.

hclang::Cast::Cast(hclang::exp expr, hclang::typeInfo into, const hclang::Lexeme l)
    : hclang::Expression(l),mIntoType(into),mExpr(expr) {
    // TODO check that types are compatible.
}

void hclang::Cast::pprint() const {
    printDefault();
    fmt::print(" {}", hclang::typeToString(mIntoType.type));
}

std::string_view hclang::Cast::getClassName() const {
    return "Cast";
}

std::list<hclang::programData> hclang::Cast::getChildren() const {
    return { mExpr };
}

// ImplicitCast.

std::string_view hclang::ImplicitCast::getClassName() const {
    return "ImplicitCast";
}

// Variable initialization.

hclang::VariableInitialization::VariableInitialization(const hclang::Identifier &id,
                                                       hclang::typeInfo type,
                                                       hclang::exp expr)
    : hclang::VariableDeclaration(id, type),mRhs(expr) {
}

void hclang::VariableInitialization::setLexeme(const hclang::Lexeme &l) {
    mLexeme = l;
    if(mRhs) {
        mLexeme |= mRhs->getLexemeConst();
    }
}

void hclang::VariableInitialization::pprint() const {
    printDefault();
    fmt::print(" {} {}", fmt::styled("U64", fg(fmt::color::light_green) | fmt::emphasis::bold),
               fmt::styled(mId.getId(), fg(fmt::color::cyan) | fmt::emphasis::bold));
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

    std::stack<pdPrintData> stack;
    for(auto i = mStatements.rbegin(); i != mStatements.rend(); i++) {
        stack.emplace(*i, 1);
    }
    std::list<hclang::programData> visited = { mStatements.front() };

    std::uint32_t lastLevel = 0;
    // Depth first traversal of the AST. Print nodes as we pop them.
    while(!stack.empty()) {
        auto [curObj, indentLevel] = stack.top();
        stack.pop();

        // Print indentation and tree formatting.
        fmt::print("{:{}}{}-", "", indentLevel * 2 - 1, (indentLevel != lastLevel) ? '`' : '|');
        curObj->pprint();
        fmt::print("\n");
        lastLevel = indentLevel;

        auto children = curObj->getChildren();
        for(auto c = children.rbegin(); c != children.rend(); c++) {
            if(std::find(visited.begin(), visited.end(), *c) == visited.end()) {
                visited.push_front(*c);
                stack.emplace(*c, indentLevel + 1);
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
    fmt::print(" {}", fmt::styled(hclang::operatorToString(mOp), fg(fmt::color::magenta)));
}

std::string_view hclang::BinaryOperator::getClassName() const {
    return "BinaryOperator";
}

std::list<hclang::programData> hclang::BinaryOperator::getChildren() const {
    return { mLhs, mRhs };
}

// UnaryOperator.

void hclang::UnaryOperator::pprint() const {
    printDefault();
    fmt::print(" {}", fmt::styled(hclang::operatorToString(mOp), fg(fmt::color::magenta)));
}

std::string_view hclang::UnaryOperator::getClassName() const {
    return "UnaryOperator";
}

std::list<hclang::programData> hclang::UnaryOperator::getChildren() const {
    return { mExpr };
}

// DeclarationStatement


void hclang::DeclarationStatement::pprint() const {
    printDefault();
}

std::string_view hclang::DeclarationStatement::getClassName() const {
    return "DeclarationStatement";
}

std::list<hclang::programData> hclang::DeclarationStatement::getChildren() const {
    std::list<hclang::programData> result;
    for(const auto i : mDecls) {
        result.push_front(i);
    }
    return result;
}

// Functions.

std::string_view hclang::operatorToString(hclang::Operator op) {
    switch(op) {
    case O::Add:
        return "+ (Addition)";
        break;
    case O::Subtract:
        return "- (Subtraction)";
        break;
    case O::Multiply:
        return "* (Multiplication)";
        break;
    case O::Divide:
        return "/ (Division)";
        break;
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

std::size_t hclang::sizeofType(hclang::typeInfo t1) {
    using hct = hclang::HCType;
    switch(t1.type) {
    case hct::I0i:
    case hct::U0i:
        return 0;
        break;
    case hct::I8i:
    case hct::U8i:
        return 1;
        break;
    case hct::I16i:
    case hct::U16i:
        return 2;
        break;
    case hct::I32i:
    case hct::U32i:
        return 4;
        break;
    case hct::I64i:
    case hct::U64i:
    case hct::Pointer:
    case hct::F64:
        return 8;
        break;
        // TODO structs, enums, etc. and pointer type
    default:
        break;
    }
    return 0;
}

bool hclang::sizeofGt(hclang::typeInfo t1, hclang::typeInfo t2) {
    return hclang::sizeofType(t1) > hclang::sizeofType(t2);
}

bool hclang::sizeofEq(hclang::typeInfo t1, hclang::typeInfo t2) {
    return hclang::sizeofType(t1) == hclang::sizeofType(t2);
}

bool hclang::isInteger(hclang::typeInfo t) {
    using hct = hclang::HCType;
    switch(t.type) {
    case hct::I0i:
    case hct::U0i:
    case hct::I8i:
    case hct::U8i:
    case hct::I16i:
    case hct::U16i:
    case hct::I32i:
    case hct::U32i:
    case hct::I64i:
    case hct::U64i:
    case hct::Pointer:
        return true;
        break;
    default:
        break;
    }
    return false;
}

int hclang::getPrecedence(hclang::Operator op) {
    switch(op) {
    case O::Assignment:
    case O::LeftshiftAssignment:
    case O::RightshiftAssignment:
    case O::MultiplyAssignment:
    case O::DivideAssignment:
    case O::AddAssignment:
    case O::SubtractAssignment:
    case O::AndAssignment:
    case O::OrAssignment:
    case O::XorAssignment:
        return 0;
        break;
    case O::LogicalOr:
        return 1;
        break;
    // case O::DoubleCaret: TODO unknown operator (probably logical Xor?)
    //     return 2;
    //     break;
    case O::LogicalAnd:
        return 3;
        break;
    case O::Equals:
    case O::NotEquals:
        return 4;
        break;
    case O::LessThan:
    case O::GreaterThan:
    case O::LessThanEqual:
    case O::GreaterThanEqual:
        return 5;
        break;
    case O::Add:
    case O::Subtract:
        return 6;
        break;
    case O::BitwiseOr:
        return 7;
        break;
    case O::BitwiseXor:
        return 8;
        break;
    case O::BitwiseAnd:
        return 9;
        break;
    case O::Multiply:
    case O::Divide:
    case O::Modulo:
        return 10;
        break;
    case O::Power:
    case O::Leftshift:
    case O::Rightshift:
        return 11;
        break;
    case O::Leftparen:
    case O::Rightparen:
        return std::numeric_limits<int>::max();
        break;
    default:
        break;
    }
    return -1;
}

bool hclang::operatorIsPrefix(hclang::Operator op) {
    switch(op) {
    case O::Negative:
    case O::Positive:
    case O::Dereference:
    case O::LogicalNot:
    case O::BitwiseNot:
    case O::AddressOf:
    case O::PrefixPlusPlus:
    case O::PrefixMinusMinus:
        return true;
        break;
    default:
        break;
    }
    return false;
}

bool hclang::operatorIsPostfix(hclang::Operator op) {
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

int hclang::operatorArgs(hclang::Operator op) {
    if(hclang::operatorIsPostfix(op) || hclang::operatorIsPrefix(op)) {
        return 1;
    }
    else if(op == O::Leftparen || op == O::Rightparen) {
        return 0;
    }
    else if(op == O::Ternary) {
        return 3;
    }
    return 2;
}
