/***************************************************************************
 * @file ast.cpp                                                           *
 * @brief Abstract syntax tree objects (implementation).                   *
 ***************************************************************************/
#include "ast.hpp"

#include <fmt/color.h>
#include <fmt/core.h>

#include <algorithm>
#include <limits>
#include <regex>
#include <stack>

#include "../util.hpp"
#include "symbols.hpp"

using TT = hclang::TokenType;
using O = hclang::Operator;
using hct = hclang::HCType;
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

void hclang::GrammarRule::setLexeme(std::optional<Lexeme> l) {
    if (!mLexeme) {
        mLexeme = l;
    } else if (mLexeme && l) {
        *mLexeme |= *l;
    }
}

// Expression.

bool hclang::Expression::isLValue() const {
    return false;
}

// StringConstant.

hclang::StringConstant::StringConstant(std::string_view str, bool stripQuotes,
                                       std::optional<Lexeme> l)
    : Constant(typeInfo{Identifier(),
                        std::make_shared<typeInfo>(typeInfo{Identifier(), nullptr, HCType::U8i}),
                        HCType::Pointer},
               l),
      mStr(str) {
    if (stripQuotes) {
        if (util::prefix(mStr, "\"")) {
            mStr = std::string_view(mStr.data() + 1, mStr.size() - 1);
        }
        if (util::postfix(mStr, "\"")) {
            mStr = std::string_view(mStr.data(), mStr.size() - 1);
        }
    }
}

void hclang::StringConstant::pprint() const {
    printDefault();
    fmt::print(" {}, len={}", PRIMARY(mStr), SECONDARY(mStr.size()));
}

std::string_view hclang::StringConstant::getClassName() const {
    return "StringConstant";
}

// IntegerConstant.

hclang::IntegerConstant::IntegerConstant(std::uint64_t value, hclang::typeInfo t,
                                         std::optional<Lexeme> l)
    : Constant(t, l), mValue(value), mIsSigned(false) {
    // Test intrinsic type.
    using hct = hclang::HCType;
    auto type = t.type;
    switch (type) {
    case hct::Class:
    case hct::Enum:
    case hct::Union:
        throw std::invalid_argument(fmt::format("{} is not an integer type", typeToString(type)));
        break;
    case hct::I0i:
    case hct::U0i:
        throw std::invalid_argument(fmt::format("{} cannot have constants", typeToString(type)));
        break;

    case hct::I8i:
    case hct::I16i:
    case hct::I32i:
    case hct::I64i:
        mIsSigned = true;
        break;
    default:
        break;
    }
}

hclang::IntegerConstant hclang::IntegerConstant::makeI8(std::string_view src, int base,
                                                        std::optional<Lexeme> l) {
    std::int8_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid int8", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid int8", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 8 bit integer", src));
        break;
    default:
        break;
    }

    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::I8i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeU8(std::string_view src, int base,
                                                        std::optional<Lexeme> l) {
    std::uint8_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid uint8", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid uint8", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 8 bit unsigned integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::U8i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeI16(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::int16_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid int16", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid int16", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 16 bit integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::I16i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeU16(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::uint16_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid uint16", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid uint16", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 16 bit unsigned integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::U16i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeI32(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::int32_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid int32", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid int32", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 32 bit integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::I32i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeU32(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::uint32_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid uint32", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid uint32", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 32 bit unsigned integer", src));
        break;
    default:
        break;
    }

    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::U32i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeI64(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::int64_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid int64", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid int64", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 64 bit integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::I64i}, l);
}

hclang::IntegerConstant hclang::IntegerConstant::makeU64(std::string_view src, int base,
                                                         std::optional<Lexeme> l) {
    std::uint64_t value = 0;
    auto [ptr, ec] = std::from_chars(src.begin(), src.end(), value, base);

    if (ptr != src.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid uint64", src));
    }

    switch (ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid uint64", src));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 64 bit unsigned integer", src));
        break;
    default:
        break;
    }
    return IntegerConstant(static_cast<std::uint64_t>(value), {""sv, nullptr, HCType::U64i}, l);
}

hclang::IntegerConstant::IntegerConstant(std::string_view source, std::optional<Lexeme> l)
    : Constant(hclang::typeInfo{""sv, nullptr, HCType::U64i}, l), mValue(0), mIsSigned(false) {
    // Check base.
    int base = 10;
    if (util::prefix(source, "0x") || util::prefix(source, "0X")) {
        source = std::string_view(source.data() + 2, source.size() - 2);
        base = 16;
    } else if (util::prefix(source, "0") && source.size() > 1) {
        source = std::string_view(source.data() + 1, source.size() - 1);
        base = 8;
    }

    if (util::postfix(source, "U8")) {
        source = std::string_view(source.data(), source.size() - 2);
        *this = makeU8(source, base, l);
    } else if (util::postfix(source, "I8")) {
        source = std::string_view(source.data(), source.size() - 2);
        *this = makeI8(source, base, l);
    } else if (util::postfix(source, "U16")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeU16(source, base, l);
    } else if (util::postfix(source, "I16")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeI16(source, base, l);
    } else if (util::postfix(source, "U32")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeU32(source, base, l);
    } else if (util::postfix(source, "I32")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeI32(source, base, l);
    } else if (util::postfix(source, "U64")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeU64(source, base, l);
    } else if (util::postfix(source, "I64")) {
        source = std::string_view(source.data(), source.size() - 3);
        *this = makeI64(source, base, l);
    } else {
        // U64 is default integer type.
        *this = makeU64(source, base, l);
    }
}

void hclang::IntegerConstant::pprint() const {
    printDefault();
    if (mIsSigned) {
        fmt::print(" {} {}", SECONDARY(mType), PRIMARY(static_cast<std::int64_t>(mValue)));
    } else {
        fmt::print(" {} {}", SECONDARY(mType), PRIMARY(mValue));
    }
}

std::string_view hclang::IntegerConstant::getClassName() const {
    return "IntegerConstant";
}

// Constant.

std::list<hclang::GR> hclang::Constant::getChildren() const {
    return {};
}

// Variable declaration

void hclang::VariableDeclaration::pprint() const {
    printDefault();
    fmt::print(" {} {}", SECONDARY(mType), PRIMARY(mId));
}

std::string_view hclang::VariableDeclaration::getClassName() const {
    return "VariableDeclaration";
}

std::list<hclang::GR> hclang::VariableDeclaration::getChildren() const {
    return {};
}

hclang::Declaration::Type hclang::VariableDeclaration::getDeclType() const {
    return Type::Variable;
}

// Return.

void hclang::Return::pprint() const {
    printDefault();
    if (!mExp) {
        fmt::print(" (void)");
        return;
    }
    fmt::print(" {}", PRIMARY(mExp->getType()));
}

std::string_view hclang::Return::getClassName() const {
    return "Return";
}

std::list<hclang::GR> hclang::Return::getChildren() const {
    if (mExp) {
        return {mExp};
    }
    return {};
}

// DeclarationReference

hclang::DeclarationReference::DeclarationReference(const hclang::Identifier &id, Type type,
                                                   const hclang::SymbolTable<decl> &table,
                                                   std::optional<Lexeme> l)
    : Expression(l), mDeclType(type), mDeclRef(table.find(id)) {
    if (mDeclRef) {
        mType = mDeclRef->getType();
    }
}

bool hclang::DeclarationReference::isLValue() const {
    return true;
}

void hclang::DeclarationReference::pprint() const {
    printDefault();
    std::size_t address = 0;
    if (mDeclRef) {
        address = reinterpret_cast<std::size_t>(mDeclRef.get());
    }
    fmt::print(" {} {} {}{:x}", PRIMARY(stringifyType()), PRIMARY(mDeclRef->getIdRef()),
               SECONDARY("0x"), SECONDARY(address));
}

std::string_view hclang::DeclarationReference::getClassName() const {
    return "DeclarationReference";
}

std::list<hclang::GR> hclang::DeclarationReference::getChildren() const {
    return {mDeclRef};
}

// LToRValue

void hclang::LToRValue::pprint() const {
    printDefault();
}

std::string_view hclang::LToRValue::getClassName() const {
    return "LToRValue";
}

std::list<hclang::GR> hclang::LToRValue::getChildren() const {
    return {mDeclRef};
}

// Cast.

hclang::Cast::Cast(hclang::exp expr, hclang::typeInfo into, std::optional<Lexeme> l)
    : hclang::Expression(into, l), mExpr(expr) {
    // TODO check that types are compatible.
}

void hclang::Cast::pprint() const {
    printDefault();
    fmt::print(" {}", PRIMARY(mType));
}

std::string_view hclang::Cast::getClassName() const {
    return "Cast";
}

std::list<hclang::GR> hclang::Cast::getChildren() const {
    return {mExpr};
}

// ImplicitCast.

std::string_view hclang::ImplicitCast::getClassName() const {
    return "ImplicitCast";
}

// Variable initialization.

hclang::VariableInitialization::VariableInitialization(const hclang::Identifier &id,
                                                       hclang::typeInfo type, hclang::exp expr)
    : hclang::VariableDeclaration(id, type), mRhs(expr) {
}

void hclang::VariableInitialization::setLexeme(std::optional<Lexeme> l) {
    mLexeme = l;
    if (mRhs && l) {
        *mLexeme |= mRhs->getLexemeConst();
    }
}

void hclang::VariableInitialization::pprint() const {
    printDefault();
    fmt::print(" {} {}", fmt::styled(getType(), fg(fmt::color::light_green) | fmt::emphasis::bold),
               fmt::styled(mId.getId(), fg(fmt::color::cyan) | fmt::emphasis::bold));
}

std::string_view hclang::VariableInitialization::getClassName() const {
    return "VariableInitialization";
}

std::list<hclang::GR> hclang::VariableInitialization::getChildren() const {
    return {mRhs};
}

// Program.

void hclang::Program::add(hclang::cmpdStmnt pd) {
    if (pd == nullptr) {
        return;
    }
    if (mStatements.empty() || std::holds_alternative<funcDefn>(mStatements.back())) {
        mStatements.push_back(pd);
    } else {
        std::get<cmpdStmnt>(mStatements.back())->add(pd);
    }
}

void hclang::Program::pprint() const {
    for (int i = 0; i < 10; i++) {
        fmt::print("+---------");
    }
    fmt::print("+\n");

    struct pdPrintData {
        hclang::GR pd;
        std::uint32_t indentLevel;

        pdPrintData(hclang::GR pd, std::uint32_t indentLevel) : pd(pd), indentLevel(indentLevel) {
        }

        pdPrintData(hclang::programData pd, std::uint32_t indentLevel)
            : pd(getPD(pd)), indentLevel(indentLevel) {
        }
    };

    if (mStatements.empty()) {
        return;
    }

    std::stack<pdPrintData> stack;
    for (auto i = mStatements.rbegin(); i != mStatements.rend(); i++) {
        stack.emplace(hclang::getPD(*i), 1);
    }
    std::list<hclang::GR> visited = {stack.top().pd};

    std::uint32_t lastLevel = 0;
    // Depth first traversal of the AST. Print nodes as we pop them.
    while (!stack.empty()) {
        auto [curObj, indentLevel] = stack.top();
        stack.pop();

        // Print indentation and tree formatting.
        fmt::print("{:{}}{}-", "", indentLevel * 2 - 1, (indentLevel != lastLevel) ? '`' : '|');
        curObj->pprint();
        fmt::print("\n");
        lastLevel = indentLevel;

        auto children = curObj->getChildren();
        for (auto c = children.rbegin(); c != children.rend(); c++) {
            if (*c == nullptr) {
                continue;
            }
            if (std::find(visited.begin(), visited.end(), *c) == visited.end()) {
                visited.push_front(*c);
                stack.emplace(*c, indentLevel + 1);
            }
        }
    }
}

std::string_view hclang::Program::getClassName() const {
    return "Program";
}

std::list<hclang::GR> hclang::Program::getChildren() const {
    std::list<GR> result;
    for (auto &pd : mStatements) {
        result.push_front(getPD(pd));
    }
    return result;
}

// CompoundStatement.

void hclang::CompoundStatement::add(hclang::stmnt statement) {
    if (statement == nullptr) {
        return;
    }
    mStatementList.push_back(statement);
    setLexeme(statement->getLexemeConst());
}

void hclang::CompoundStatement::add(hclang::cmpdStmnt statement) {
    if (!statement || statement->isEmpty()) {
        return;
    }
    mStatementList.insert(mStatementList.end(), statement->mStatementList.begin(),
                          statement->mStatementList.end());
    // Assumes that `statements` lexeme is properly set.
    if (!statement->mStatementList.empty()) {
        setLexeme(statement->mStatementList.back()->getLexemeConst());
    }
}

void hclang::CompoundStatement::pprint() const {
    printDefault();
}

std::string_view hclang::CompoundStatement::getClassName() const {
    return "CompoundStatement";
}

std::list<hclang::GR> hclang::CompoundStatement::getChildren() const {
    std::list<hclang::GR> result;
    for (const auto &statement : mStatementList) {
        result.push_back(statement);
    }
    return result;
}

// BinaryOperator.

void hclang::BinaryOperator::pprint() const {
    printDefault();
    fmt::print(" {}", fmt::styled(hclang::operatorToString(mOp), fg(fmt::color::magenta)));
}

std::string_view hclang::BinaryOperator::getClassName() const {
    return "BinaryOperator";
}

std::list<hclang::GR> hclang::BinaryOperator::getChildren() const {
    return {mLhs, mRhs};
}

// Binary Assignment

std::list<hclang::GR> hclang::BinaryAssignment::getChildren() const {
    return {mLhs, mRhs};
}

// UnaryOperator.

void hclang::UnaryOperator::pprint() const {
    printDefault();
    fmt::print(" {}", fmt::styled(hclang::operatorToString(mOp), fg(fmt::color::magenta)));
}

std::string_view hclang::UnaryOperator::getClassName() const {
    return "UnaryOperator";
}

std::list<hclang::GR> hclang::UnaryOperator::getChildren() const {
    return {mExpr};
}

// UnaryAssignment

std::list<hclang::GR> hclang::UnaryAssignment::getChildren() const {
    return {mExpr};
}

// DeclarationStatement

void hclang::DeclarationStatement::pprint() const {
    printDefault();
}

std::string_view hclang::DeclarationStatement::getClassName() const {
    return "DeclarationStatement";
}

std::list<hclang::GR> hclang::DeclarationStatement::getChildren() const {
    std::list<hclang::GR> result;
    for (const auto i : mDecls) {
        result.push_front(i);
    }
    return result;
}

// FunctionDefinition.

void hclang::FunctionDefinition::pprint() const {
    printDefault();
}

std::string_view hclang::FunctionDefinition::getClassName() const {
    return "FunctionDefinition";
}

std::list<hclang::GR> hclang::FunctionDefinition::getChildren() const {
    return {scastGR(mBody)};
}

// FunctionDeclaration.

void hclang::FunctionDeclaration::pprint() const {
    printDefault();
    fmt::print(" {} {}", PRIMARY(mId), SECONDARY(mType));
}

std::string_view hclang::FunctionDeclaration::getClassName() const {
    return "FunctionDeclaration";
}

std::list<hclang::GR> hclang::FunctionDeclaration::getChildren() const {
    std::list<hclang::GR> result{mDefinition};
    result.insert(result.begin(), mArgs.begin(), mArgs.end());
    return result;
}

hclang::Declaration::Type hclang::FunctionDeclaration::getDeclType() const {
    return Type::Function;
}

// If.

void hclang::If::pprint() const {
    printDefault();
    fmt::print(" {}, elifs: {}", mElseBody ? "has else" : "no else", mElseIfs.size());
}

std::string_view hclang::If::getClassName() const {
    return "IfStatement";
}

std::list<hclang::GR> hclang::If::getChildren() const {
    // TODO elifs
    std::list<hclang::GR> result;
    result.assign(mElseIfs.begin(), mElseIfs.end());
    result.push_front(mConditional);
    result.push_back(mBody);
    result.push_back(mElseBody);
    return result;
}

// ElseIf.

void hclang::ElseIf::pprint() const {
    printDefault();
}

std::string_view hclang::ElseIf::getClassName() const {
    return "ElseIfStatement";
}

std::list<hclang::GR> hclang::ElseIf::getChildren() const {
    // TODO elifs
    return {scastGR(mConditional), scastGR(mBody)};
}

// While

void hclang::While::pprint() const {
    printDefault();
    fmt::print(" {}", mIsDo ? "do-while" : "");
}

std::string_view hclang::While::getClassName() const {
    return "WhileStatement";
}

std::list<hclang::GR> hclang::While::getChildren() const {
    return {scastGR(mConditional), scastGR(mBody)};
}

// Goto

void hclang::Goto::pprint() const {
    printDefault();
}

std::string_view hclang::Goto::getClassName() const {
    return "Goto";
}

std::list<hclang::GR> hclang::Goto::getChildren() const {
    return {scastGR(mLabel)};
}

// Label

void hclang::Label::pprint() const {
    printDefault();
}

std::string_view hclang::Label::getClassName() const {
    return "Label";
}

std::list<hclang::GR> hclang::Label::getChildren() const {
    return {};
}

// For

void hclang::For::pprint() const {
    printDefault();
}

std::string_view hclang::For::getClassName() const {
    return "ForStatement";
}

std::list<hclang::GR> hclang::For::getChildren() const {
    std::list<hclang::GR> result;
    for (auto &startExp : mStartExps) {
        result.push_back(startExp);
    }
    result.push_back(mConditional);
    for (auto &endExp : mEndExps) {
        result.push_back(endExp);
    }
    result.push_back(mBody);
    return result;
}

// FunctionCall

void hclang::FunctionCall::pprint() const {
    printDefault();
    fmt::print(" {}", PRIMARY(mFunc->getIdRef()));
}

std::string_view hclang::FunctionCall::getClassName() const {
    return "FunctionCall";
}

std::list<hclang::GR> hclang::FunctionCall::getChildren() const {
    std::list<hclang::GR> result = {scastGR(mFunc)};
    result.insert(result.end(), mArgExps.begin(), mArgExps.end());
    return result;
}

// Functions.

std::string_view hclang::operatorToString(hclang::Operator op) {
    switch (op) {
    case O::AddAssignment:
        return "+= (Add assignment)";
        break;
    case O::SubtractAssignment:
        return "-= (Subtract assignment)";
        break;
    case O::MultiplyAssignment:
        return "*= (Multiply assignment)";
        break;
    case O::DivideAssignment:
        return "/= (Divide assignment)";
        break;
    case O::ModuloAssignment:
        return "%= (Modulo assignment)";
        break;
    case O::OrAssignment:
        return "|= (OR assignment)";
        break;
    case O::AndAssignment:
        return "&= (AND assignment)";
        break;
    case O::XorAssignment:
        return "^= (XOR assignment)";
        break;
    case O::LeftshiftAssignment:
        return "<<= (Leftshift Assignment)";
        break;
    case O::RightshiftAssignment:
        return ">>= (Rightshift Assignment)";
        break;
    case O::Assignment:
        return "= (Assignment)";
        break;
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
    case O::Leftparen:
        return "( (Left paren)";
        break;
    case O::Rightparen:
        return ") (Right paren)";
        break;
    case O::Equals:
        return "== (Equality)";
        break;
    case O::NotEquals:
        return "!= (Inequality)";
        break;
    case O::LessThanEqual:
        return "<= (Less than equal to)";
        break;
    case O::GreaterThanEqual:
        return ">= (Greater than equal to)";
        break;
    case O::LessThan:
        return "< (Less than)";
        break;
    case O::GreaterThan:
        return "> (Greater than)";
        break;
    case O::PostfixPlusPlus:
        return "++ (Postfix Increment)";
        break;
    case O::PostfixMinusMinus:
        return "++ (Postfix Decrement)";
        break;
    case O::PrefixPlusPlus:
        return "++ (Prefix Increment)";
        break;
    case O::PrefixMinusMinus:
        return "++ (Prefix Decrement)";
        break;
    case O::Negative:
        return "- (Unary Negative)";
        break;
    case O::Positive:
        return "+ (Unary Positive)";
        break;
    case O::SizeOf:
        return "Sizeof";
        break;
    case O::AddressOf:
        return "& (Unary Address of)";
        break;
    case O::Dereference:
        return "* (Unary pointer dereference)";
        break;
    case O::LogicalAnd:
        return "&& (Binary logical and)";
        break;
    case O::LogicalOr:
        return "|| (Binary logical or)";
        break;
    default:
        break;
    }
    return "";
}

std::string_view hclang::typeToString(hclang::HCType type) {
    using hct = hclang::HCType;
    switch (type) {
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
    throw std::invalid_argument(fmt::format("{} is not a valid HCType", static_cast<int>(type)));
}

std::size_t hclang::sizeofType(hclang::typeInfo t1) {
    using hct = hclang::HCType;
    switch (t1.type) {
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
    switch (t.type) {
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
    switch (op) {
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
    case O::Negative:
        return 12;
        break;
    case O::PrefixPlusPlus:
    case O::PrefixMinusMinus:
    case O::LogicalNot:
    case O::BitwiseNot:
    case O::Dereference:
    case O::AddressOf:
    case O::SizeOf:
        return 13;
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
    switch (op) {
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
    switch (op) {
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
    if (hclang::operatorIsPostfix(op) || hclang::operatorIsPrefix(op)) {
        return 1;
    } else if (op == O::Leftparen || op == O::Rightparen) {
        return 0;
    } else if (op == O::Ternary) {
        return 3;
    }
    return 2;
}

std::uint64_t hclang::sizeOf(typeInfo ti) {
    if (ti.id.getId() != "") {
        // Custom types.
        return 0;
    } else if (ti.pointer) {
        return 0;
    }

    switch (ti.type) {
    case hct::U8i:
    case hct::I8i:
        return 1;
        break;
    case hct::U16i:
    case hct::I16i:
        return 2;
        break;
    case hct::U32i:
    case hct::I32i:
        return 4;
        break;
    case hct::U64i:
    case hct::I64i:
        return 8;
        break;
    default:
        break;
    }
    return 0;
}
