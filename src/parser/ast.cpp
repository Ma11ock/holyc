#include "ast.hpp"

#include <fmt/core.h>
#include <regex>

// Conditionals.

slang::Conditional::Conditional(slang::Conditional::Type type, Exp cond1)
    : mType(type),mCond1(cond1),mCond2(nullptr) {
    if(type != Type::None) {
        throw std::invalid_argument(fmt::format("Conditional: Not enough operands: {}", *this));
    }
    if(mCond1 == nullptr) {
        throw std::invalid_argument(fmt::format("Conditional: No conditional: {}", *this));
    }
}

slang::Conditional::Conditional(slang::Conditional::Type type, Exp cond1, Exp cond2)
    : mType(type),mCond1(cond1),mCond2(cond2) {
    if(type != Type::None) {
        if(mCond1 == nullptr || mCond2 == nullptr) {
            throw std::invalid_argument
                (fmt::format("Conditional: Not enough conditionals: {}", *this));
        }
    }
    if(mCond1 == nullptr) {
        throw std::invalid_argument(fmt::format("Conditional: No conditional: {}", *this));
    }
}

slang::LLV slang::Conditional::toLLVM() const {
    return nullptr;
}

std::string slang::Conditional::stringify() const {
    return fmt::format("Type: {}, Conditional 1: {}, Condtional 2: {}",
                       stringifyType(), mCond1, mCond2);
}

// Negate.

slang::LLV slang::Negate::toLLVM() const {
    return nullptr;
}

std::string slang::Negate::stringify() const {
    return fmt::format("Negate: -{}", mExp);
}


// Real.

slang::Real::Real(std::string_view source) : mNum(0.0) {
    auto [ptr, ec] = std::from_chars(source.begin(), source.end(), mNum);

    if(ptr != source.end()) {
        throw std::invalid_argument(fmt::format("{} is not a valid double",
                                                source));
    }

    switch(ec) {
    case std::errc::invalid_argument:
        throw std::invalid_argument(fmt::format("{} is not a valid double",
                                                source));
        break;
    case std::errc::result_out_of_range:
        throw std::range_error(fmt::format("{} is out of range of 64 bit float",
                                           source));
        break;
    default:
        break;
    }
}

slang::LLV slang::Real::toLLVM() const {
    return nullptr;
}

std::string slang::Real::stringify() const {
    return fmt::format("Real: {}", mNum);
}

// Character.

slang::Character::Character(std::string_view source) : mChar('\0') {
    if(source.size() >= 1) {
        throw std::invalid_argument("Character: \"{}\" is a string not a character");
    } else if(source.empty()) {
        throw std::invalid_argument("Character: source string is empty");
    }

    mChar = source[0];
}

slang::LLV slang::Character::toLLVM() const {
    return nullptr;
}

std::string slang::Character::stringify() const {
    return fmt::format("Character: {}", mChar);
}

// Boolean.

slang::Boolean::Boolean(std::string_view source) : mBool(false) {
    const static std::regex trueRegex("true", std::regex_constants::icase |
                                      std::regex_constants::ECMAScript);
    const static std::regex falseRegex("false", std::regex_constants::icase |
                                       std::regex_constants::ECMAScript);
    if(std::regex_match(source.begin(), source.end(), trueRegex)) {
        mBool = true;
    } else if(std::regex_match(source.begin(), source.end(), falseRegex)) {
        mBool = false;
    } else {
        throw std::invalid_argument(fmt::format("\"{}\" is not a valid boolean", source));
    }
}

slang::LLV slang::Boolean::toLLVM() const {
    return nullptr;
}

std::string slang::Boolean::stringify() const {
    return fmt::format("Boolean: {}", mBool);
}


// Integer.

slang::Integer::Integer(std::string_view source) : mNum(0) {
    auto [ptr, ec] = std::from_chars(source.begin(), source.end(), mNum);

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

slang::LLV slang::Integer::toLLVM() const {
    return nullptr;
}

std::string slang::Integer::stringify() const {
    return fmt::format("Integer: {}", mNum);
}

// Text.

slang::LLV slang::Text::toLLVM() const {
    return nullptr;
}

std::string slang::Text::stringify() const {
    return fmt::format("Text: {}", mText);
}

// Identifier.
slang::LLV slang::Identifier::toLLVM() const {
    return nullptr;
}

std::string slang::Identifier::stringify() const {
    return fmt::format("Identifier: {}", mId);
}

// Binary operator.

std::string slang::BinaryOperator::stringify() const {
    return fmt::format("LHS: {}, RHS: {}", mLeft, mRight);
}

// (Binary) Plus.

slang::LLV slang::Plus::toLLVM() const {
    return nullptr;
}

std::string slang::Plus::stringify() const {
    return fmt::format("Plus: {}", BinaryOperator::stringify());
}

// (Binary) minus.

slang::LLV slang::Minus::toLLVM() const {
    return nullptr;
}

std::string slang::Minus::stringify() const {
    return fmt::format("Minus: {}", BinaryOperator::stringify());
}


// Multiply.

slang::LLV slang::Multiply::toLLVM() const {
    return nullptr;
}

std::string slang::Multiply::stringify() const {
    return fmt::format("Multiply: {}", BinaryOperator::stringify());
}

// Divide.

slang::LLV slang::Divide::toLLVM() const {
    return nullptr;
}

std::string slang::Divide::stringify() const {
    return fmt::format("Divide: {}", BinaryOperator::stringify());
}

// LessThanEqual.

slang::LLV slang::LessThanEqual::toLLVM() const {
    return nullptr;
}

std::string slang::LessThanEqual::stringify() const {
    return fmt::format("LessThanEqual: {}", BinaryOperator::stringify());
}

// LessThan.

slang::LLV slang::LessThan::toLLVM() const {
    return nullptr;
}

std::string slang::LessThan::stringify() const {
    return fmt::format("LessThan: {}", BinaryOperator::stringify());
}

// GreaterThan.

slang::LLV slang::GreaterThan::toLLVM() const {
    return nullptr;
}

std::string slang::GreaterThan::stringify() const {
    return fmt::format("GreaterThan: {}", BinaryOperator::stringify());
}

// GreaterThanEqual.

slang::LLV slang::GreaterThanEqual::toLLVM() const {
    return nullptr;
}

std::string slang::GreaterThanEqual::stringify() const {
    return fmt::format("GreaterThanEqual: {}", BinaryOperator::stringify());
}

// NotEquals.

slang::LLV slang::NotEquals::toLLVM() const {
    return nullptr;
}

std::string slang::NotEquals::stringify() const {
    return fmt::format("NotEquals: {}", BinaryOperator::stringify());
}

// Equals.

slang::LLV slang::Equals::toLLVM() const {
    return nullptr;
}

std::string slang::Equals::stringify() const {
    return fmt::format("Equals: {}", BinaryOperator::stringify());
}

// Assign.

slang::LLV slang::Assign::toLLVM() const {
    return nullptr;
}

std::string slang::Assign::stringify() const {
    return fmt::format("Assign: LHS: {}, RHS: {}", mId, mExp);
}

// Block.

slang::LLV slang::Block::toLLVM() const {
    return nullptr;
}

std::string slang::Block::stringify() const {
    if(isEmpty()) {
        return fmt::format("Block: \\{Empty\\}");
    }

    std::string s = "";
    for(const auto &stmnt : mStatements) {
        s += fmt::format("\t{}\n", stmnt);
    }
    return fmt::format("Block: {}", s);
}

// While.

slang::LLV slang::While::toLLVM() const {
    return nullptr;
}

std::string slang::While::stringify() const {
    return fmt::format("While: Boolexpr: {}, Body: \t{}", mBoolExp, mStatements);
}

// If.

slang::LLV slang::If::toLLVM() const {
    return nullptr;
}

std::string slang::If::stringify() const {
    std::string elifs = "";
    for(const auto &elif : mElifBlocks) {
        elifs += fmt::format("\t{}\n", elif);
    }
    return fmt::format("If: Boolexpr: {}, Body: \t{}, else ifs: {}, else: {}",
                       mBoolExp, mStatements, elifs, mElse);
}
