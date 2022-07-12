#include "ast.hpp"

#include <fmt/core.h>
#include <regex>

using TT = slang::TokenType;
using O = slang::Operator;

// GrammarRule.

std::string slang::GrammarRule::stringify() const {
    return fmt::format("{} 0x{} <line:{}:{},col:{}:{}>", getClassName(),
                       reinterpret_cast<std::size_t>(this), mLexeme.getStartLineNumber(),
                       mLexeme.getEndLineNumber(), mLexeme.getStartColNumber(),
                       mLexeme.getEndColNumber());
}

// Unary Operator.

// std::string slang::UnaryOperator::stringify() const {
//     return fmt::format("Exp: {}", mExp);
// }

// // Negate.

// slang::LLV slang::Negate::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Negate::stringify() const {
//     return fmt::format("Negate: {}", slang::UnaryOperator::stringify());
// }

// // Unaryplus.

// slang::LLV slang::UnaryPlus::toLLVM() const {
//     return nullptr;
// }

// std::string slang::UnaryPlus::stringify() const {
//     return fmt::format("UnaryPlus: {}", slang::UnaryOperator::stringify());
// }


// // Real.

// slang::Real::Real(std::string_view source) : mNum(0.0) {
//     auto [ptr, ec] = std::from_chars(source.begin(), source.end(), mNum);

//     if(ptr != source.end()) {
//         throw std::invalid_argument(fmt::format("{} is not a valid double",
//                                                 source));
//     }

//     switch(ec) {
//     case std::errc::invalid_argument:
//         throw std::invalid_argument(fmt::format("{} is not a valid double",
//                                                 source));
//         break;
//     case std::errc::result_out_of_range:
//         throw std::range_error(fmt::format("{} is out of range of 64 bit float",
//                                            source));
//         break;
//     default:
//         break;
//     }
// }

// slang::LLV slang::Real::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Real::stringify() const {
//     return fmt::format("Real: {}", mNum);
// }

// // Character.

// slang::Character::Character(std::string_view source) : mChar('\0') {
//     if(source.size() >= 1) {
//         throw std::invalid_argument("Character: \"{}\" is a string not a character");
//     } else if(source.empty()) {
//         throw std::invalid_argument("Character: source string is empty");
//     }

//     mChar = source[0];
// }

// slang::LLV slang::Character::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Character::stringify() const {
//     return fmt::format("Character: {}", mChar);
// }

// // Boolean.

// slang::Boolean::Boolean(std::string_view source) : mBool(false) {
//     const static std::regex trueRegex("true", std::regex_constants::icase |
//                                       std::regex_constants::ECMAScript);
//     const static std::regex falseRegex("false", std::regex_constants::icase |
//                                        std::regex_constants::ECMAScript);
//     if(std::regex_match(source.begin(), source.end(), trueRegex)) {
//         mBool = true;
//     } else if(std::regex_match(source.begin(), source.end(), falseRegex)) {
//         mBool = false;
//     } else {
//         throw std::invalid_argument(fmt::format("\"{}\" is not a valid boolean", source));
//     }
// }

// slang::LLV slang::Boolean::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Boolean::stringify() const {
//     return fmt::format("Boolean: {}", mBool);
// }


// // Integer.

// slang::LLV slang::Integer::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Integer::stringify() const {
//     return fmt::format("Integer: {}", mNum);
// }

// // Text.

// slang::LLV slang::Text::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Text::stringify() const {
//     return fmt::format("Text: {}", mText);
// }

// slang::BinaryOperator::BinaryOperator(slang::Exp left, slang::Exp right)
//     : mLeft(left),mRight(right) {
//     if(left == nullptr) {
//         throw std::invalid_argument(fmt::format("{}: Left hand side should not be NULL.",
//                                                 getBOpName()));
//     }
//     if(right == nullptr) {
//         throw std::invalid_argument(fmt::format("{}: Right hand side should not be NULL.",
//                                                 getBOpName()));
//     }
// }

// std::string slang::BinaryOperator::stringify() const {
//     return fmt::format("LHS: {}, RHS: {}", mLeft, mRight);
// }

// // LogicalAnd.

// slang::LLV slang::LogicalAnd::toLLVM() const {
//     return nullptr;
// }

// std::string slang::LogicalAnd::stringify() const {
//     return fmt::format("LogicalAnd: {}", BinaryOperator::stringify());
// }

// std::string_view slang::LogicalAnd::getBOpName() const {
//     return "LogicalAnd";
// }

// // LogicalOr.

// slang::LLV slang::LogicalOr::toLLVM() const {
//     return nullptr;
// }

// std::string slang::LogicalOr::stringify() const {
//     return fmt::format("LogicalOr: {}", BinaryOperator::stringify());
// }

// std::string_view slang::LogicalOr::getBOpName() const {
//     return "LogicalOr";
// }

// // (Binary) Plus.

// slang::LLV slang::Plus::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Plus::stringify() const {
//     return fmt::format("Plus: {}", BinaryOperator::stringify());
// }

// std::string_view slang::Plus::getBOpName() const {
//     return "Plus";
// }

// // (Binary) minus.

// slang::LLV slang::Minus::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Minus::stringify() const {
//     return fmt::format("Minus: {}", BinaryOperator::stringify());
// }

// std::string_view slang::Minus::getBOpName() const {
//     return "Minus";
// }

// // Multiply.

// slang::LLV slang::Multiply::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Multiply::stringify() const {
//     return fmt::format("Multiply: {}", BinaryOperator::stringify());
// }

// std::string_view slang::Multiply::getBOpName() const {
//     return "Multiply";
// }

// // Divide.

// slang::LLV slang::Divide::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Divide::stringify() const {
//     return fmt::format("Divide: {}", BinaryOperator::stringify());
// }

// std::string_view slang::Divide::getBOpName() const {
//     return "Divide";
// }

// // LessThanEqual.

// slang::LLV slang::LessThanEqual::toLLVM() const {
//     return nullptr;
// }

// std::string slang::LessThanEqual::stringify() const {
//     return fmt::format("LessThanEqual: {}", BinaryOperator::stringify());
// }

// std::string_view slang::LessThanEqual::getBOpName() const {
//     return "LessThanEqual";
// }

// // LessThan.

// slang::LLV slang::LessThan::toLLVM() const {
//     return nullptr;
// }

// std::string slang::LessThan::stringify() const {
//     return fmt::format("LessThan: {}", BinaryOperator::stringify());
// }

// std::string_view slang::LessThan::getBOpName() const {
//     return "LessThan";
// }

// // GreaterThan.

// slang::LLV slang::GreaterThan::toLLVM() const {
//     return nullptr;
// }

// std::string slang::GreaterThan::stringify() const {
//     return fmt::format("GreaterThan: {}", BinaryOperator::stringify());
// }

// std::string_view slang::GreaterThan::getBOpName() const {
//     return "GreaterThan";
// }

// // GreaterThanEqual.

// slang::LLV slang::GreaterThanEqual::toLLVM() const {
//     return nullptr;
// }

// std::string slang::GreaterThanEqual::stringify() const {
//     return fmt::format("GreaterThanEqual: {}", BinaryOperator::stringify());
// }

// std::string_view slang::GreaterThanEqual::getBOpName() const {
//     return "GreaterThanEqual";
// }

// // NotEquals.

// slang::LLV slang::NotEquals::toLLVM() const {
//     return nullptr;
// }

// std::string slang::NotEquals::stringify() const {
//     return fmt::format("NotEquals: {}", BinaryOperator::stringify());
// }

// std::string_view slang::NotEquals::getBOpName() const {
//     return "NotEquals";
// }

// // Equals.

// slang::LLV slang::Equals::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Equals::stringify() const {
//     return fmt::format("Equals: {}", BinaryOperator::stringify());
// }

// std::string_view slang::Equals::getBOpName() const {
//     return "Equals";
// }

// // Assign.

// slang::LLV slang::Assign::toLLVM() const {
//     return nullptr;
// }

// std::string slang::Assign::stringify() const {
//     return fmt::format("Assign: LHS: {}, RHS: {}", mId, mExp);
// }

// std::string_view slang::Assign::getBOpName() const {
//     return "Assign";
// }

// Block.

// std::string slang::Block::stringify() const {
//     if(isEmpty()) {
//         return fmt::format("Block: \\{Empty\\}");
//     }

//     std::string s = "";
//     for(const auto &stmnt : mStatements) {
//         s += fmt::format("\t{}\n", stmnt);
//     }
//     return fmt::format("Block: {}", s);
// }

// // // While.

// // slang::LLV slang::While::toLLVM() const {
// //     return nullptr;
// // }

// // std::string slang::While::stringify() const {
// //     return fmt::format("While: Boolexpr: {}, Body: \t{}", mBoolExp, mStatements);
// // }

// // If.

// std::string slang::If::stringify() const {
//     std::string elifs = "";
//     for(const auto &elif : mElifBlocks) {
//         elifs += fmt::format("\t{}\n", elif);
//     }
//     return fmt::format("If: Boolexpr: {}, Body: \t{}, else ifs: {}, else: {}",
//                        mBoolExp, mStatements, elifs, mElse);
// }

// // IntegerConstant.

// slang::LLV slang::IntegerConstant::toLLVM() const {
//     return nullptr;
// }

// std::string slang::IntegerConstant::stringify() const {
//     return fmt::format("IntegerConstant: {}", mValue);
// }

// slang::IntegerConstant::IntegerConstant(std::string_view value) : mValue(0) {
//     auto [ptr, ec] = std::from_chars(value.begin(), value.end(), mValue);

//     if(ptr != value.end()) {
//         throw std::invalid_argument(fmt::format("{} is not a valid int64",
//                                                 value));
//     }

//     switch(ec) {
//     case std::errc::invalid_argument:
//         throw std::invalid_argument(fmt::format("{} is not a valid int64",
//                                                 value));
//         break;
//     case std::errc::result_out_of_range:
//         throw std::range_error(fmt::format("{} is out of range of 64 bit integer",
//                                            value));
//         break;
//     default:
//         break;
//     }
// }

// // // Variable.

// // slang::LLV slang::Variable::toLLVM() const {
// //     return nullptr;
// // }

// // std::string slang::Variable::stringify() const {
// //     return fmt::format("Variable: ID: {}, Type: {}", mId, mConstant);
// // }

// // // Procedure.

// // slang::LLV slang::Procedure::toLLVM() const {
// //     return nullptr;
// // }

// // std::string slang::Procedure::stringify() const {
// //     std::string args = "";
// //     for(const auto &a : mArgs) {
// //         args += fmt::format("{}, ", a);
// //     }
// //     return fmt::format("Procedure: ID: {}, Arguments: {}Return: {}",
// //                        mId, args, mReturn);
// // }


// New code.


std::string slang::IntegerConstant::stringify() const {
    if(mIsSigned) {
        return fmt::format("{} 'U64' {}", slang::GrammarRule::stringify(),
                           static_cast<std::int64_t>(mValue));
    }
    return fmt::format("{} 'U64' {}", slang::GrammarRule::stringify(), mValue);
}

std::string_view slang::IntegerConstant::getClassName() const {
    return "IntegerConstant";
}

// Variable declaration

std::string slang::VariableDeclaration::stringify() const {
    return fmt::format("{} Var:{} Type:{}", slang::GrammarRule::stringify(),
                       mId, "U64");
}

std::string_view slang::VariableDeclaration::getClassName() const {
    return "VariableDeclaration";
}


// Program.

void slang::Program::add(slang::programData pd) {
    mStatements.push_back(pd);
}

std::string slang::Program::stringify() const {
    return fmt::format("{}", slang::GrammarRule::stringify());
}

std::string_view slang::Program::getClassName() const {
    return "Program";
}


// CompoundStatement.


void slang::CompoundStatement::add(slang::stmnt statement) {
    mStatementList.push_back(statement);
}

std::string slang::CompoundStatement::stringify() const {
    return fmt::format("{}", slang::GrammarRule::stringify());
}

std::string_view slang::CompoundStatement::getClassName() const {
    return "CompoundStatement";
}

// BinaryOperator.

std::string slang::BinaryOperator::stringify() const {
    return fmt::format("{}: {}", slang::GrammarRule::stringify(),
                       slang::operatorToLexeme(mOp));
}

std::string_view slang::BinaryOperator::getClassName() const {
    return "BinaryOperator";
}

// DeclarationStatement


std::string slang::DeclarationStatement::stringify() const {
    return std::string(getClassName());
}

std::string_view slang::DeclarationStatement::getClassName() const {
    return "DeclarationStatement";
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
