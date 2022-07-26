#include "ast.hpp"
#include <string_view>

using namespace std::string_view_literals;
using hct = hclang::HCType;

void hclang::If::parseSemantics() {
    if(auto type = mConditional->getType();
       type.isIntrinsic() && !type.isVoid()) {
        if(type.type != HCType::U64i) {
            mConditional = hclang::makeImpCast(mConditional, typeInfo {});
        }
        // Compare to 0.
        mConditional = makeBinOp(Operator::NotEquals, mConditional, makeIntConst(0, typeInfo {}));
    } else {
        throw std::runtime_error("Error: if statement conditional must take an \
expression that returns an intrinsic type");
    }
}

void hclang::Cast::parseSemantics() {
}

void hclang::ImplicitCast::parseSemantics() {
}

// Type promotion Heiarchy:
// F64
// U64
// I64
// U32
// I32
// U16
// I16
// U8
// I8

void hclang::BinaryOperator::parseSemantics() {
    // Rules: if either side is a float, the other side is cast to a float.
    // If boths sides are ints:
    //    If one side is larger than the other, the smaller side is promoted
    //    If either side is unsigned, the other side becomes unsigned.

    auto lType = mLhs->getType();
    auto rType = mRhs->getType();

    if(!lType.isIntrinsic() || lType.isVoid()) {
        // TODO error
        throw std::invalid_argument("Ltype is wrong.");
    }
    if(!rType.isIntrinsic() || rType.isVoid()) {
        // TODO error
        throw std::invalid_argument("Rtype is wrong.");
    }

    if(lType == rType) {
        return;
    }

    if(lType.isFloat() ||
       hclang::sizeOf(lType) > hclang::sizeOf(rType) ||
       lType.isUnsigned() && rType.isSigned()) {

        mRhs = hclang::makeImpCast(mRhs, lType);
    } else if(rType.isFloat() ||
              hclang::sizeOf(lType) < hclang::sizeOf(rType) ||
              rType.isUnsigned() && lType.isSigned()) {

        mLhs = hclang::makeImpCast(mLhs, rType);
    }
}

void hclang::UnaryOperator::parseSemantics() {
    // TODO
}

void hclang::IntegerConstant::parseSemantics() {
}


void hclang::CompoundStatement::parseSemantics() {
    for(auto &statement : mStatementList) {
        statement->parseSemantics();
    }
}

void hclang::Declaration::parseSemantics() {
}

void hclang::FunctionDefinition::parseSemantics() {
}

void hclang::FunctionDeclaration::parseSemantics() {
}

void hclang::VariableDeclaration::parseSemantics() {
}


void hclang::VariableInitialization::parseSemantics() {
}

void hclang::DeclarationReference::parseSemantics() {
}


void hclang::DeclarationStatement::parseSemantics() {
}

void hclang::Program::parseSemantics() {
    for(auto &statement : mStatements) {
        getPD(statement)->parseSemantics();
    }
}


void hclang::Return::parseSemantics() {
    if(mExp) {
        mExp->parseSemantics();
    }
}
