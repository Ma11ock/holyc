#include <stdexcept>
#include <string_view>

#include "ast.hpp"

using namespace std::string_view_literals;
using hct = hclang::HCType;

// Type promotion heiarchy:
// F64
// U64, Pointers
// I64
// U32
// I32
// U16
// I16
// U8
// I8
static void checkImplicitCast(hclang::typeInfo lType, hclang::exp &rhs) {
    auto rType = rhs->getType();

    if (lType.isVoid() || rType.isVoid()) {
        throw std::invalid_argument("Void");
    }

    if (lType == rType) {
        return;
    }

    if (!lType.isIntrinsic()) {
        throw std::invalid_argument(fmt::format("LHS is {}, not intrinsic.", lType));
    }

    if (!rType.isIntrinsic()) {
        throw std::invalid_argument(fmt::format("RHS is {}, not intrinsic.", rType));
    }

    if (lType.isVoid()) {
        throw std::invalid_argument("L is void");
    }
    if (rType.isVoid()) {
        throw std::invalid_argument("R is void");
    }

    rhs = hclang::makeImpCast(rhs, lType);
}

// Type promotion heiarchy:
// F64
// U64, Pointers
// I64
// U32
// I32
// U16
// I16
// U8
// I8
static hclang::typeInfo checkImplicitCast(hclang::exp &lhs, hclang::exp &rhs,
                                          bool isAssignment = false, bool allowVoid = false) {
    allowVoid = isAssignment ? false : allowVoid;

    auto lType = lhs->getType();
    auto rType = rhs->getType();

    if (lType.isVoid() || rType.isVoid()) {
        if (allowVoid) {
            throw std::invalid_argument("Void");
        } else {
            return lType;
        }
    }

    if (lType == rType) {
        return lType;
    }

    if (!lType.isIntrinsic()) {
        throw std::invalid_argument(fmt::format("LHS is {}, not intrinsic.", lType));
    }

    if (!rType.isIntrinsic()) {
        throw std::invalid_argument(fmt::format("RHS is {}, not intrinsic.", rType));
    }

    if (lType.isVoid()) {
        throw std::invalid_argument("L is void");
    }
    if (rType.isVoid()) {
        throw std::invalid_argument("R is void");
    }

    hclang::typeInfo result;

    if (lType.isFloat() || hclang::sizeOf(lType) > hclang::sizeOf(rType) ||
        (lType.isUnsigned() && rType.isSigned()) || isAssignment || lType.isPointer()) {
        rhs = hclang::makeImpCast(rhs, lType);
        result = lType;
    } else if (rType.isFloat() || hclang::sizeOf(lType) < hclang::sizeOf(rType) ||
               (rType.isUnsigned() && lType.isSigned()) && !isAssignment || rType.isPointer()) {
        lhs = hclang::makeImpCast(lhs, rType);
        result = rType;
    }

    return result;
}

/* NOTE: ParseSemantics should always allow its children to semantically parse first,
   because their type may change. */

void hclang::If::parseSemantics(semanticContext &sc) {
    mConditional->parseSemantics(sc);

    if (auto type = mConditional->getType(); type.isIntrinsic() && !type.isVoid()) {
        // Cast to U64i to make comparisons uniform.
        if (type.type != HCType::U64i) {
            mConditional = hclang::makeImpCast(mConditional, typeInfo{});
        }
    } else {
        throw std::runtime_error("Error: if statement conditional must take an \
expression that returns an intrinsic type");
    }

    mBody->parseSemantics(sc);
    for (auto &elif : mElseIfs) {
        elif->parseSemantics(sc);
    }
    if (mElseBody) {
        mElseBody->parseSemantics(sc);
    }
}

void hclang::While::parseSemantics(semanticContext &sc) {
    mConditional->parseSemantics(sc);
    if (auto type = mConditional->getType(); type.isIntrinsic() && !type.isVoid()) {
        // Cast to U64i to make comparisons uniform.
        if (type.type != HCType::U64i) {
            mConditional = hclang::makeImpCast(mConditional, typeInfo{});
        }
    } else {
        throw std::runtime_error("Error: if statement conditional must take an \
expression that returns an intrinsic type");
    }

    mBody->parseSemantics(sc);
}

void hclang::For::parseSemantics(semanticContext &sc) {
    if (mConditional) {
        mConditional->parseSemantics(sc);
        if (auto type = mConditional->getType(); type.isIntrinsic() && !type.isVoid()) {
            // Cast to U64i to make comparisons uniform.
            if (type.type != HCType::U64i) {
                mConditional = hclang::makeImpCast(mConditional, typeInfo{});
            }
        } else {
            throw std::runtime_error("Error: if statement conditional must take an \
expression that returns an intrinsic type");
        }
    } else {
        // Insert implicit true.
        mConditional = makeIntConst(1, typeInfo{});
    }

    for (auto &startExp : mStartExps) {
        startExp->parseSemantics(sc);
    }

    for (auto &endExp : mEndExps) {
        endExp->parseSemantics(sc);
    }

    mBody->parseSemantics(sc);
}

void hclang::ElseIf::parseSemantics(semanticContext &sc) {
    if (auto type = mConditional->getType(); type.isIntrinsic() && !type.isVoid()) {
        if (type.type != HCType::U64i) {
            mConditional = hclang::makeImpCast(mConditional, typeInfo{});
        }
        // Compare to 0.
        mConditional = makeBinOp(Operator::NotEquals, mConditional, makeIntConst(0, typeInfo{}));
    } else {
        throw std::runtime_error("Error: if statement conditional must take an \
expression that returns an intrinsic type");
    }

    mBody->parseSemantics(sc);
}

void hclang::Cast::parseSemantics(semanticContext &sc) {
    mExpr->parseSemantics(sc);
}

void hclang::ImplicitCast::parseSemantics(semanticContext &sc) {
    mExpr->parseSemantics(sc);
}

void hclang::BinaryOperator::parseSemantics(semanticContext &sc) {
    // Rules: if either side is a float, the other side is cast to a float.
    // If boths sides are ints:
    //    If one side is larger than the other, the smaller side is promoted
    //    If either side is unsigned, the other side becomes unsigned.

    mLhs->parseSemantics(sc);
    mRhs->parseSemantics(sc);

    mType = checkImplicitCast(mLhs, mRhs, hclang::isAssignment(mOp));
}

void hclang::BinaryAssignment::parseSemantics(semanticContext &sc) {
    mLhs->parseSemantics(sc);
    mRhs->parseSemantics(sc);

    checkImplicitCast(mType, mRhs);
}

void hclang::UnaryOperator::parseSemantics(semanticContext &sc) {
    mExpr->parseSemantics(sc);

    auto childType = mExpr->getType();

    if (isArithmetic(mOp) && !childType.isIntrinsic()) {
        throw std::invalid_argument("Not an intrinsic");
    }

    mType = childType;
}

void hclang::IntegerConstant::parseSemantics(semanticContext &sc) {
}

void hclang::CompoundStatement::parseSemantics(semanticContext &sc) {
    for (auto &statement : mStatementList) {
        statement->parseSemantics(sc);
    }
}

void hclang::Declaration::parseSemantics(semanticContext &sc) {
}

void hclang::FunctionDefinition::parseSemantics(semanticContext &sc) {
    mBody->parseSemantics(sc);
}

void hclang::FunctionDeclaration::parseSemantics(semanticContext &sc) {
    for (auto arg : mArgs) {
        arg->parseSemantics(sc);
    }
    if (mDefinition) {
        sc.curFunc = mDefinition;
        mDefinition->parseSemantics(sc);
        sc.curFunc = std::nullopt;
    }
}

void hclang::VariableDeclaration::parseSemantics(semanticContext &sc) {
}

void hclang::VariableInitialization::parseSemantics(semanticContext &sc) {
    mRhs->parseSemantics(sc);

    checkImplicitCast(mType, mRhs);
}

void hclang::DeclarationReference::parseSemantics(semanticContext &sc) {
    mDeclRef->parseSemantics(sc);
}

void hclang::LToRValue::parseSemantics(semanticContext &sc) {
    mDeclRef->parseSemantics(sc);
    mType = mDeclRef->getType();
}

void hclang::DeclarationStatement::parseSemantics(semanticContext &sc) {
    for (auto &decl : mDecls) {
        decl->parseSemantics(sc);
    }
}

void hclang::Program::parseSemantics(semanticContext &sc) {
    for (auto &statement : mStatements) {
        getPD(statement)->parseSemantics(sc);
    }
}

void hclang::FunctionCall::parseSemantics(semanticContext &sc) {
    // Cast to function call types.
    auto funcArgs = mFunc->getArgsRef();
    auto funcIter = funcArgs.begin();
    for (auto expIter = mArgExps.begin(); expIter != mArgExps.end() && funcIter != funcArgs.end();
         expIter++, funcIter++) {
        (*expIter)->parseSemantics(sc);
        auto curExp = *expIter;
        checkImplicitCast((*funcIter)->getType(), curExp);
        *expIter = curExp;
    }
}

void hclang::Return::parseSemantics(semanticContext &sc) {
    mExp->parseSemantics(sc);
    // Returns in global scope return i32's.
    typeInfo ti = {""sv, nullptr, HCType::U32i};  // Function return type.
    if (sc.curFunc) {
        ti = (*sc.curFunc)->getType();
    }
    // First check if function is void, and if so, so is the return.
    checkImplicitCast(ti, mExp);
}

void hclang::Label::parseSemantics(semanticContext &sc) {
}

void hclang::Goto::parseSemantics(semanticContext &sc) {
}
