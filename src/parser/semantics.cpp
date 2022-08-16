#include <string_view>

#include "ast.hpp"

using namespace std::string_view_literals;
using hct = hclang::HCType;

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

void hclang::BinaryOperator::parseSemantics(semanticContext &sc) {
    // Rules: if either side is a float, the other side is cast to a float.
    // If boths sides are ints:
    //    If one side is larger than the other, the smaller side is promoted
    //    If either side is unsigned, the other side becomes unsigned.

    mLhs->parseSemantics(sc);
    mRhs->parseSemantics(sc);

    auto lType = mLhs->getType();
    auto rType = mRhs->getType();

    if (!lType.isIntrinsic() || lType.isVoid()) {
        // TODO error
        throw std::invalid_argument("Ltype is wrong.");
    }
    if (!rType.isIntrinsic() || rType.isVoid()) {
        // TODO error
        throw std::invalid_argument("Rtype is wrong.");
    }

    if (lType == rType) {
        mType = lType;
        return;
    }

    if (lType.isFloat() || hclang::sizeOf(lType) > hclang::sizeOf(rType) ||
        (lType.isUnsigned() && rType.isSigned()) || isAssignment()) {
        mRhs = hclang::makeImpCast(mRhs, lType);
        mType = lType;
    } else if (rType.isFloat() || hclang::sizeOf(lType) < hclang::sizeOf(rType) ||
               (rType.isUnsigned() && lType.isSigned()) && !isAssignment()) {
        mLhs = hclang::makeImpCast(mLhs, rType);
        mType = rType;
    }
}

void hclang::BinaryAssignment::parseSemantics(semanticContext &sc) {
    mLhs->parseSemantics(sc);
    mRhs->parseSemantics(sc);

    auto lType = mLhs->getType();
    auto rType = mRhs->getType();

    if (lType.isVoid()) {
        throw std::invalid_argument("L is void");
    }
    if (rType.isVoid()) {
        throw std::invalid_argument("R is void");
    }

    if (lType == rType) {
        mType = lType;
        return;
    }

    if (!lType.isIntrinsic()) {
        throw std::invalid_argument("L is not intrinsic");
    }

    if (!rType.isIntrinsic()) {
        throw std::invalid_argument("R is not intrinsic");
    }

    mRhs = makeImpCast(mRhs, lType);
    mType = lType;
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

    auto rType = mRhs->getType();

    if (rType == mType) {
        return;
    }

    if (mType.isInteger()) {
        // Convert rhs to our type.
        mRhs = hclang::makeImpCast(mRhs, mType);
        return;
    }
    if (mType.isFloat()) {
        // Convert rhs to our type.
        if (mRhs->getType().isInteger()) {
            mRhs = hclang::makeImpCast(mRhs, mType);
        }
        return;
    }

    throw std::invalid_argument("Rhs and lhs types do not match");
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
    for (auto &maybeArg : mArgExps) {
        maybeArg->parseSemantics(sc);
    }
}

void hclang::Return::parseSemantics(semanticContext &sc) {
    // Returns in global scope return i32's.
    typeInfo ti = {""sv, nullptr, HCType::U32i};  // Function return type.
    if (sc.curFunc) {
        ti = (*sc.curFunc)->getType();
    }
    // First check if function is void, and if so, so is the return.

    if (ti.isVoid()) {
        if (mExp) {
            throw std::invalid_argument("Function is void but something is returned");
        }
        return;
    }

    if (mExp) {
        mExp->parseSemantics(sc);
    } else {
        throw std::invalid_argument("Function is non-void, but return has no argument");
    }

    auto retType = mExp->getType();
    if (retType == ti) {
        return;
    }

    // Insert implicit cast.
    if (!retType.isIntrinsic()) {
        throw std::invalid_argument("Return type is not intrinsic, cannot be cast");
    }

    if (!ti.isIntrinsic()) {
        throw std::invalid_argument("Return type is not intrinsic, cannot be cast");
    }

    // Coerce type being returned to type of function.
    mExp = makeImpCast(mExp, ti);
}

void hclang::Label::parseSemantics(semanticContext &sc) {
}

void hclang::Goto::parseSemantics(semanticContext &sc) {
}
