/***************************************************************************
 * @file ast.hpp                                                           *
 * @brief Abstract syntax tree objects (interface).                        *
 ***************************************************************************/
#ifndef SLANG_AST_HPP
#define SLANG_AST_HPP

#include <fmt/core.h>

#include <charconv>
#include <cstdint>
#include <list>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <variant>

#include "../hclang.hpp"
#include "../lexer/lexer.hpp"
#include "../util.hpp"

namespace llvm {
class Value;
}

namespace hclang {
/**
 * Alias for llvm::value*.
 */
using LLV = llvm::Value *;

// Forward declaration to avoid circular includes.
template <typename T>
class SymbolTable;

// Forward declaration of struct used for LLVM code generation.
struct parserContext;

// Forward declartion of struct used for semantic parsing.
struct semanticContext;

/**
 * All valid operators in HolyC.
 */
enum class Operator {
    /// ! (unary).
    LogicalNot,
    /// - (unary).
    Negative,
    /// + (unary).
    Positive,
    /// * (unary).
    Dereference,
    /// & (unary).
    AddressOf,
    /// ~ (unary).
    BitwiseNot,
    /// ++ (unary / prefix).
    PrefixPlusPlus,
    /// ++ (unary / postfix).
    PostfixPlusPlus,
    /// -- (unary / prefix).
    PrefixMinusMinus,
    /// -- (unary / postfix).
    PostfixMinusMinus,
    /// && (binary).
    LogicalAnd,
    /// || (binary).
    LogicalOr,
    /// < (binary).
    LessThan,
    /// <= (binary).
    LessThanEqual,
    /// > (binary).
    GreaterThan,
    /// >= (binary).
    GreaterThanEqual,
    /// == (binary).
    Equals,
    /// != (binary).
    NotEquals,
    /// Add (binary).
    Add,
    /// Minus (binary).
    Subtract,
    /// Multiply (binary).
    Multiply,
    /// Divide (binary).
    Divide,
    /// % (binary).
    Modulo,
    /// << (binary).
    Leftshift,
    /// >> (binary).
    Rightshift,
    /// & (binary).
    BitwiseAnd,
    /// | (binary).
    BitwiseOr,
    /// ` (binary [right associative!]).
    Power,
    //// ^ (binary).
    BitwiseXor,
    /// = (binary (assignment), lhs not expression).
    Assignment,
    /// <<= (binary (assignment), lhs not expression).
    LeftshiftAssignment,
    /// >>= (binary (assignment), lhs not expression).
    RightshiftAssignment,
    /// *= (binary (assignment), lhs not expression).
    MultiplyAssignment,
    /// /= (binary (assignment), lhs not expression).
    DivideAssignment,
    /// &= (binary (assignment), lhs not expression).
    AndAssignment,
    /// |= (binary (assignment), lhs not expression).
    OrAssignment,
    /// ^= (binary (assignment), lhs not expression).
    XorAssignment,
    /// += (binary (assignment), lhs not expression).
    AddAssignment,
    /// -= (binary (assignment), lhs not expression).
    SubtractAssignment,
    /// %= (binary (assignment), lhs not expression).
    ModuloAssignment,
    /// ?: (ternary).
    Ternary,
    /// Lparen (control).
    Leftparen,
    /// Rparen (control).
    Rightparen,
    /// Sizeof operator.
    SizeOf,
};

inline bool isAssignment(Operator op) {
    switch (op) {
    case Operator::Assignment:
    case Operator::AddAssignment:
    case Operator::LeftshiftAssignment:
    case Operator::RightshiftAssignment:
    case Operator::SubtractAssignment:
    case Operator::MultiplyAssignment:
    case Operator::DivideAssignment:
    case Operator::ModuloAssignment:
    case Operator::OrAssignment:
    case Operator::AndAssignment:
    case Operator::XorAssignment:
    case Operator::PrefixMinusMinus:
    case Operator::PrefixPlusPlus:
    case Operator::PostfixMinusMinus:
    case Operator::PostfixPlusPlus:
        return true;
        break;
    default:
        break;
    }
    return false;
}

inline bool isArithmetic(Operator op) {
    switch (op) {
    case Operator::Ternary:
    case Operator::AddressOf:
        return false;
        break;
    default:
        break;
    }
    return true;
}

inline bool isComparison(Operator op) {
    switch (op) {
    case Operator::LessThan:
    case Operator::LessThanEqual:
    case Operator::GreaterThan:
    case Operator::GreaterThanEqual:
    case Operator::NotEquals:
    case Operator::Equals:
        return true;
    default:
        break;
    }
    return false;
}

/**
 * HolyC type categories, including intrinsic types, classes, enums,
 * and structs.
 */
enum class HCType {
    Typeless,
    Invalid,
    U0i,
    U8i,
    U16i,
    U32i,
    U64i,
    I0i,
    I8i,
    I16i,
    I32i,
    I64i,
    F64,
    Pointer,
    // Need identifiers.
    Class,
    Enum,
    Union,
};

/**
 * Type information.
 */
struct typeInfo {
    /// Identifier/typename. Only used if `type` is class, enum, or union.
    Identifier id = Identifier();
    /// Type that is pointed to. Only used if `type` is pointer.
    std::shared_ptr<typeInfo> pointer = nullptr;
    /// HolyC type category.
    HCType type = HCType::U64i;

    inline bool isIntrinsic() const {
        switch (type) {
        case HCType::Class:
        case HCType::Enum:
        case HCType::Union:
            return false;
        default:
            break;
        }
        return true;
    }

    inline typeInfo pointerTo() {
        return hclang::typeInfo{Identifier(), std::make_shared<typeInfo>(*this), HCType::Pointer};
    }

    inline bool operator==(const typeInfo &other) const {
        return (id == other.id) && (type == other.type) &&
               (((pointer && other.pointer) && (*pointer == *other.pointer)) ||
                (!pointer && !other.pointer));
    }

    inline bool isFloat() const {
        return type == HCType::F64;
    }

    inline bool isSigned() const {
        switch (type) {
        case HCType::I0i:
        case HCType::I8i:
        case HCType::I16i:
        case HCType::I32i:
        case HCType::I64i:
            return true;
        default:
            break;
        }
        return false;
    }

    inline bool isUnsigned() const {
        switch (type) {
        case HCType::U0i:
        case HCType::U8i:
        case HCType::U16i:
        case HCType::U32i:
        case HCType::U64i:
            return true;
        default:
            break;
        }
        return false;
    }

    inline typeInfo &operator*() {
        return *pointer;
    }

    inline bool isInteger() const {
        switch (type) {
        case HCType::U0i:
        case HCType::U8i:
        case HCType::U16i:
        case HCType::U32i:
        case HCType::U64i:
        case HCType::I0i:
        case HCType::I8i:
        case HCType::I16i:
        case HCType::I32i:
        case HCType::I64i:
            return true;
        default:
            break;
        }
        return false;
    }

    inline bool isVoid() const {
        switch (type) {
        case HCType::U0i:
        case HCType::I0i:
            return true;
            break;
        default:
            break;
        }
        return false;
    }

    inline bool isPointer() const {
        return type == HCType::Pointer;
    }
};

/**
 * Bitfield struct. A wrapper around a uint32. Each field represents
 * a different storage specifier.
 */
struct StorageClass {
    /// No specified storage class (default for object type).
    inline static constexpr std::uint32_t Default = 0;
    /// Hold this value in a register (compiler hint).
    inline static constexpr std::uint32_t Reg = 1;
    /// Do not hold this value in a register (compiler hint).
    inline static constexpr std::uint32_t Noreg = 2;
    /// Public/exported object.
    inline static constexpr std::uint32_t Public = 4;
    /// Static/not exported object.
    inline static constexpr std::uint32_t Static = 8;
    /// External object.
    inline static constexpr std::uint32_t Extern = 16;
    /// External asm object.
    inline static constexpr std::uint32_t _Extern = 32;

    /// Value wrapped by StorageClass.
    std::uint32_t value;

    /// Default constructor. Initialize to default.
    StorageClass() : value(StorageClass::Default) {
    }

    /**
     * Value constructor. Set `value` to `v`.
     * @param v Value to set `value` to.
     */
    StorageClass(std::uint32_t v) : value(v) {
    }

    /**
     * Copy constructor.
     * @param sc Value to set `value` to.
     */
    StorageClass(const StorageClass &sc) : value(sc.value) {
    }

    MAKE_INTEGER_FUNCS_BINARY(StorageClass, value, std::uint32_t)
    MAKE_INTEGER_FUNCS_UNARY(StorageClass, value)

    /**
     * Return true if this is a register value.
     * @return True if this a register value, else false.
     */
    inline bool isReg() const {
        return (value & Reg) != 0;
    }

    /**
     * Return true if this is a not register value.
     * @return True if this a not register value, else false.
     */
    inline bool isNoreg() const {
        return (value & Noreg) != 0;
    }

    /**
     * Return true if this is a public/exported object.
     * @return True if this a public/exported object, else false.
     */
    inline bool isPublic() const {
        return (value & Public) != 0;
    }

    /**
     * Return true if this is a static/unexported object.
     * @return True if this a static/unexported object, else false.
     */
    inline bool isStatic() const {
        return (value & Static) != 0;
    }

    /**
     * Return true if this is an externally defined object.
     * @return True if this is an externally defined object, else false.
     */
    inline bool isExtern() const {
        return (value & Extern) != 0;
    }

    /**
     * Return true if this is an externally defined asm object.
     * @return True if this is an externally defined asm object, else false.
     */
    inline bool is_Extern() const {
        return (value & _Extern) != 0;
    }
};

/**
 * Abstract parent of all AST objects. Represents a HolyC production rule.
 */
class GrammarRule {
    public:
    /**
     * Value constructor. Set the lexeme.
     * @param lexeme Value to set `lexeme` to.
     */
    GrammarRule(std::optional<Lexeme> lexeme = std::nullopt) : mLexeme(lexeme) {
    }

    /// Destructor. Default.
    virtual ~GrammarRule() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const = 0;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const = 0;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<std::shared_ptr<GrammarRule>> getChildren() const = 0;

    inline const Lexeme &getLexemeConst() const {
        const static Lexeme DEFAULT_LEX;
        if (mLexeme) {
            return *mLexeme;
        }
        return DEFAULT_LEX;
    }

    virtual void setLexeme(std::optional<Lexeme> l);

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc) = 0;

    protected:
    /// Lexeme that began this production rule.
    std::optional<hclang::Lexeme> mLexeme;

    /// Default pretty printing function.
    void printDefault() const;
};

/**
 * Alias to a shared pointer of a `GrammerRule`.
 * @see GrammarRule
 */
using GR = std::shared_ptr<GrammarRule>;

/**
 * Dummy class that represents all statement productions (assignment, if, while,
 * etc.).
 */
class Statement : public GrammarRule {
    public:
    /**
     * Value constructor. Set the lexeme.
     * @param lexeme Value to set `lexeme` to.
     */
    Statement(std::optional<Lexeme> l = std::nullopt) : GrammarRule(l) {
    }
};

/**
 * Alias to shared pointer to `Statement`.
 * @see Statement
 */
using stmnt = std::shared_ptr<Statement>;
/**
 * Alias to list of shared pointers to `Statement`.
 * @see Statement
 * @see stmnt
 */
using statementList = std::list<stmnt>;

/**
 * Dummy class for HolyC expressions (language constructs that return
 * something).
 */
class Expression : public Statement {
    public:
    /**
     * Value constructor. Sets all fields.
     * @param type Type of the integer constant. Must be Ux or Ix where x > 0.
     * @param lex Lexeme for the expression.
     */
    Expression(typeInfo type, std::optional<Lexeme> lex = std::nullopt)
        : Statement(lex), mType(type) {
    }

    /**
     * Value constructor. Sets the beginning lexeme.
     * @param l Lexeme that began this grammar production.
     */
    Expression(std::optional<Lexeme> l = std::nullopt) : Statement(l), mType() {
    }

    /// Default destructor. Defaulted.
    virtual ~Expression() = default;

    inline typeInfo getType() const {
        return mType;
    }

    virtual bool isLValue() const;

    protected:
    /// Type of integer (should only intrinsic of size > 0 or pointer).
    typeInfo mType;
};

/**
 * Alias to a shared pointer of a `GrammerRule`.
 * @see Expression
 */
using exp = std::shared_ptr<Expression>;

/**
 * Alias to a list of expressions.
 * @see exp
 */
using expList = std::list<exp>;

/**
 * Cast one intrinsic into another.
 */
class Cast : public Expression {
    public:
    Cast(exp expr, typeInfo into, std::optional<Lexeme> l = std::nullopt);
    /// Defaulted destructor.
    virtual ~Cast() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const;
    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// Expression to cast.
    exp mExpr;
};

/**
 * (Implicitly) cast one intrinsic into another.
 */
class ImplicitCast : public Cast {
    public:
    ImplicitCast(exp expr, typeInfo into) : Cast(expr, into) {
    }

    /// Defaulted destructor.
    virtual ~ImplicitCast() = default;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);
};

using impCast = std::shared_ptr<ImplicitCast>;

template <typename... Args>
inline impCast makeImpCast(Args &&...args) {
    return std::make_shared<ImplicitCast>(std::forward<Args>(args)...);
}

/**
 * Binary operator language construct (any operator with two arguments).
 */
class BinaryOperator : public Expression {
    public:
    /**
     * Value constructor. Sets all members.
     * @param op The operator category.
     * @param lhs Left hand side of the operator.
     * @param rhs Right hand side of the operator.
     */
    BinaryOperator(Operator op, exp lhs, exp rhs, std::optional<Lexeme> l = std::nullopt)
        : Expression(l), mOp(op), mLhs(lhs), mRhs(rhs) {
    }

    /// Defaulted destructor.
    virtual ~BinaryOperator() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    inline bool isAssignment() const {
        return hclang::isAssignment(mOp);
    }

    protected:
    /// Operator category.
    Operator mOp;
    /// Left hand side of the operator.
    exp mLhs;
    /// Right hand side of the operator.
    exp mRhs;
};

using binOp = std::shared_ptr<BinaryOperator>;

template <typename... Args>
inline binOp makeBinOp(Args &&...args) {
    return std::make_shared<BinaryOperator>(std::forward<Args>(args)...);
}

/**
 * Unary operator language construct (any operator with one argument).
 */
class UnaryOperator : public Expression {
    public:
    /**
     * Value constructor. Sets all members.
     * @param op The operator category.
     * @param lhs Left hand side of the operator.
     * @param rhs Right hand side of the operator.
     */
    UnaryOperator(Operator op, exp expr, std::optional<Lexeme> l = std::nullopt)
        : Expression(l), mOp(op), mExpr(expr) {
    }

    /// Defaulted destructor.
    virtual ~UnaryOperator() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// Operator category.
    Operator mOp;
    /// Left hand side of the operator.
    exp mExpr;
};

using unOp = std::shared_ptr<UnaryOperator>;

template <typename... Args>
inline unOp makeUnOp(Args &&...args) {
    return std::make_shared<UnaryOperator>(std::forward<Args>(args)...);
}

/**
 * Dummy class for constant values.
 */
class Constant : public Expression {
    public:
    /// Defaulted default constructor.
    Constant() = default;

    /**
     * Value constructor. Set the lexeme.
     * @param type The type value of the constant.
     * @param lexeme Value to set `lexeme` to.
     */
    Constant(typeInfo type, std::optional<Lexeme> l = std::nullopt) : Expression(type, l) {
    }

    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;
    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    /// Defaulted destructor.
    virtual ~Constant() = default;
};

/**
 * String constant. Null terminated U8i*.
 */
class StringConstant : public Constant {
    public:
    StringConstant(std::string_view str, bool stripQuotes = true,
                   std::optional<Lexeme> l = std::nullopt);

    StringConstant(const Lexeme &l, bool stripQuotes = true)
        : StringConstant(l.getText(), stripQuotes) {
    }

    virtual ~StringConstant() = default;

    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;

    protected:
    std::string_view mStr;
};

using stringConst = std::shared_ptr<StringConstant>;

template <typename... Args>
inline stringConst makeStringConst(Args &&...args) {
    return std::make_shared<StringConstant>(std::forward<Args>(args)...);
}

/**
 * Integer constants (for intrinsics).
 */
class IntegerConstant : public Constant {
    public:
    static IntegerConstant makeI8(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeU8(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeI16(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeU16(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeI32(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeU32(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeI64(std::string_view src, int base, std::optional<Lexeme> l);
    static IntegerConstant makeU64(std::string_view src, int base, std::optional<Lexeme> l);

    /// Defaulted constructor.
    IntegerConstant() = default;
    /**
     * Value constructor. Set all members.
     * @param value Value of the constant.
     * @param type Type of integer. Only intrinsics of size > 0 are valid (or
     * pointer).
     * @param l Lexeme of the integer constant.
     */
    IntegerConstant(std::uint64_t value, typeInfo type, std::optional<Lexeme> l = std::nullopt);

    /**
     * Value constructor. Derive integer constant from source.
     * @param source Lexeme that of the integer constant.
     */
    IntegerConstant(const Lexeme &source) : IntegerConstant(source.getText(), source) {
    }

    /**
     * Value constructor. Derive integer constant from source.
     * @param source Source that of the integer constant.
     * @param l Lexeme of the integer constant.
     */
    IntegerConstant(std::string_view source, std::optional<Lexeme> l = std::nullopt);

    /**
     * Set the signedness of the integer constant.
     * @param isSigned True if constant is signed, false if not.
     */
    inline void setSign(bool isSigned) {
        mIsSigned = isSigned;
    }

    /// Destructor. Default.
    virtual ~IntegerConstant() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;

    inline IntegerConstant &operator==(const IntegerConstant &ic) {
        if (this == &ic) {
            return *this;
        }

        mIsSigned = ic.mIsSigned;
        mLexeme = ic.mLexeme;
        mType = ic.mType;
        mValue = ic.mValue;
        return *this;
    }

    protected:
    /// Value of the constant.
    std::uint64_t mValue;
    /// True if mValue signed, false if not.
    bool mIsSigned;
};

using intConst = std::shared_ptr<IntegerConstant>;

template <typename... Args>
inline intConst makeIntConst(Args &&...args) {
    return std::make_shared<IntegerConstant>(std::forward<Args>(args)...);
}

/**
 * A list of ordered statements.
 */
class CompoundStatement : public Statement {
    public:
    CompoundStatement() = default;
    void add(stmnt statement);
    void add(std::shared_ptr<CompoundStatement> statement);
    virtual ~CompoundStatement() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    bool isEmpty() const {
        return mStatementList.empty();
    }

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    statementList mStatementList;
};

using cmpdStmnt = std::shared_ptr<CompoundStatement>;

template <typename... Args>
inline cmpdStmnt makeCmpdStmnt(Args &&...args) {
    return std::make_shared<CompoundStatement>(std::forward<Args>(args)...);
}

class ElseIf : public Statement {
    public:
    ElseIf(exp conditional, cmpdStmnt body) : mConditional(conditional), mBody(body) {
    }

    virtual ~ElseIf() = default;

    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    inline exp getConditional() const {
        return mConditional;
    }

    inline cmpdStmnt getBody() const {
        return mBody;
    }

    protected:
    /// Else if conditoinal.
    exp mConditional;
    /// Compound statement child.
    cmpdStmnt mBody;
};

using elIf = std::shared_ptr<ElseIf>;

template <typename... Args>
inline elIf makeElIf(Args &&...args) {
    return std::make_shared<ElseIf>(std::forward<Args>(args)...);
}

class If : public Statement {
    public:
    If(exp conditional, cmpdStmnt body, std::list<elIf> elIfs, cmpdStmnt elseBody,
       std::optional<Lexeme> l = std::nullopt)
        : Statement(l),
          mConditional(conditional),
          mBody(body),
          mElseIfs(elIfs),
          mElseBody(elseBody) {
    }

    virtual ~If() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// Boolean expression. TODO implicit comparison to 0.
    exp mConditional;
    /// Body of the if statement.
    cmpdStmnt mBody;
    /// Else ifs.
    std::list<elIf> mElseIfs;
    /// Body of the optional "else" statement.
    cmpdStmnt mElseBody;
};

using ifStmnt = std::shared_ptr<If>;

template <typename... Args>
inline ifStmnt makeIf(Args &&...args) {
    return std::make_shared<If>(std::forward<Args>(args)...);
}

class While : public Statement {
    public:
    While(exp conditional, cmpdStmnt body, bool isDo = false,
          std::optional<Lexeme> l = std::nullopt)
        : Statement(l), mConditional(conditional), mBody(body), mIsDo(isDo) {
    }

    virtual ~While() = default;

    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    exp mConditional;
    cmpdStmnt mBody;
    bool mIsDo;
};

using whileStmnt = std::shared_ptr<While>;

template <typename... Args>
inline whileStmnt makeWhile(Args &&...args) {
    return std::make_shared<While>(std::forward<Args>(args)...);
}

class For : public Statement {
    public:
    For(expList startExpressions, exp conditional, expList endExpressions, cmpdStmnt body,
        std::optional<Lexeme> l = std::nullopt)
        : Statement(l),
          mStartExps(startExpressions),
          mConditional(conditional),
          mEndExps(endExpressions),
          mBody(body) {
    }

    virtual ~For() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    protected:
    expList mStartExps;
    exp mConditional;
    expList mEndExps;
    cmpdStmnt mBody;
};

using forStmnt = std::shared_ptr<For>;

template <typename... Args>
inline forStmnt makeFor(Args &&...args) {
    return std::make_shared<For>(std::forward<Args>(args)...);
}

class Label : public Statement {
    public:
    Label(const Identifier &id, std::optional<Lexeme> l = std::nullopt) : Statement(l), mId(id) {
    }

    virtual ~Label() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc);

    const inline Identifier &getIdRef() const {
        return mId;
    }

    inline Identifier getId() const {
        return mId;
    }

    protected:
    Identifier mId;
};

using label = std::shared_ptr<Label>;

template <typename... Args>
inline label makeLabel(Args &&...args) {
    return std::make_shared<Label>(std::forward<Args>(args)...);
}

class Goto : public Statement {
    public:
    Goto(label label, std::optional<Lexeme> l = std::nullopt) : Statement(l), mLabel(label) {
    }

    virtual ~Goto() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    protected:
    label mLabel;
};

using gotoStmnt = std::shared_ptr<Goto>;

template <typename... Args>
inline gotoStmnt makeGoto(Args &&...args) {
    return std::make_shared<Goto>(std::forward<Args>(args)...);
}

/**
 * Dummy class for Declaration productions.
 */
class Declaration : public Statement {
    public:
    /// Defaulted constructor.
    Declaration() : mId(""), mType(), mStorageClass() {
    }

    /**
     * Value constructor. Set the lexeme.
     */
    Declaration(const Lexeme &l) : Statement(l), mId(l), mType(), mStorageClass() {
    }

    /**
     * Value constructor. Set the lexeme.
     */
    Declaration(const Identifier &id, typeInfo type, StorageClass sclass = StorageClass::Default,
                std::optional<Lexeme> l = std::nullopt)
        : Statement(l), mId(id), mType(type), mStorageClass(sclass) {
    }

    /// Destructor. Default.
    virtual ~Declaration() = default;

    const inline Identifier &getIdRef() const {
        return mId;
    }

    inline typeInfo getType() const {
        return mType;
    }

    inline StorageClass getStorageClass() const {
        return mStorageClass;
    }

    virtual void parseSemantics(semanticContext &sc);

    enum class Type {
        Variable,
        Function,
    };

    virtual Type getDeclType() const = 0;

    protected:
    /// Identifier of the declared variable.
    Identifier mId;
    /// Type of thing being declared.
    typeInfo mType;
    /// Storage class of thing being declared.
    StorageClass mStorageClass;
};

/**
 * Alias to a shared pointer of `Declaration`.
 * @see Declaration
 */
using decl = std::shared_ptr<Declaration>;

/**
 * VariableDeclaration production rule.
 */
class VariableDeclaration : public Declaration {
    public:
    /**
     * Value constructor. Set the ID of the declared variable.
     */
    VariableDeclaration(const Identifier &id, typeInfo type,
                        StorageClass sclass = StorageClass::Default,
                        std::optional<Lexeme> l = std::nullopt)
        : Declaration(id, type, sclass, l) {
    }

    /// Destructor. Default.
    virtual ~VariableDeclaration() = default;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;
    virtual void parseSemantics(semanticContext &sc);

    virtual Type getDeclType() const;
};

/**
 * Alias for shared pointer to VariableDeclaration.
 * @see VariableDeclaration.
 */
using varDecl = std::shared_ptr<VariableDeclaration>;

template <typename... Args>
inline varDecl makeVarDecl(Args &&...args) {
    return std::make_shared<VariableDeclaration>(std::forward<Args>(args)...);
}

class FunctionDefinition : public Statement {
    public:
    FunctionDefinition(typeInfo type, cmpdStmnt body, std::optional<Lexeme> l = std::nullopt)
        : Statement(l), mBody(body), mType(type) {
    }

    virtual ~FunctionDefinition() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    inline typeInfo getType() const {
        return mType;
    }

    protected:
    cmpdStmnt mBody;
    typeInfo mType;
};

using funcDefn = std::shared_ptr<FunctionDefinition>;

template <typename... Args>
inline funcDefn makeFuncDefn(Args &&...args) {
    return std::make_shared<FunctionDefinition>(std::forward<Args>(args)...);
}

class FunctionDeclaration : public Declaration {
    public:
    FunctionDeclaration(typeInfo type, const Identifier &id, funcDefn defn, std::list<varDecl> args,
                        StorageClass sclass, std::optional<Lexeme> l = std::nullopt)
        : Declaration(id, type, sclass, l), mDefinition(defn), mArgs(args) {
    }

    FunctionDeclaration(typeInfo type, const Identifier &id, std::list<varDecl> args,
                        StorageClass sclass, std::optional<Lexeme> l = std::nullopt)
        : Declaration(id, type, sclass, l), mDefinition(nullptr), mArgs(args) {
    }

    virtual ~FunctionDeclaration() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;
    virtual void parseSemantics(semanticContext &sc);

    virtual Type getDeclType() const;

    inline bool hasDefinition() const {
        return static_cast<bool>(mDefinition);
    }

    inline const std::list<varDecl> &getArgsRef() const {
        return mArgs;
    }

    protected:
    funcDefn mDefinition;
    std::list<varDecl> mArgs;
};

using funcDecl = std::shared_ptr<FunctionDeclaration>;

template <typename... Args>
inline funcDecl makeFuncDecl(Args &&...args) {
    return std::make_shared<FunctionDeclaration>(std::forward<Args>(args)...);
}

class Return : public Statement {
    public:
    Return() = default;

    Return(exp expr, std::optional<Lexeme> l = std::nullopt) : Statement(l), mExp(expr) {
    }

    virtual ~Return() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    protected:
    exp mExp;
};

using ret = std::shared_ptr<Return>;

template <typename... Args>
inline ret makeRet(Args &&...args) {
    return std::make_shared<Return>(std::forward<Args>(args)...);
}

/**
 * Variable declaration and initialization. Different from `Assignment`
 * in that initialization is not an operator.
 */
class VariableInitialization : public VariableDeclaration {
    public:
    /**
     * Value constructor. Set all members.
     * @param id Identifier of the declared variable.
     * @param expr Expression to set the declared variable's value to.
     */
    VariableInitialization(const Identifier &id, typeInfo type, exp expr);
    /// Destructor. Default.
    virtual ~VariableInitialization() = default;

    virtual void setLexeme(std::optional<Lexeme> l);
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// RHS.
    exp mRhs;
};

/**
 * Alias to shared pointer to a `VariableInitialization`.
 * @see VariableInitialization.
 */
using varInit = std::shared_ptr<VariableInitialization>;

template <typename... Args>
inline varInit makeVarInit(Args &&...args) {
    return std::make_shared<VariableInitialization>(std::forward<Args>(args)...);
}

class FunctionCall : public Expression {
    public:
    FunctionCall(funcDecl func, expList argExps, std::optional<Lexeme> l = std::nullopt)
        : Expression(l), mFunc(func), mArgExps(argExps) {
    }

    virtual ~FunctionCall() = default;

    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;
    virtual void parseSemantics(semanticContext &sc);

    protected:
    funcDecl mFunc;
    expList mArgExps;
};

using funcCall = std::shared_ptr<FunctionCall>;

template <typename... Args>
inline funcCall makeFuncCall(Args &&...args) {
    return std::make_shared<FunctionCall>(std::forward<Args>(args)...);
}

class DeclarationReference : public Expression {
    public:
    enum class Type {
        LValue,
        Function,
    };

    DeclarationReference(const Identifier &id, Type type, const SymbolTable<decl> &table,
                         std::optional<Lexeme> l = std::nullopt);
    virtual ~DeclarationReference() = default;
    /**
     * Generate LLVM byteco<decl>de.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    inline std::string_view stringifyType() const {
        return stringifyType(mDeclType);
    }

    inline std::string_view stringifyType(Type t) const {
        switch (t) {
        case Type::LValue:
            return "lvalue";
            break;
        case Type::Function:
            return "function";
            break;
        default:
            break;
        }
        return "?";
    }

    virtual void parseSemantics(semanticContext &sc);

    inline const Identifier &getIdRef() const {
        return mDeclRef->getIdRef();
    }

    inline decl getDeclRef() const {
        return mDeclRef;
    }

    virtual bool isLValue() const;

    private:
    Type mDeclType;
    decl mDeclRef;
};

using declRef = std::shared_ptr<DeclarationReference>;

template <typename... Args>
inline declRef makeDeclRef(Args &&...args) {
    return std::make_shared<DeclarationReference>(std::forward<Args>(args)...);
}

class BinaryAssignment : public BinaryOperator {
    public:
    BinaryAssignment(Operator op, declRef lhs, exp rhs, std::optional<Lexeme> l = std::nullopt)
        : BinaryOperator(op, lhs, rhs, l), mLhs(lhs) {
    }

    virtual ~BinaryAssignment() = default;
    /**
     * Generate LLVM byteco<decl>de.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    virtual void parseSemantics(semanticContext &sc);
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const;

    protected:
    declRef mLhs;
};

using binAsgn = std::shared_ptr<BinaryAssignment>;

template <typename... Args>
inline binAsgn makeBinAsgn(Args &&...args) {
    return std::make_shared<BinaryAssignment>(std::forward<Args>(args)...);
}

class UnaryAssignment : public UnaryOperator {
    public:
    UnaryAssignment(Operator op, declRef expr, std::optional<Lexeme> l = std::nullopt)
        : UnaryOperator(op, expr, l), mExpr(expr) {
    }

    virtual ~UnaryAssignment() = default;
    /**
     * Generate LLVM byteco<decl>de.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const;

    protected:
    declRef mExpr;
};

using unAsgn = std::shared_ptr<UnaryOperator>;

template <typename... Args>
inline unAsgn makeUnAsgn(Args &&...args) {
    return std::make_shared<UnaryAssignment>(std::forward<Args>(args)...);
}

class LToRValue : public Expression {
    public:
    LToRValue(declRef declRef) : Expression(declRef->getType()), mDeclRef(declRef) {
    }

    virtual ~LToRValue() = default;

    /**
     * Generate LLVM byteco<decl>de.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    protected:
    declRef mDeclRef;
};

using l2rval = std::shared_ptr<LToRValue>;

template <typename... Args>
inline l2rval makeL2Rval(Args &&...args) {
    return std::make_shared<LToRValue>(std::forward<Args>(args)...);
}

/**
 * Declaration statement production rule.
 */
class DeclarationStatement : public Statement {
    public:
    /// Defaulted constructor.
    DeclarationStatement() = default;
    /// Destructor. Default.
    virtual ~DeclarationStatement() = default;

    /**
     * Value constructor. Set the first declaration.
     * @param dec The first declaration of the declaration statement rule.
     */
    DeclarationStatement(decl dec) : Statement(dec->getLexemeConst()), mDecls({dec}) {
    }

    /**
     * Add a variable declaration object.
     */
    inline void push(decl d) {
        mDecls.push_back(d);
    }

    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    inline std::optional<typeInfo> getType() const {
        if (mDecls.empty()) {
            return std::nullopt;
        }
        return mDecls.front()->getType();
    }

    inline std::optional<Declaration::Type> getDeclType() const {
        if (mDecls.empty()) {
            return std::nullopt;
        }
        return mDecls.front()->getDeclType();
    }

    inline std::optional<StorageClass> getStorageClass() const {
        if (mDecls.empty()) {
            return std::nullopt;
        }
        return mDecls.front()->getStorageClass();
    }

    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// List of declarations that make this statement.
    std::list<decl> mDecls;
};

/**
 * Alias to shared pointer to `DeclarationStatement`.
 * @see DeclarationStatement
 */
using declStmnt = std::shared_ptr<DeclarationStatement>;

/**
 * Alias to a shared pointer of a `GrammerRule`.
 * @see GrammarRule
 */
using programData = std::variant<cmpdStmnt, funcDefn>;

inline GR getPD(const programData &pd) {
    if (std::holds_alternative<cmpdStmnt>(pd)) {
        return std::get<cmpdStmnt>(pd);
    }
    return std::get<funcDefn>(pd);
}

/**
 * Represents the whole file, or compilation unit.
 */
class Program : public GrammarRule {
    public:
    /// Defaulted constructor.
    Program() = default;
    /**
     * Add statement to program.
     * @param pd Statement to add.
     */
    void add(cmpdStmnt pd);
    /// Destructor. Default.
    virtual ~Program() = default;
    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;
    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const;
    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const;
    /**
     * Get all production rule members.
     * @return All production rule members.
     */
    virtual std::list<GR> getChildren() const;

    virtual void parseSemantics(semanticContext &sc);

    protected:
    /// Global level compound statements and function declarations.
    std::list<programData> mStatements;
};

/**
 * Query operator position.
 * @param op Operator to query.
 * @return True if `op` is unary and prefix, false if not.
 */
bool operatorIsPrefix(Operator op);

/**
 * Query operator position.
 * @param op Operator to query.
 * @return True if `op` is unary and postfix, false if not.
 */
bool operatorIsPostfix(Operator op);

/**
 * Query number of arguments an operator takes.
 * @param op Operator to query.
 * @return Number of arguments "op" takes (control characters, like '(',
 * take 0 as they're not compiled to LLVM).
 */
int operatorArgs(Operator op);

/**
 * Get an Operator category as a string.
 * @param op Operator to convert to a string.
 * @return `op` converted to a string.
 */
std::string_view operatorToString(Operator op);

/**
 * Get a HolyC type as a string.
 * @parm type HolyC type to convert to a string.
 * @return `type` converted to a string.
 */
std::string_view typeToString(HCType type);

/**
 * Get the size of a type.
 * @param t1 Type to query size of.
 * @return sizeof(t1).
 */
std::size_t sizeofType(typeInfo t1);

/**
 * Compare the size of t1 and t2. Return true if sizeof(t1) > sizeof(t2).
 * @param t1 Left hand side of comparison.
 * @param t2 Right hand side of comparison.
 * @return True if sizeof(t1) > sizeof(t2).
 */
bool sizeofGt(typeInfo t1, typeInfo t2);

/**
 * Compare the size of t1 and t2. Return true if sizeof(t1) == sizeof(t2).
 * @param t1 Left hand side of comparison.
 * @param t2 Right hand side of comparison.
 * @return True if sizeof(t1) == sizeof(t2).
 */
bool sizeofEq(typeInfo t1, typeInfo t2);

/**
 * Return true if `t1` is an integer type (including pointer).
 * @param t1 Type to query.
 * @return true if `t1` is an integer type (including pointer).
 */
bool isInteger(typeInfo t1);

/**
 * Get the operator precedence of an operator.
 * @param op Operator to get precedence of.
 */
int getPrecedence(Operator op);

template <typename T>
inline GR scastGR(std::shared_ptr<T> p) {
    return std::static_pointer_cast<GrammarRule>(p);
}

template <typename T>
inline exp scastEx(std::shared_ptr<T> p) {
    return std::static_pointer_cast<Expression>(p);
}

std::uint64_t sizeOf(typeInfo ti);

bool isScalar(typeInfo ti);

/**
 * Struct used for semantic (correctness) parsing.
 */
struct semanticContext {
    /// Current function code is being generated into. If NULL, then we are
    /// in global context.
    std::optional<funcDefn> curFunc;
};
}  // namespace hclang

namespace fmt {
template <>
struct fmt::formatter<hclang::Operator> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(hclang::Operator op, FormatContext &ctx) {
        return fmt::format_to(ctx.out(), "{}", hclang::operatorToString(op));
    }
};
MAKE_FMT_STYLE_SPEC(hclang::Operator)
}  // namespace fmt

namespace fmt {
template <>
struct fmt::formatter<hclang::typeInfo> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &ctx) {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const hclang::typeInfo &info, FormatContext &ctx) {
        using hct = hclang::HCType;
        switch (info.type) {
        case hct::Union:
            return fmt::format_to(ctx.out(), "Union.{}", info.id);
            break;
        case hct::Class:
            return fmt::format_to(ctx.out(), "class.{}", info.id);
            break;
        case hct::Enum:
            return fmt::format_to(ctx.out(), "Enum.{}", info.id);
            break;
        case hct::Pointer:
            if (info.pointer) {
                return fmt::format_to(ctx.out(), "*{}", *info.pointer);
            }
            return fmt::format_to(ctx.out(), "*nullptr");
            break;
        default:
            break;
        }
        return fmt::format_to(ctx.out(), "{}", hclang::typeToString(info.type));
    }
};
MAKE_FMT_STYLE_SPEC(hclang::typeInfo)
}  // namespace fmt

#endif /* SLANG_AST_HPP */
