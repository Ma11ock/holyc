/***************************************************************************
 * @file ast.hpp                                                           *
 * @brief Abstract syntax tree objects (interface).                        *
 ***************************************************************************/
#ifndef SLANG_AST_HPP
#define SLANG_AST_HPP

#include <memory>
#include <string_view>
#include <stdexcept>
#include <fmt/core.h>
#include <charconv>
#include <cstdint>
#include <list>
#include <unordered_map>
#include <variant>

#include "type.hpp"
#include "../hclang.hpp"
#include "../lexer/token.hpp"
#include "../util.hpp"

namespace llvm {
    class Value;
}

namespace hclang {
    /**
     * Alias for llvm::value*.
     */
    using LLV = llvm::Value*;
    /**
     * All valid operators in HolyC.
     */
    enum class Operator {
        /// ! (unary).
        LogicalNot,
        /// - (unary).
        Negative,
        /// + (unary).
        Postivie,
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
        Minus,
        /// Multiply (binary).
        Multiply,
        /// Divide (binary).
        Divide,
        /// % (binary).
        Modulo,
        /// << (binary).
        Lshift,
        /// >> (binary).
        Rshift,
        /// & (binary).
        BitwiseAnd,
        /// | (binary).
        BitwiseOr,
        //// ^ (binary).
        BitwiseXor,
        /// Lparen (control).
        Lparen,
        /// Rparen (control).
        Rparen,
    };

    /**
     * HolyC type categories, including intrinsic types, classes, enums,
     * and structs.
     */
    enum class HCType {
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
        Identifier id;
        /// Type that is pointed to. Only used if `type` is pointer.
        std::shared_ptr<typeInfo> pointer;
        /// HolyC type category.
        HCType type;
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
        StorageClass() : value(StorageClass::Default) {}
        /**
         * Value constructor. Set `value` to `v`.
         * @param v Value to set `value` to.
         */
        StorageClass(std::uint32_t v) : value(v) {}
        /**
         * Copy constructor.
         * @param sc Value to set `value` to.
         */
        StorageClass(const StorageClass &sc) : value(sc.value) {}

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
        GrammarRule(const hclang::Lexeme &lexeme) : mLexeme(lexeme) {}
        /// Defaulted constructor.
        GrammarRule() = default;
        /// Destructor. Default.
        virtual ~GrammarRule() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const = 0;
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
    protected:
        /// Lexeme that began this production rule.
        hclang::Lexeme mLexeme;

        /// Default pretty printing function.
        void printDefault() const;
    };

    /**
     * Alias to a shared pointer of a `GrammerRule`.
     * @see GrammarRule
     */
    using GR = std::shared_ptr<GrammarRule>;
    /**
     * Alias to a shared pointer of a `GrammerRule`.
     * @see GrammarRule
     */
    using programData = std::shared_ptr<GrammarRule>;

    /**
     * Dummy class for HolyC expressions (language constructs that return something).
     */
    class Expression : public GrammarRule {
    public:
        /// Default constructor. Defaulted.
        Expression() = default;
        /**
         * Value constructor. Sets the beginning lexeme.
         * @param l Lexeme that began this grammar production.
         */
        Expression(const hclang::Lexeme &l) : GrammarRule(l) {}
        /// Default destructor. Defaulted.
        virtual ~Expression() = default;
    };

    /**
     * Alias to a shared pointer of a `GrammerRule`.
     * @see Expression
     */
    using exp = std::shared_ptr<Expression>;

    /**
     * Cast one intrinsic into another.
     */
    class Cast : public Expression {
    public:
        Cast(exp expr, typeInfo into, const Lexeme l = Lexeme());
        /// Defaulted destructor.
        virtual ~Cast() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    protected:
        /// Type to cast `mExpr` into.
        typeInfo mIntoType;
        /// Expression to cast.
        exp mExpr;
    };

    /**
     * (Implicitly) cast one intrinsic into another.
     */
    class ImplicitCast : public Cast {
    public:
        ImplicitCast(exp expr, typeInfo into) : Cast(expr, into) {}
        /// Defaulted destructor.
        virtual ~ImplicitCast() = default;
        /**
         * Get class name.
         * @return Class name.
         */
        virtual std::string_view getClassName() const;
    };


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
        BinaryOperator(Operator op, exp lhs, exp rhs)
            : mOp(op),mLhs(lhs),mRhs(rhs) {}
        /// Defaulted destructor.
        virtual ~BinaryOperator() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    private:
        /// Operator category.
        Operator mOp;
        /// Left hand side of the operator.
        exp mLhs;
        /// Right hand side of the operator.
        exp mRhs;
    };

    /**
     * Dummy class that represents all statement productions (assignment, if, while, etc.).
     */
    class Statement : public GrammarRule {
    public:
        /// Defaulted constructor.
        Statement() = default;
        /**
         * Value constructor. Set the lexeme.
         * @param lexeme Value to set `lexeme` to.
         */
        Statement(const Lexeme &l) : GrammarRule(l) {}
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
     * Assignment statement construct.
     * NOT used for declaration initialization, as that is not an operator.
     * The identifier must already be defined.
     */
    class Assign : public Statement,Expression {
    public:
        /**
         * Value constructor. Set all children.
         * @param id Left hand side, variable identifier.
         * @param expr Right hand side. Expression to set `id`'s value to.
         */
        Assign(const Identifier &id, exp expr) : mLhs(id),mRhs(expr) {}
        /// Defaulted destructor.
        virtual ~Assign() = default;
        /**
         * Get all production rule members.
         * @return All production rule members.
         */
        virtual std::list<programData> getChildren() const;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
        /// Pretty print this grammar rule (does not print children).
        virtual void pprint() const;
        /**
         * Get class name.
         * @return Class name.
         */
        virtual std::string_view getClassName() const;
    protected:
        /// Left hand side of assignment, variable identifier.
        Identifier mLhs;
        /// Right hand side of assignment, value to assign.
        exp mRhs;
    };

    /**
     * Dummy class for constant values.
     */
    class Constant : public Expression {
    public:
        /// Defaulted default constructor.
        Constant() = default;
        /**
         * Value constructor. Set the lexeme.
         * @param lexeme Value to set `lexeme` to.
         */
        Constant(const Lexeme &l) : Expression(l) {}
        /// Defaulted destructor.
        virtual ~Constant() = default;
    };

    /**
     * Integer constants (for intrinsics).
     */
    class IntegerConstant : public Constant {
    public:
        /// Defaulted constructor.
        IntegerConstant() = default;
        /**
         * Value constructor. Set all members.
         * @param value Value of the constant.
         * @param type Type of integer. Only intrinsics of size > 0 are valid (or pointer).
         * @param isSigned True if constant is signed, false if unsigned.
         * @param l Lexeme of the integer constant.
         */
        IntegerConstant(std::uint64_t value, HCType type, bool isSigned = false,
                        const Lexeme &l = Lexeme());
        /**
         * Value constructor. Derive integer constant from source.
         * @param source Lexeme that of the integer constant.
         * @param isSigned True if constant is signed, false if unsigned.
         */
        IntegerConstant(const Lexeme &source, bool isSigned = false) :
            IntegerConstant(source.getText(), isSigned) {}
        /**
         * Value constructor. Derive integer constant from source.
         * @param source Source that of the integer constant.
         * @param isSigned True if constant is signed, false if unsigned.
         */
        IntegerConstant(std::string_view source, bool isSigned = false);
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
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    protected:
        /// Value of the constant.
        std::uint64_t mValue;
        /// Type of integer (should only intrinsic of size > 0 or pointer).
        HCType mType;
        /// True if mValue signed, false if not.
        bool mIsSigned;
    };

    /**
     * A list of ordered statements.
     */
    class CompoundStatement : public Statement {
    public:
        CompoundStatement() = default;
        void add(stmnt statement);
        virtual ~CompoundStatement() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    protected:
        statementList mStatementList;
    };

    using cmpdStmnt = std::shared_ptr<CompoundStatement>;

    class FunctionDefinition : public GrammarRule {
    public:
        FunctionDefinition() = default;
        virtual ~FunctionDefinition() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    };

    /**
     * Dummy class for Declaration productions.
     */
    class Declaration : public GrammarRule {
    public:
        /// Defaulted constructor.
        Declaration() = default;
        /// Destructor. Default.
        virtual ~Declaration() = default;
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
        VariableDeclaration(const Identifier &id, typeInfo type)
            : mId(id),mType(type) {}
        /**
         * Value constructor. Set the lexeme.
         * @param lexeme Value to set `lexeme` to.
         */
        VariableDeclaration(const Lexeme &lexeme, typeInfo type)
            : mId(lexeme),mType(type) {}
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
        virtual LLV toLLVM() const;
        /// Pretty print this grammar rule (does not print children).
        virtual void pprint() const;
        /**
         * Get all production rule members.
         * @return All production rule members.
         */
        virtual std::list<programData> getChildren() const;
    protected:
        /// Identifier of the declared variable.
        Identifier mId;
        /// Type information.
        typeInfo mType;
    };

    /**
     * Alias for shared pointer to VariableDeclaration.
     * @see VariableDeclaration.
     */
    using varDecl = std::shared_ptr<VariableDeclaration>;

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
        /**
         * Get class name.
         * @return Class name.
         */
        virtual std::string_view getClassName() const;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
        /// Pretty print this grammar rule (does not print children).
        virtual void pprint() const;
        /**
         * Get all production rule members.
         * @return All production rule members.
         */
        virtual std::list<programData> getChildren() const;
    protected:
        /// RHS.
        exp mRhs;
    };

    /**
     * Alias to shared ponter to a `VariableInitialization`.
     * @see VariableInitialization.
     */
    using varInit = std::shared_ptr<VariableInitialization>;

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
         * Add a variable declaration object.
         */
        void push(varDecl decl);
        /**
         * Get class name.
         * @return Class name.
         */
        virtual std::string_view getClassName() const;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
        /// Pretty print this grammar rule (does not print children).
        virtual void pprint() const;
        /**
         * Get all production rule members.
         * @return All production rule members.
         */
        virtual std::list<programData> getChildren() const;
    protected:
        /// List of variable declarations that make this statement.
        std::list<varDecl> mDecls;
    };

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
        void add(programData pd);
        /// Destructor. Default.
        virtual ~Program() = default;
        /**
         * Generate LLVM bytecode.
         * @return LLVM object representing this production rule.
         */
        virtual LLV toLLVM() const;
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
        virtual std::list<programData> getChildren() const;
    protected:
        /// Global level compound statements and function declarations.
        std::list<programData> mStatements;
    };

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
}

#endif /* SLANG_AST_HPP */
