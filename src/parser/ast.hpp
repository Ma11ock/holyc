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
#include "../slang.hpp"
#include "../lexer/token.hpp"

namespace llvm {
    class Value;
}

namespace slang {
    using LLV = llvm::Value*;

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
        // Need identifiers.
        Class,
        Enum,
        Union,
    };

    struct typeInfo {
        Identifier id;
        HCType type;
    };

    struct StorageClass {
        inline static const std::uint32_t Default = 0;
        inline static const std::uint32_t Reg = 1;
        inline static const std::uint32_t Noreg = 2;
        inline static const std::uint32_t Public = 4;
        inline static const std::uint32_t Static = 8;
        inline static const std::uint32_t Extern = 16;
        inline static const std::uint32_t _Extern = 32;

        std::uint32_t mValue;

        StorageClass() : mValue(0) {}
        StorageClass(std::uint32_t v) : mValue(v) {}
        StorageClass(const StorageClass &sc) : mValue(sc.mValue) {}

        inline StorageClass operator|(std::uint32_t v) const {
            return mValue | v;
        }
        inline StorageClass operator|(StorageClass sc) const {
            return *this | sc.mValue;
        }
        inline StorageClass operator|=(std::uint32_t v) {
            return mValue |= v;
        }
        inline StorageClass operator|=(StorageClass sc) {
            return *this |= sc.mValue;
        }

        inline StorageClass operator&(std::uint32_t v) const {
            return mValue & v;
        }
        inline StorageClass operator&(StorageClass sc) const {
            return mValue & sc.mValue;
        }
        inline StorageClass operator&=(std::uint32_t v) {
            return mValue &= v;
        }
        inline StorageClass operator&=(StorageClass sc) {
            return mValue &= sc.mValue;
        }

        inline StorageClass operator^(std::uint32_t v) const {
            return mValue ^ v;
        }
        inline StorageClass operator^(StorageClass sc) const {
            return mValue ^ sc.mValue;
        }
        inline StorageClass operator^=(std::uint32_t v) {
            return mValue ^= v;
        }
        inline StorageClass operator^=(StorageClass sc) {
            return mValue ^= sc.mValue;
        }

        inline StorageClass operator~() const {
            return ~mValue;
        }

        inline StorageClass operator=(StorageClass sc) {
            mValue = sc.mValue;
            return *this;
        }
        inline StorageClass operator=(std::uint32_t v) {
            mValue = v;
            return *this;
        }

        inline bool operator==(StorageClass sc) const {
            return mValue == sc.mValue;
        }
        inline bool operator!=(StorageClass sc) const {
            return mValue != sc.mValue;
        }

        inline bool isReg() const {
            return (mValue & Reg) != 0;
        }
        inline bool isNoreg() const {
            return (mValue & Noreg) != 0;
        }
        inline bool isPublic() const {
            return (mValue & Public) != 0;
        }
        inline bool isStatic() const {
            return (mValue & Static) != 0;
        }
        inline bool isExtern() const {
            return (mValue & Extern) != 0;
        }
        inline bool is_Extern() const {
            return (mValue & _Extern) != 0;
        }
    };

    class GrammarRule {
    public:
        GrammarRule(const slang::Lexeme &lexeme) : mLexeme(lexeme) {}
        GrammarRule() = default;
        virtual ~GrammarRule() = default;
        virtual LLV toLLVM() const = 0;
        virtual void pprint() const;
        virtual std::string_view getClassName() const = 0;
        virtual std::list<std::shared_ptr<GrammarRule>> getChildren() const = 0;
    protected:
        slang::Lexeme mLexeme;

        void printDefault() const;
    };

    using GR = std::shared_ptr<GrammarRule>;
    using programData = std::shared_ptr<GrammarRule>;

    class Expression : public GrammarRule {
    public:
        Expression() = default;
        virtual ~Expression() = default;
    protected:
    };

    using exp = std::shared_ptr<Expression>;

    class BinaryOperator : public Expression {
    public:
        BinaryOperator(Operator op, exp lhs, exp rhs)
            : mOp(op),mLhs(lhs),mRhs(rhs) {}
        virtual ~BinaryOperator() = default;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
        virtual std::list<programData> getChildren() const;
    private:
        Operator mOp;
        exp mLhs;
        exp mRhs;
    };

    class Statement : public Expression {
    public:
    protected:
    };

    using stmnt = std::shared_ptr<Statement>;
    using statementList = std::list<stmnt>;

    class Assign : public Statement {
    public:
        Assign(const Identifier &id, exp expr) : mLhs(id),mRhs(expr) {}
        virtual ~Assign() = default;

        virtual std::list<programData> getChildren() const;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
    protected:
        Identifier mLhs;
        exp mRhs;
    };

    class Constant : public Statement {
    public:
        Constant() = default;
        virtual ~Constant() = default;
    };

    class IntegerConstant : public Constant {
    public:
        IntegerConstant() = default;
        IntegerConstant(std::uint64_t value, bool isSigned = false) :
            mValue(value),mIsSigned(isSigned) {}
        IntegerConstant(const Lexeme &source) : IntegerConstant(source.getText()) {}
        IntegerConstant(std::string_view source);
        virtual ~IntegerConstant() = default;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
        virtual std::list<programData> getChildren() const;
    protected:
        std::uint64_t mValue;
        bool mIsSigned;
    };

    class CompoundStatement : public Statement {
    public:
        CompoundStatement() = default;
        void add(stmnt statement);
        virtual ~CompoundStatement() = default;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
        virtual std::list<programData> getChildren() const;
    protected:
        statementList mStatementList;
    };

    using cmpdStmnt = std::shared_ptr<CompoundStatement>;

    class FunctionDefinition : public GrammarRule {
    public:
        FunctionDefinition() = default;
        virtual ~FunctionDefinition() = default;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
        virtual std::list<programData> getChildren() const;
    };

    class Declaration : public GrammarRule {
    public:
        Declaration() = default;
        virtual ~Declaration() = default;
    };

    using decl = std::shared_ptr<Declaration>;

    class VariableDeclaration : public Declaration {
    public:
        VariableDeclaration(const Identifier &id) : mId(id) {}
        VariableDeclaration(const Lexeme &lexeme) : mId(lexeme) {}
        virtual ~VariableDeclaration() = default;
        virtual std::string_view getClassName() const;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::list<programData> getChildren() const;
    protected:
        Identifier mId;
        // TODO type.
    };

    using varDecl = std::shared_ptr<VariableDeclaration>;

    class VariableInitialization : public VariableDeclaration {
    public:
        VariableInitialization(const Identifier &id, exp expr)
            : VariableDeclaration(id),mRhs(expr) {}
        virtual ~VariableInitialization() = default;
        virtual std::string_view getClassName() const;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::list<programData> getChildren() const;
    protected:
        /// RHS.
        exp mRhs;
    };

    using varInit = std::shared_ptr<VariableInitialization>;

    class DeclarationStatement : public Statement {
    public:
        DeclarationStatement() = default;
        virtual ~DeclarationStatement() = default;

        void push(varDecl decl);
        virtual std::string_view getClassName() const;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::list<programData> getChildren() const;
    protected:
        std::list<varDecl> mDecls;
    };

    class Program : public GrammarRule {
    public:
        Program() = default;
        void add(programData pd);
        virtual ~Program() = default;
        virtual LLV toLLVM() const;
        virtual void pprint() const;
        virtual std::string_view getClassName() const;
        virtual std::list<programData> getChildren() const;
    protected:
        std::list<programData> mStatements;
    };

    // Function declarations.
    std::string_view operatorToLexeme(Operator op);
    std::string_view stringifyOperator(Operator op);
}

#endif /* SLANG_AST_HPP */
