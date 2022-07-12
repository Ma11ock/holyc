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

    class GrammarRule {
    public:
        GrammarRule(const slang::Lexeme &lexeme) : mLexeme(lexeme) {}
        GrammarRule() = default;
        virtual ~GrammarRule() = default;
        virtual LLV toLLVM() const = 0;
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const = 0;
    protected:
        slang::Lexeme mLexeme;
    };

    using GR = std::shared_ptr<GrammarRule>;

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
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const;
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

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
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
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const;
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
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const;
    protected:
        statementList mStatementList;
    };

    using cmpdStmnt = std::shared_ptr<CompoundStatement>;

    class FunctionDefinition : public GrammarRule {
    public:
        FunctionDefinition() = default;
        virtual ~FunctionDefinition() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const;
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
        virtual std::string stringify() const;
    protected:
        Identifier mId;
        // TODO type.
    };

    using varDecl = std::shared_ptr<VariableDeclaration>;

    class VariableInitialization : public VariableDeclaration {
    public:
        VariableInitialization(const Identifier &id, exp expr) : mLhs(id),mRhs(expr) {}
        virtual ~VariableInitialization() = default;
        virtual std::string_view getClassName() const;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
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
        virtual std::string stringify() const;
    protected:
        std::list<varDecl> mDecls;
    };

    using programData = std::shared_ptr<GrammarRule>;

    class Program : public GrammarRule {
    public:
        Program() = default;
        void add(programData pd);
        virtual ~Program() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        virtual std::string_view getClassName() const;
    protected:
        std::list<programData> mStatements;
    };

    // Function declarations.
    std::string_view operatorToLexeme(Operator op);
    std::string_view stringifyOperator(Operator op);
}

#endif /* SLANG_AST_HPP */
