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

    class Statement : public Expression {
    public:
    protected:
    };

    using stmnt = std::shared_ptr<Statement>;
    using statementList = std::list<stmnt>;
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
        /// LHS.
        Identifier mId;
        // TODO type.
    };

    using varDecl = std::shared_ptr<VariableDeclaration>;


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
}

#endif /* SLANG_AST_HPP */
