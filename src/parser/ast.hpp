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

#include "type.hpp"
#include "../slang.hpp"

namespace llvm {
    class Value;
}

namespace slang {
    using LLV = llvm::Value*;

    class GrammarRule {
    public:
        GrammarRule() = default;
        virtual ~GrammarRule() = default;
        virtual LLV toLLVM() const = 0;
        virtual std::string stringify() const = 0;
    };

    using GR = std::shared_ptr<GrammarRule>;

    class Expression : public GrammarRule {
    public:
        Expression() = default;
        virtual ~Expression() = default;
    protected:
    };

    using Exp = std::shared_ptr<Expression>;

    class Identifier : public Expression {
    public:
        Identifier(const std::string &id) : mId(id) {}
        Identifier(std::string_view id) : mId(std::string(id)) {}
        virtual ~Identifier() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        LLV getLoad() const;
        inline std::string_view getID() const { return mId; }
        inline std::string getIDCopy() const { return mId; }

        inline bool operator==(const Identifier &other) {
            return mId == other.mId;
        }

        inline bool operator!=(const Identifier &other) {
            return mId != other.mId;
        }

        inline bool operator==(const std::shared_ptr<Identifier> &other) {
            if(other) {
                return mId == other->mId;
            }
            return false;
        }

        inline bool operator!=(const std::shared_ptr<Identifier> &other) {
            if(other) {
                return mId != other->mId;
            }
            return false;
        }

    protected:
        std::string mId;
    };

    class UnaryOperator : public Expression {
    public:
        UnaryOperator(Exp exp = nullptr) : mExp(exp) {}
        virtual ~UnaryOperator() = default;

        virtual std::string stringify() const;

        bool isTmp() const {
            return mExp != nullptr;
        }
    protected:
        Exp mExp;
    };

    class Negate : public UnaryOperator {
    public:
        Negate(Exp exp) : UnaryOperator(exp) {}
        virtual ~Negate() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class UnaryPlus : public UnaryOperator {
    public:
        UnaryPlus(Exp exp) : UnaryOperator(exp) {}
        virtual ~UnaryPlus() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class BinaryOperator : public Expression {
    public:
        BinaryOperator(Exp left, Exp right);
        BinaryOperator() : mLeft(nullptr),mRight(nullptr) {}
        virtual ~BinaryOperator() = default;

        virtual std::string stringify() const;
        virtual std::string_view getBOpName() const = 0;

        bool isTmp() const {
            return mLeft != nullptr && mRight != nullptr;
        }
    protected:
        Exp mLeft;
        Exp mRight;
    };

    class RelationalOperator : public BinaryOperator {
    public:
        RelationalOperator(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual ~RelationalOperator() = default;
    };

    class LogicalAnd : public BinaryOperator {
    public:
        LogicalAnd(Exp left, Exp right) : BinaryOperator(left, right) {}
        virtual ~LogicalAnd() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        virtual std::string_view getName() const;
    };

    class LogicalOr : public BinaryOperator {
    public:
        LogicalOr(Exp left, Exp right) : BinaryOperator(left, right) {}
        virtual ~LogicalOr() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        virtual std::string_view getName() const;
    };

    class Plus : public BinaryOperator {
    public:
        virtual ~Plus() = default;
        Plus(Exp left, Exp right)
            : BinaryOperator(left, right) {}

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class Minus : public BinaryOperator {
    public:
        virtual ~Minus() = default;
        Minus(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class Multiply : public BinaryOperator {
    public:
        virtual ~Multiply() = default;
        Multiply(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class Divide : public BinaryOperator {
    public:
        virtual ~Divide() = default;
        Divide(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class LessThanEqual : public RelationalOperator {
    public:
        virtual ~LessThanEqual() = default;
        LessThanEqual(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class LessThan : public RelationalOperator {
    public:
        virtual ~LessThan() = default;
        LessThan(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class GreaterThanEqual : public RelationalOperator {
    public:
        virtual ~GreaterThanEqual() = default;
        GreaterThanEqual(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class GreaterThan : public RelationalOperator {
    public:
        virtual ~GreaterThan() = default;
        GreaterThan(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };


    class NotEquals : public RelationalOperator {
    public:
        virtual ~NotEquals() = default;
        NotEquals(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class Equals : public RelationalOperator {
    public:
        virtual ~Equals() = default;
        Equals(Exp left, Exp right)
            : RelationalOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class IntegerConstant : public Expression {
    public:
        IntegerConstant(std::uint32_t value) : mValue(value) {}
        IntegerConstant(std::string_view value);
        virtual ~IntegerConstant() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        std::uint32_t mValue;
    };

    // Statements.

    class Statement : public GrammarRule {
    public:
        virtual ~Statement() = default;
        virtual LLV toLLVM() const = 0;
        virtual std::string stringify() const = 0;
    };

    using Stmnt = std::shared_ptr<Statement>;
    using StatementList = std::list<Stmnt>;

    class Assign : public Statement,Expression {
    public:
        Assign(const std::string &name, Exp exp) :
            mId(std::make_shared<Identifier>(name)),mExp(exp) {}
        virtual ~Assign() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        /// Lhs.
        std::shared_ptr<Identifier> mId;
        /// Rhs.
        Exp mExp;
    };

    class Block : public Statement {
    public:
        virtual ~Block() = default;

        Block(const StatementList &statements) : mStatements(statements) {}
        Block() = default;

        StatementList getStatements() const {
            return mStatements;
        }

        bool isEmpty() const {
            return mStatements.empty();
        }

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        StatementList mStatements;
    };

    class While : public Statement {
    public:
        While(Exp boolExp, const StatementList statements) :
            mBoolExp(boolExp),mStatements(std::make_shared<Block>(statements)) {}
        virtual ~While() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        // While boolean expression.
        Exp mBoolExp;
        // Statements.
        std::shared_ptr<Block> mStatements;
    };

    class If : public Statement {
    public:
        using List = std::list<std::shared_ptr<If>>;
        If(Exp boolexp, const StatementList &statements) :
            mBoolExp(boolexp),mStatements(std::make_shared<Block>(statements)),
            mElifBlocks({}),mElse() {}

        If(Exp boolexp, const StatementList &statements,
           const List &elifBlocks,
           const StatementList &elseStatements) :
            mBoolExp(boolexp),mStatements(std::make_shared<Block>(statements)),
            mElifBlocks(elifBlocks),mElse(std::make_shared<Block>(elseStatements)) {}
        virtual ~If() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        // If boolean expression.
        Exp mBoolExp;
        // Statements.
        std::shared_ptr<Block> mStatements;

        // Elif statements.
        List mElifBlocks;
        // Else statement.
        std::shared_ptr<Block> mElse;
    };
}

// Fmt specializations.
namespace fmt {
    template<>
    struct formatter<slang::Exp> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const slang::Exp &exp, FormatContext &ctx) {
            if(exp) {
                return format_to(ctx.out(), "{}", exp->stringify());
            }
            return format_to(ctx.out(), "0x{:x}", static_cast<std::size_t>(0));
        }
    };

    template<>
    struct formatter<std::shared_ptr<slang::Identifier>> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const std::shared_ptr<slang::Identifier> &exp, FormatContext &ctx) {
            if(exp) {
                return format_to(ctx.out(), "{}", exp->stringify());
            }
            return format_to(ctx.out(), "0x{:x}", static_cast<std::size_t>(0));
        }
    };

    template<>
    struct formatter<std::shared_ptr<slang::Statement>> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const std::shared_ptr<slang::Statement> &exp, FormatContext &ctx) {
            if(exp) {
                return format_to(ctx.out(), "{}", exp->stringify());
            }
            return format_to(ctx.out(), "0x{:x}", static_cast<std::size_t>(0));
        }
    };

    template<>
    struct formatter<std::shared_ptr<slang::Block>> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const std::shared_ptr<slang::Block> &exp, FormatContext &ctx) {
            if(exp) {
                return format_to(ctx.out(), "{}", exp->stringify());
            }
            return format_to(ctx.out(), "0x{:x}", static_cast<std::size_t>(0));
        }
    };

    template<>
    struct formatter<std::shared_ptr<slang::If>> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const std::shared_ptr<slang::If> &exp, FormatContext &ctx) {
            if(exp) {
                return format_to(ctx.out(), "{}", exp->stringify());
            }
            return format_to(ctx.out(), "0x{:x}", static_cast<std::size_t>(0));
        }
    };
}

#endif /* SLANG_AST_HPP */
