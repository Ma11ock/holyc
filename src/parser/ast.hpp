#ifndef SLANG_AST_HPP
#define SLANG_AST_HPP

#include <memory>
#include <string_view>
#include <stdexcept>
#include <fmt/core.h>
#include <charconv>
#include <cstdint>
#include <list>

namespace llvm {
    class Value;
}

namespace slang {
    using LLV = llvm::Value*;
    using real = double;
    using integer = std::int64_t;
    using text = std::string;

    class Expression {
    public:
        Expression() = default;
        virtual ~Expression() = default;
        virtual LLV toLLVM() const = 0;
        virtual std::string stringify() const = 0;
    protected:
    };

    using Exp = std::shared_ptr<Expression>;

    class Conditional : public Expression
    {
    public:
        enum class Type {
            None,
            Or,
            And,
        };

        Conditional(Type type, Exp cond1);
        Conditional(Type type, Exp cond1, Exp cond2);
        virtual ~Conditional() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;

        std::string_view stringifyType() const {
            switch(mType) {
            case Type::None:
                return "None";
                break;
            case Type::And:
                return "And";
                break;
            case Type::Or:
                return "Or";
                break;
            default:
                break;
            }
            throw std::invalid_argument("TODO");
        }
    protected:
        /// Type of conditional.
        Type mType;
        /// First conditional.
        Exp mCond1;
        //// Second conditional, is nullptr if Type == None.
        Exp mCond2;
    };


    class Negate : public Expression {
    public:
        Negate(Exp exp) : mExp(exp) {
        }
        virtual ~Negate() = default;
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        Exp mExp;
    };

    class Real : public Expression {
    public:
        Real(std::string_view source);
        Real(real num) : mNum(num) {}
        virtual ~Real() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        real mNum;
    };

    class Integer : public Expression {
    public:
        Integer(std::string_view source);
        Integer(integer num) : mNum(num) {}
        virtual ~Integer() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        integer mNum;
    };

    class Text : public Expression {
    public:
        Text(const text &t) : mText(t) {}
        virtual ~Text() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    protected:
        text mText;
    };

    // TODO array
    // class Array : public Expression {
    // public:
    //     Array(const array &t) : mArray(t) {}
    //     virtual ~Array() = default;

    //     virtual LLV toLLVM() const;
    //     virtual std::string stringify() const;
    // protected:
    //     array mArray;
    // };

    class Identifier : public Expression {
    public:
        Identifier(const std::string &id) : mId(id) {}
        virtual ~Identifier() = default;

        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
        LLV getLoad() const;

    protected:
        std::string mId;
    };

    class BinaryOperator : public Expression {
    protected:
        Exp mLeft;
        Exp mRight;
    public:
        virtual ~BinaryOperator() = default;
        BinaryOperator(Exp left, Exp right)
            : mLeft(left),mRight(right) {
        }

        virtual std::string stringify() const;
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

    class LessThanEqual : public BinaryOperator {
    public:
        virtual ~LessThanEqual() = default;
        LessThanEqual(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class LessThan : public BinaryOperator {
    public:
        virtual ~LessThan() = default;
        LessThan(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class GreaterThanEqual : public BinaryOperator {
    public:
        virtual ~GreaterThanEqual() = default;
        GreaterThanEqual(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class GreaterThan : public BinaryOperator {
    public:
        virtual ~GreaterThan() = default;
        GreaterThan(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };


    class NotEquals : public BinaryOperator {
    public:
        virtual ~NotEquals() = default;
        NotEquals(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    class Equals : public BinaryOperator {
    public:
        virtual ~Equals() = default;
        Equals(Exp left, Exp right)
            : BinaryOperator(left, right) {}
        virtual LLV toLLVM() const;
        virtual std::string stringify() const;
    };

    // Statements.

    class Statement : public Expression {
    public:
        virtual ~Statement() = default;
    };

    using Stmnt = std::shared_ptr<Statement>;
    using StatementList = std::list<Stmnt>;

    class Assign : public Statement {
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
        If(Exp boolExp, const StatementList &statements) :
            mBoolExp(boolExp),mStatements(std::make_shared<Block>(statements)),
            mElifBlocks({}),mElse() {}

        If(Exp boolExp, const StatementList &statements,
           const List &elifBlocks,
           const StatementList &elseStatements) :
            mBoolExp(boolExp),mStatements(std::make_shared<Block>(statements)),
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
    struct formatter<slang::Conditional> {
        template<typename P>
        constexpr auto parse(P &ctx) {
            return ctx.begin();
        }

        template<typename FormatContext>
        auto format(const slang::Conditional &val, FormatContext &ctx) {
            return format_to(ctx.out(), "{}", val.stringify());
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
