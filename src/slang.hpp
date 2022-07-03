#ifndef SLANG_HPP
#define SLANG_HPP

#include <string>

namespace slang {
    const inline std::string VERSION = "0.1";
    const inline std::string NAME = "hclang";
    enum class TokenType {
        // Misc.
        Error,
        Eof,
        // Keywords.
        LCurlyBracket,
        RCurlyBracket,
        If,
        Else,
        ElseIf,
        Class,
        Enum,
        Union,
        While,
        For,
        Boolean,
        Goto,
        Switch,
        Case,
        Extern,
        Import,
        _Extern,
        _Import,
        Try,
        Catch,
        Throw,
        Static,
        // Intrinsic types.
        I8i,
        I16i,
        I32i,
        I64i,
        U0i,
        U8i,
        U16i,
        U32i,
        U64i,
        F64,
        // Operators.
        Plus,
        Minus,
        Star,
        Divide,
        Modulo,
        Identifier,
        Equals,
        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
        Equality,
        Inequality,
        Power,
        LogicalAnd,
        LogicalOr,
        LogicalNot,
        Ampersand,
        BitwiseOr,
        BitwiseXor,
        BitwiseNot,
        BitshiftLeft,
        BitshiftRight,
        QuestionMark,
        Sizeof,
        // Grammar symbols.
        Semicolon,
        Colon,
        Lparen,
        Rparen,
        Dot,
        Lbracket,
        Rbracket,
        Comma,
        DoubleQuote,
        SingleQuote,
        TripleDot,
        // Data.
        CharacterConstant,
        FloatConstant,
        IntegerConstant,
        StringConstant,
        Label,

        // For internal use by the Lexer. These are never sent to the parser.
        HexadecimalConstant,
        OctalConstant,
        Space,
    }; // Token Type.
}  // slang
#endif /* SLANG_HPP */
