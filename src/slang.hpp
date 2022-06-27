#ifndef SLANG_HPP
#define SLANG_HPP

namespace slang {
    enum class TokenType {
        // Misc.
        Space,
        Eof,
        Error,
        // Keywords.
        Begin,
        End,
        If,
        Then,
        Else,
        Procedure,
        Slangation,
        Class,
        Virtual,
        Is,
        Ref,
        New,
        Array,
        Do,
        Step,
        Name,
        Until,
        Activate,
        While,
        For,
        True,
        False,
        Boolean,
        Integer,
        Real,
        Text,
        Goto,
        // Operators.
        Plus,
        Minus,
        Times,
        Divide,
        Identifier,
        Assign,
        Strcat,
        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
        Equality,
        Inequality,
        Power,
        // Grammar symbols.
        Semicolon,
        Colon,
        Lparen,
        Rparen,
        Dot,
        Lbracket,
        Rbracket,
        Comma,
        // Data.
        CharacterConstant,
        RealConstant,
        IntegerConstant,
        StringConstant,
    }; // Token Type.
}  // slang
#endif /* SLANG_HPP */
