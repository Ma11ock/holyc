
#include "test.hpp"

TEST(lexer, single_line_comment) {
    hclang::Lexer lexer("// 1+1=2"sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, multiline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, multiline_comment_embedded_singleline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n// A single line comment. \n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, multiline_comment_embedded_multiline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n /* Another multiline comment */ \n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, single_line_commented_out_code) {
    hclang::Lexer lexer("// U64i testvar = 20 << 3; "sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, multiline_commented_out_code) {
    hclang::Lexer lexer("/*\nU64i testvar = 20 << 3;\n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), TT::Eof);
}

TEST(lexer, mathexpr) {
    hclang::Lexer lexer("2+-3, -59*420;"sv, testConfig);
    for (const auto &t : std::array{TT::IntegerConstant, TT::Plus, TT::Minus, TT::IntegerConstant,
                                    TT::Comma, TT::Minus, TT::IntegerConstant, TT::Star,
                                    TT::IntegerConstant, TT::Semicolon, TT::Eof}) {
        ASSERT_EQ(lexer.pull(), t);
    }
}

TEST(lexer, mathexpr_spaces) {
    hclang::Lexer lexer("2 + -3, -59 * 420;\n"sv, testConfig);
    for (const auto &t : std::array{TT::IntegerConstant, TT::Plus, TT::Minus, TT::IntegerConstant,
                                    TT::Comma, TT::Minus, TT::IntegerConstant, TT::Star,
                                    TT::IntegerConstant, TT::Semicolon, TT::Eof}) {
        ASSERT_EQ(lexer.pull(), t);
    }
}

TEST(lexer, extern_func_decl) {
    hclang::Lexer lexer("extern U0i putchar(I32i v);"sv, testConfig);
    for (const auto &t : std::array{TT::Extern, TT::U0i, TT::Identifier, TT::Lparen, TT::I32i,
                                    TT::Identifier, TT::Rparen, TT::Semicolon, TT::Eof}) {
        ASSERT_EQ(lexer.pull(), t);
    }
}

TEST(lexer, var_decl_init) {
    hclang::Lexer lexer("U64i testVar = 320 << 3;"sv, testConfig);
    for (const auto &t :
         std::array{TT::U64i, TT::Identifier, TT::Equals, TT::IntegerConstant, TT::BitshiftLeft,
                    TT::IntegerConstant, TT::Semicolon, TT::Eof}) {
        ASSERT_EQ(lexer.pull(), t);
    }
}
