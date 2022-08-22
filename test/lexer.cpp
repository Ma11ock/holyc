

#include "../src/lexer/lexer.hpp"

#include <fmt/core.h>
#include <gtest/gtest-printers.h>
#include <gtest/gtest.h>

#include <iostream>

#include "../src/config.hpp"

static hclang::Config testConfig({"tstfile"});

namespace hclang {
std::ostream &operator<<(std::ostream &stream, const hclang::Lexeme &l) {
    return stream << fmt::format("Text: {}\nType: {} @{}", l.getText(), l.getTokenType(), l);
}

}  // namespace hclang

using namespace std::string_view_literals;

TEST(lexer, single_line_comment) {
    hclang::Lexer lexer("// 1+1=2"sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}

TEST(lexer, multiline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}

TEST(lexer, multiline_comment_embedded_singleline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n// A single line comment. \n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}

TEST(lexer, multiline_comment_embedded_multiline_comment) {
    hclang::Lexer lexer("/* 1+1=2\n\n /* Another multiline comment */ \n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}

TEST(lexer, single_line_commented_out_code) {
    hclang::Lexer lexer("// U64i testvar = 20 << 3; "sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}

TEST(lexer, multiline_commented_out_code) {
    hclang::Lexer lexer("/*\nU64i testvar = 20 << 3;\n*/"sv, testConfig);
    ASSERT_EQ(lexer.pull(), hclang::Lexeme(hclang::TokenType::Eof));
}
