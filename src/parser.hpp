// MIT License
//
// Simple compiler front-end for LLVM written as a learning exercise.
// Copyright © 2017 Alberto Taiuti
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef BRTO_PARSER_HPP
#define BRTO_PARSER_HPP

#include <memory>
#include <istream>
#include <string>
#include <map>
#include <variant>
#include <utility>
#include <ast_types.hpp>

namespace brt {

enum class TokenType {
  eof,
  // End of production
  semicolon,
  // Commands
  def,
  ext,
  // Primary
  identifier,
  number,

  // Control
  if_stm,
  then,
  else_stm,
  for_loop,
  in,

  l_bracket,
  r_bracket,
  comma,

  // Anything unrecongnised
  generic_ascii,

  max_val,
}; // enum class TokenType

struct Token {
  TokenType type;
  std::variant<double, std::string, char> val;
}; // class Token

/// Utility function template to retrieve a token value given its type
template <typename T>
const T &GetTokenVal(const Token &tok) {
  return std::get<T>(tok.val);
}
/// TODO Do away with TokenType and use visitor to retrieve the value of the
///      token depending on its type

class Lexer {
 public:
  Lexer(std::istream &istream);

  // Rule of 7
  Lexer() = delete;
  Lexer(const Lexer &) = delete;
  Lexer &operator()(const Lexer &) = delete;
  Lexer(Lexer &&) = delete;
  Lexer &operator()(Lexer &&) = delete;
  ~Lexer() = default;

  const Token &GetNextToken();
  const Token &GetCurrToken();
  int GetCurrentTokenPrecedence() const;

  using SP = std::shared_ptr<Lexer>;

 private:
  Token GetNextTokenInternal();

  std::istream istream_;
  char last_char_;
  Token curr_tok_;

}; // class Lexer

class Parser {
 public:
  Parser(Lexer::SP lexer);

  // rule of 7
  Parser() = delete;
  Parser(const Parser &) = delete;
  Parser &operator()(const Parser &) = delete;
  Parser(Parser &&) = delete;
  Parser &operator()(Parser &&) = delete;
  ~Parser() = default;

  UPASTNode ParseTopLevelExpr();
  UPASTNode ParseExtern();
  UPASTNode ParseDefinition();

 private:
  Lexer::SP lexer_;
  // TODO Change how the map is acquired
  std::map<char, int> binop_precedence_;
  Token curr_tok_;

  void GetNextToken();
  int GetCurrentTokenPrecedence() const;
  UPASTNode ParsePrototype();
  UPExprAST ParsePrimary();
  UPExprAST ParseIdentifierExpr();
  UPExprAST ParseParenExpr();
  UPExprAST ParseExpression();
  UPExprAST ParseBinOpRHS(int expr_prec, UPExprAST lhs);
  UPExprAST ParseNumberExpr();
  UPExprAST ParseIfExpr();
  UPExprAST ParseForExpr();

}; // class Parser

} // namespace brt

#endif
