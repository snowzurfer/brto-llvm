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
//
// TODO: Possibly move all this stuff in implementation files and only expose
//       generic program usage interface

#ifndef BRTO_PARSER_HPP
#define BRTO_PARSER_HPP

#include <memory>
#include <istream>
#include <string>
#include <map>
#include <ast.hpp>
#include <variant>
#include <utility>

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

  Token GetNextToken();
  int GetCurrentTokenPrecedence() const;

 private:
  std::istream istream_;
  char last_char_;

}; // class Lexer

class Parser {
 public:
  Parser(std::istream &istream);

  // Rule of 7
  Parser() = delete;
  Parser(const Parser &) = delete;
  Parser &operator()(const Parser &) = delete;
  Parser(Parser &&) = delete;
  Parser &operator()(Parser &&) = delete;
  ~Parser() = default;

  enum class RC {
    not_eof,
    eof,
  }; // enum class RC

  /// Parse one production or return nullptr if the production was not valid
  RC Parse();

 private:
  Lexer lexer_;
  Token curr_tok_;
  // TODO Change how the map is acquired
  std::map<char, int> binop_precedence_;

  void GetNextToken();
  int GetCurrentTokenPrecedence() const;
  void HandleExtern();
  void HandleDefinition();
  void HandleTopLevelExpression();
  UqPtrASTNode ParseTopLevelExpr();
  UqPtrASTNode ParseExtern();
  UqPtrASTNode ParseDefinition();
  UqPtrASTNode ParsePrototype();
  UqPtrASTNode ParsePrimary();
  UqPtrASTNode ParseIdentifierExpr();
  UqPtrASTNode ParseParenExpr();
  UqPtrASTNode ParseExpression();
  UqPtrASTNode ParseBinOpRHS(int expr_prec, UqPtrASTNode lhs);
  UqPtrASTNode ParseNumberExpr();

}; // class Parser

/// Use type aliasing to improve code syntax
using PRC = brt::Parser::RC;

} // namespace brt

#endif
