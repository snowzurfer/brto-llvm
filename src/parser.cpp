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

#include <parser.hpp>
#include <ast.hpp>
#include <istream>
#include <string>
#include <memory>
#include <map>
#include <array>
#include <vector>
#include <sstream>
#include <cctype>
#include <cassert>

namespace brt {

//------------------------ Lexer ----------------------------------

const std::string kCommandTokenDef = "def";
const std::string kCommandTokenExt = "extern";
const char kCommentToken = '#';

char Token::GetLastChar() const {
  assert(identifier.size() == 1);
  return *identifier.begin();
}

Lexer::Lexer(std::istream &istream)
    : istream_{istream.rdbuf()}, last_char_{' '} {}

/// Retrieve a token from the stream and return it
Token Lexer::GetNextToken() {
  std::string identifier_str;
  double num_val = -1.0;

  // Skip any whitespace
  while (std::isspace(last_char_)) {
    istream_.get(last_char_);
  }

  // Idenfitier:: [a-zA-Z][a-zA-Z0-9]
  if (std::isalpha(last_char_)) {
    identifier_str = last_char_;

    istream_.get(last_char_);
    while (std::isalnum(last_char_)) {
      identifier_str += last_char_;
      istream_.get(last_char_);
    }

    if (identifier_str == kCommandTokenDef) {
      return {TokenType::def, num_val, identifier_str};
    }
    if (identifier_str == kCommandTokenExt) {
      return {TokenType::ext, num_val, identifier_str};
    }
    return {TokenType::identifier, num_val, identifier_str};
  }

  // Numbers [0-9.]+
  if (std::isdigit(last_char_) || last_char_ == '.') {
    std::string num_str;
    do {
      num_str += last_char_;
      istream_.get(last_char_);
    } while (std::isdigit(last_char_) || last_char_ == '.');

    std::istringstream isstrm (num_str);
    isstrm >> num_val;
    return {TokenType::number, num_val, identifier_str};
  }

  // Comments
  if (last_char_ == kCommentToken) {
  do {
      istream_.get(last_char_);
    } while (last_char_ != EOF && last_char_ != '\n' && last_char_ != '\r');

    if (last_char_ != EOF) {
      return GetNextToken();
    }
  }

  // Check for end of file
  if (last_char_ == EOF) {
    identifier_str = "EOF";
    return {TokenType::eof, num_val, identifier_str};
  }

  // Save the last character and then advance before checking what char it was
  assert(isascii(last_char_));
  identifier_str = last_char_;
  char this_char = last_char_;
  std::cin.get(last_char_);

  if (this_char == ';') {
    return {TokenType::semicolon, num_val, identifier_str};
  }
  if (this_char == '(') {
    return {TokenType::l_bracket, num_val, identifier_str};
  }
  if (this_char == ')') {
    return {TokenType::r_bracket, num_val, identifier_str};
  }
  if (this_char == ',') {
    return {TokenType::comma, num_val, identifier_str};
  }

  // If all other tests fail, set as its ascii value
  return {TokenType::generic_ascii, num_val, identifier_str};
}

//------------------------ Parser ----------------------------------

Parser::Parser(std::istream &istream) : lexer_{istream} {
  binop_precedence_['<'] = 10;
  binop_precedence_['+'] = 20;
  binop_precedence_['-'] = 20;
  binop_precedence_['*'] = 40; // highest.
}

/// top ::= definition | external | expression | ';'
Parser::RC Parser::Parse() {
  // Prime the lexer
  GetNextToken();

  switch (curr_tok_.type) {
    case TokenType::eof:
      return RC::eof;
    case TokenType::semicolon: // ignore top-level semicolons.
      // Consume
      GetNextToken();
    case TokenType::def:
      HandleDefinition();
      break;
    case TokenType::ext:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
  }

  return RC::not_eof;
}

void Parser::GetNextToken() {
  curr_tok_ = lexer_.GetNextToken();
}

/// Get the precedence of the pending binary operator token.
int Parser::GetCurrentTokenPrecedence() const {
  char last_char = curr_tok_.GetLastChar();
  if (curr_tok_.type != TokenType::generic_ascii) {
    return -1;
  }

  // Make sure it's a declared binop.
  int tok_prec = binop_precedence_.at(last_char);
  if (tok_prec <= 0) {
    return -1;
  }

  return tok_prec;
}

void Parser::HandleExtern() {
  if (ParseExtern()) {
    std::cerr << "Parsed an extern\n";
  } /*else {*/
    //// Skip token for error recovery.
    // TODO Consume until newline
    //GetNextToken();
  /*}*/
}

void Parser::HandleDefinition() {
  if (ParseDefinition()) {
    std::cerr << "Parsed a function definition\n";
  }
  // TODO Consume until newline
}

void Parser::HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    std::cerr << "Parsed a top-level expr\n";
  } /*else {*/
    //// Skip token for error recovery.
    // TODO Consume until newline
    //GetNextToken();
  /*}*/
}

/// toplevelexpr ::= expression
std::unique_ptr<FunctionAST> Parser::ParseTopLevelExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing toplevel\n";
#endif
  if (auto e = ParseExpression()) {
    // Make an anonymous proto.
    auto proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
std::unique_ptr<PrototypeAST> Parser::ParseExtern() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing extern\n";
#endif
  GetNextToken();  // eat extern.
  return ParsePrototype();
}

/// definition ::= 'def' prototype expression
std::unique_ptr<FunctionAST> Parser::ParseDefinition() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing definition\n";
#endif
  GetNextToken();  // eat def.
  auto proto = ParsePrototype();
  if (!proto) {
    return nullptr;
  }

  if (auto e = ParseExpression()) {
    return std::make_unique<FunctionAST>(std::move(proto), std::move(e));
  }

  return nullptr;
}

/// prototype
///   ::= id '(' id* ')'
std::unique_ptr<PrototypeAST> Parser::ParsePrototype() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing prototype\n";
#endif
  if (curr_tok_.type != TokenType::identifier) {
    return LogErrorP("Expected function name in prototype");
  }

  std::string fn_name = curr_tok_.identifier;
  GetNextToken();

  if (curr_tok_.type  != TokenType::l_bracket) {
    return LogErrorP("Expected '(' in prototype");
  }

  // Read the list of argument names.
  std::vector<std::string> arg_names;
  GetNextToken();
  while (curr_tok_.type == TokenType::identifier) {
    arg_names.push_back(curr_tok_.identifier);
    GetNextToken();
  }
  if (curr_tok_.type  != TokenType::r_bracket) {
    return LogErrorP("Expected ')' in prototype");
  }

  // success.
  GetNextToken();  // eat ')'.

  return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
std::unique_ptr<ExprAST> Parser::ParsePrimary() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing primary\n";
#endif
  switch (curr_tok_.type) {
    case TokenType::identifier: {
        return ParseIdentifierExpr();
    }
    case TokenType::number: {
      return ParseNumberExpr();
    }
    case TokenType::l_bracket: {
      return ParseParenExpr();
    }
    default: {
      return LogError("unknown token when expecting an expression");
    }
  }
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing identifier expression\n";
#endif
  std::string id_name = curr_tok_.identifier;

  GetNextToken();  // eat identifier.

  if (curr_tok_.type != TokenType::l_bracket) { // Simple variable ref.
    return std::make_unique<VariableExprAST>(std::move(id_name));
  }

  // Call.
  GetNextToken();  // eat (
  std::vector<std::unique_ptr<ExprAST>> args;
  if (curr_tok_.type != TokenType::r_bracket) {
    while (1) {
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      }
      else {
        return nullptr;
      }

      if (curr_tok_.type == TokenType::r_bracket) {
        break;
      }

      if (curr_tok_.type != TokenType::comma) {
        return LogError("Expected ')' or ',' in argument list");
      }

      GetNextToken();
    }
  }

  // Eat the ')'.
  GetNextToken();

  return std::make_unique<CallExprAST>(std::move(id_name), std::move(args));
}

/// parenexpr ::= '(' expression ')'
std::unique_ptr<ExprAST> Parser::ParseParenExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing paren expression\n";
#endif
  GetNextToken(); // eat (.
  auto v = ParseExpression();
  if (!v) {
    return nullptr;
  }

  if (curr_tok_.type != TokenType::r_bracket) {
    return LogError("expected ')'");
  }

  GetNextToken(); // eat ).
  return v;
}

/// expression
///   ::= primary binoprhs
std::unique_ptr<ExprAST> Parser::ParseExpression() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing expression\n";
#endif
  auto lhs = ParsePrimary();
  if (!lhs) {
    return nullptr;
  }

  return ParseBinOpRHS(0, std::move(lhs));
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST> Parser::ParseBinOpRHS(int expr_prec,
                                              std::unique_ptr<ExprAST> lhs) {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing binoprhs\n";
#endif
  // If this is a binop, find its precedence.
  while (1) {
    auto tok_prec = GetCurrentTokenPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (tok_prec < expr_prec) {
      return lhs;
    }

    // Okay, we know this is a binop.
    auto bin_op = curr_tok_.GetLastChar();
    GetNextToken();  // eat binop

    // Parse the primary expression after the binary operator.
    auto rhs = ParsePrimary();
    if (!rhs) {
      return nullptr;
    }

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int next_prec = GetCurrentTokenPrecedence();
    if (tok_prec < next_prec) {
      rhs = ParseBinOpRHS(tok_prec + 1, std::move(rhs));
    }

    // Merge lhs/rhs
    lhs = std::make_unique<BinaryExprAST>(bin_op, std::move(lhs),
                                          std::move(rhs));
  } // go to top of while loop and parse other binary expressions
}

/// numberexpr ::= number
std::unique_ptr<ExprAST> Parser::ParseNumberExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing nunmberexpr\n";
#endif
  auto result = std::make_unique<NumberExprAST>(curr_tok_.val);
  GetNextToken(); // consume the number
  return result;
}

} // namespace brt
