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
//#include <ast.hpp>
#include <istream>
#include <string>
#include <memory>
#include <map>
#include <array>
#include <vector>
#include <sstream>
#include <cctype>
#include <cassert>
#include <stdexcept>

namespace brt {

//------------------------ Lexer ----------------------------------

const std::string kCommandTokenDef = "def";
const std::string kCommandTokenExt = "extern";
const char kCommentToken = '#';
const std::string kErrMsgPrefix = "## ERR: ";

Lexer::Lexer(std::istream &istream)
    : istream_{istream.rdbuf()}, last_char_{' '} {}

/// Retrieve a token from the stream and return it
Token Lexer::GetNextTokenInternal() {
  // Skip any whitespace
  while (std::isspace(last_char_)) {
    istream_.get(last_char_);
  }

  // Idenfitier:: [a-zA-Z][a-zA-Z0-9]
  if (std::isalpha(last_char_)) {
    std::string identifier_str{last_char_};

    istream_.get(last_char_);
    while (std::isalnum(last_char_)) {
      identifier_str += last_char_;
      istream_.get(last_char_);
    }

    if (identifier_str == kCommandTokenDef) {
      return {TokenType::def, std::move(identifier_str)};
    }
    if (identifier_str == kCommandTokenExt) {
      return {TokenType::ext, std::move(identifier_str)};
    }
    return {TokenType::identifier, std::move(identifier_str)};
  }

  // Numbers [0-9.]+
  if (std::isdigit(last_char_) || last_char_ == '.') {
    std::string num_str;
    double num_val = -1.0;
    do {
      num_str += last_char_;
      istream_.get(last_char_);
    } while (std::isdigit(last_char_) || last_char_ == '.');

    std::istringstream isstrm (num_str);
    isstrm >> num_val;
    return {TokenType::number, num_val};
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
    return {TokenType::eof, static_cast<char>(EOF)};
  }

  // Save the last character and then advance before checking what char it was
  assert(isascii(last_char_));
  char this_char = last_char_;
  std::cin.get(last_char_);

  if (this_char == ';') {
    return {TokenType::semicolon, this_char};
  }
  if (this_char == '(') {
    return {TokenType::l_bracket, this_char};
  }
  if (this_char == ')') {
    return {TokenType::r_bracket, this_char};
  }
  if (this_char == ',') {
    return {TokenType::comma, this_char};
  }

  // If all other tests fail, set as its ascii value
  return {TokenType::generic_ascii, this_char};
}

const Token &Lexer::GetNextToken() {
  curr_tok_ = GetNextTokenInternal();

  return curr_tok_;
}

const Token &Lexer::GetCurrToken() {
  return curr_tok_;
}

//------------------------ Parser ----------------------------------

const std::string kNameFuncInProtErrStr = "Expected function name in prototype";
const std::string kLBrackProtoErrStr = "Expected '(' in prototype";
const std::string kRBrackProtoErrStr = "Expected ')' in prototype";
const std::string kUnknTokExpectingExprErrStr =
  "unknown token when expecting an expression";
const std::string kRBrackCommaArgsErrStr =
  "Expected ')' or ',' in argument list";
const std::string kRBrackExpectedErrStr = "expected ')'";

Parser::Parser(const Lexer::TSPtr &lexer) : lexer_{lexer} {
  binop_precedence_['<'] = 10;
  binop_precedence_['+'] = 20;
  binop_precedence_['-'] = 20;
  binop_precedence_['*'] = 40; // highest.
}

/// top ::= definition | external | expression | ';'
UPtrASTNode Parser::Parse() {
  // Retrieve a cache of the current token
  curr_tok_ = lexer_->GetCurrToken();

  switch (curr_tok_.type) {
    case TokenType::def:
      return ParseDefinition();
    case TokenType::ext:
      return ParseExtern();
    default:
      return ParseTopLevelExpr();
      break;
  }
}

void Parser::GetNextToken() {
  curr_tok_ = lexer_->GetNextToken();
}

/// Get the precedence of the pending binary operator token.
int Parser::GetCurrentTokenPrecedence() const {
  if (curr_tok_.type != TokenType::generic_ascii) {
    return -1;
  }

  char last_char = GetTokenVal<char>(curr_tok_);

  // Make sure it's a declared binop.
  int tok_prec = binop_precedence_.at(last_char);
  if (tok_prec <= 0) {
    return -1;
  }

  return tok_prec;
}

/// toplevelexpr ::= expression
UPtrASTNode Parser::ParseTopLevelExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing toplevel\n";
#endif
  auto e = ParseExpression();
  // Make an anonymous proto.
  auto proto = make_node<ProtoAST>("", std::vector<std::string>());
  // Then return it into a function
  return make_node<FuncAST>(std::move(proto), std::move(e));
}

/// external ::= 'extern' prototype
UPtrASTNode Parser::ParseExtern() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing extern\n";
#endif
  GetNextToken();  // eat extern.
  return ParsePrototype();
}

/// definition ::= 'def' prototype expression
UPtrASTNode Parser::ParseDefinition() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing definition\n";
#endif
  GetNextToken();  // eat def.
  auto proto = ParsePrototype();

  auto e = ParseExpression();
  return make_node<FuncAST>(std::move(proto), std::move(e));
}

/// prototype
///   ::= id '(' id* ')'
UPtrASTNode Parser::ParsePrototype() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing prototype\n";
#endif
  if (curr_tok_.type != TokenType::identifier) {
    throw ParsingErr(kNameFuncInProtErrStr);
  }

  std::string fn_name = GetTokenVal<std::string>(curr_tok_);
  GetNextToken();

  if (curr_tok_.type  != TokenType::l_bracket) {
    throw ParsingErr(kLBrackProtoErrStr);
  }

  // Read the list of argument names.
  std::vector<std::string> arg_names;
  GetNextToken();
  while (curr_tok_.type == TokenType::identifier) {
    arg_names.push_back(GetTokenVal<std::string>(curr_tok_));
    GetNextToken();
  }
  if (curr_tok_.type  != TokenType::r_bracket) {
    throw ParsingErr(kRBrackProtoErrStr);
  }

  // success.
  GetNextToken();  // eat ')'.

  return make_node<ProtoAST>(std::move(fn_name), std::move(arg_names));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
UPtrASTNode Parser::ParsePrimary() {
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
      throw ParsingErr(kUnknTokExpectingExprErrStr);
    }
  }
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
UPtrASTNode Parser::ParseIdentifierExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing identifier expression\n";
#endif
  std::string id_name = GetTokenVal<std::string>(curr_tok_);

  GetNextToken();  // eat identifier.

  if (curr_tok_.type != TokenType::l_bracket) { // Simple variable ref.
    return make_node<VarExprAST>(std::move(id_name));
  }

  // Call.
  GetNextToken();  // eat (
  std::vector<UPtrASTNode> args;
  if (curr_tok_.type != TokenType::r_bracket) {
    while (1) {
      args.push_back(ParseExpression());

      if (curr_tok_.type == TokenType::r_bracket) {
        break;
      }

      if (curr_tok_.type != TokenType::comma) {
        throw ParsingErr(kRBrackCommaArgsErrStr);
      }

      GetNextToken();
    }
  }

  // Eat the ')'.
  GetNextToken();
  return make_node<CallExprAST>(std::move(id_name), std::move(args));
}

/// parenexpr ::= '(' expression ')'
UPtrASTNode Parser::ParseParenExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing paren expression\n";
#endif
  GetNextToken(); // eat (.
  auto v = ParseExpression();

  if (curr_tok_.type != TokenType::r_bracket) {
    throw ParsingErr(kRBrackExpectedErrStr);
  }

  GetNextToken(); // eat ).
  return v;
}

/// expression
///   ::= primary binoprhs
UPtrASTNode Parser::ParseExpression() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing expression\n";
#endif
  auto lhs = ParsePrimary();

  return ParseBinOpRHS(0, std::move(lhs));
}

/// binoprhs
///   ::= ('+' primary)*
UPtrASTNode Parser::ParseBinOpRHS(int expr_prec, UPtrASTNode lhs) {
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
    auto bin_op = GetTokenVal<char>(curr_tok_);
    GetNextToken();  // eat binop

    // Parse the primary expression after the binary operator.
    auto rhs = ParsePrimary();

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int next_prec = GetCurrentTokenPrecedence();
    if (tok_prec < next_prec) {
      rhs = ParseBinOpRHS(tok_prec + 1, std::move(rhs));
    }

    // Merge lhs/rhs
    lhs = make_node<BinExprAST>(bin_op, std::move(lhs),
                                          std::move(rhs));
  } // go to top of while loop and parse other binary expressions
}

/// numberexpr ::= number
UPtrASTNode Parser::ParseNumberExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing nunmberexpr\n";
#endif
  auto result = make_node<NumLitExprAST>(GetTokenVal<double>(curr_tok_));

  GetNextToken(); // consume the number

  return result;
}

} // namespace brt
