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
#include <istream>
#include <string>
#include <memory>
#include <map>
#include <array>
#include <vector>
#include <sstream>
#include <cassert>
#include <iostream>
#include <ast.hpp>


namespace brt {

//------------------------ Lexer ----------------------------------

const std::string kCommandTokenDef = "def";
const std::string kCommandTokenExt = "extern";
const std::string kCommandTokenIf = "if";
const std::string kCommandTokenThen = "then";
const std::string kCommandTokenElse= "else";
const std::string kCommandTokenFor = "for";
const std::string kCommandTokenIn = "in";
const char kCommentToken = '#';

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
    if (identifier_str == kCommandTokenIf) {
      return {TokenType::if_stm, std::move(identifier_str)};
    }
    if (identifier_str == kCommandTokenThen) {
      return {TokenType::then, std::move(identifier_str)};
    }
    if (identifier_str == kCommandTokenElse) {
      return {TokenType::else_stm, std::move(identifier_str)};
    }
    if (identifier_str == kCommandTokenFor) {
      return {TokenType::for_loop, std::move(identifier_str)};
    }
    if (identifier_str == kCommandTokenIn) {
      return {TokenType::in, std::move(identifier_str)};
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

const std::string kAnonExprName = "__anon_expr";
const std::string kExpectedNameFuncInProtErrStr =
  "Expected function name in prototype";
const std::string kLBrackProtoErrStr = "Expected '(' in prototype";
const std::string kRBrackProtoErrStr = "Expected ')' in prototype";
const std::string kUnknTokExpectingExprErrStr =
  "unknown token when expecting an expression";
const std::string kRBrackCommaArgsErrStr =
  "Expected ')' or ',' in argument list";
const std::string kRBrackExpectedErrStr = "expected ')'";
const std::string kCouldntParseTopLvlExpr = "Could not parse Top Lvl Expr";
const std::string kCouldntParseDef = "Could not parse Def";
const std::string kCouldntParseIdentExpr = "Couldn't parse Identifier Expr";
const std::string kCouldntParseParenExpr = "Couldn't parse Paren Expr";
const std::string kCouldntParseExpr = "Couldn't parse Expr";
const std::string kCouldntParseBinOpExpr = "Couldn't parse Binop Expr";
const std::string kCouldntParseIfExprErrStr = "Couldn't parse If Expr";
const std::string kExpectedThenErrStr = "Expected then";
const std::string kExpectedElseErrStr = "Expected else";
const std::string kExpectedIdentAfterForErrStr =
  "Expected identifier after for";
const std::string kExpectedEqualAfterForErrStr =
  "Expected '=' after for";
const std::string kCouldntParseForExprErrStr = "Couldn't parse For Expr";
const std::string kExpectedCommaAfterForErrStr =
"Expected ',' after for start value";
const std::string kExpectedInAfterForErrStr = "expected 'in' after for";

Parser::Parser(Lexer::SP lexer) : lexer_{std::move(lexer)} {
  binop_precedence_['<'] = 10;
  binop_precedence_['+'] = 20;
  binop_precedence_['-'] = 20;
  binop_precedence_['*'] = 40; // highest.
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
UPASTNode Parser::ParseTopLevelExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing toplevel\n";
#endif
  if (auto e = ParseExpression()) {;
    // Make an anonymous proto.
    auto proto = make_node<ProtoAST>(kAnonExprName, std::vector<std::string>());
    // Then return it into a function
    return make_node<FuncAST>(std::move(proto), std::move(e));
  }

  return LogError<UPASTNode>(kCouldntParseTopLvlExpr);
}

/// external ::= 'extern' prototype
UPASTNode Parser::ParseExtern() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing extern\n";
#endif
  GetNextToken();  // eat extern.
  return ParsePrototype();
}

/// definition ::= 'def' prototype expression
UPASTNode Parser::ParseDefinition() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing definition\n";
#endif
  GetNextToken();  // eat def.
  auto proto = ParsePrototype();
  if (!proto) {
    return LogError<UPASTNode>(kCouldntParseDef);
  }

  auto e = ParseExpression();
  if (!e) {
    return LogError<UPASTNode>(kCouldntParseDef);
  }

  return make_node<FuncAST>(std::move(proto), std::move(e));
}

/// prototype
///   ::= id '(' id* ')'
UPASTNode Parser::ParsePrototype() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing prototype\n";
#endif
  if (curr_tok_.type != TokenType::identifier) {
    return LogError<UPASTNode>(kExpectedNameFuncInProtErrStr);
  }

  std::string fn_name = GetTokenVal<std::string>(curr_tok_);
  GetNextToken();

  if (curr_tok_.type  != TokenType::l_bracket) {
    return LogError<UPASTNode>(kLBrackProtoErrStr);
  }

  // Read the list of argument names.
  std::vector<std::string> arg_names;
  GetNextToken();
  while (curr_tok_.type == TokenType::identifier) {
    arg_names.push_back(GetTokenVal<std::string>(curr_tok_));
    GetNextToken();
  }
  if (curr_tok_.type  != TokenType::r_bracket) {
    return LogError<UPASTNode>(kRBrackProtoErrStr);
  }

  // success.
  GetNextToken();  // eat ')'.

  return make_node<ProtoAST>(std::move(fn_name), std::move(arg_names));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
UPExprAST Parser::ParsePrimary() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing primary\n";
#endif
  curr_tok_ = lexer_->GetCurrToken();

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
    case TokenType::if_stm: {
      return ParseIfExpr();
    }
    case TokenType::for_loop: {
      return ParseForExpr();
    }
    default: {
      return LogError<UPExprAST>(kUnknTokExpectingExprErrStr);
    }
  }
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
UPExprAST Parser::ParseIdentifierExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing identifier expression\n";
#endif
  std::string id_name = GetTokenVal<std::string>(curr_tok_);

  GetNextToken();  // eat identifier.

  if (curr_tok_.type != TokenType::l_bracket) { // Simple variable ref.
    return make_expr<VarExprAST>(std::move(id_name));
  }

  // Call.
  GetNextToken();  // eat (
  UPExprASTVec args;
  if (curr_tok_.type != TokenType::r_bracket) {
    while (1) {
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      }
      else {
        return LogError<UPExprAST>(kCouldntParseIdentExpr);
      }

      if (curr_tok_.type == TokenType::r_bracket) {
        break;
      }

      if (curr_tok_.type != TokenType::comma) {
        return LogError<UPExprAST>(kRBrackCommaArgsErrStr);
      }

      GetNextToken();
    }
  }

  // Eat the ')'.
  GetNextToken();
  return make_expr<CallExprAST>(std::move(id_name), std::move(args));
}

/// parenexpr ::= '(' expression ')'
UPExprAST Parser::ParseParenExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing paren expression\n";
#endif
  GetNextToken(); // eat (.
  auto v = ParseExpression();
  if (!v) {
    return LogError<UPExprAST>(kCouldntParseDef);
  }

  if (curr_tok_.type != TokenType::r_bracket) {
    return LogError<UPExprAST>(kCouldntParseDef);
  }

  GetNextToken(); // eat ).
  return v;
}

/// expression
///   ::= primary binoprhs
UPExprAST Parser::ParseExpression() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing expression\n";
#endif
  auto lhs = ParsePrimary();
  if (!lhs) {
    return LogError<UPExprAST>(kCouldntParseExpr);
  }

  return ParseBinOpRHS(0, std::move(lhs));
}

/// binoprhs
///   ::= ('+' primary)*
UPExprAST Parser::ParseBinOpRHS(int expr_prec, UPExprAST lhs) {
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
    if (!rhs) {
      return LogError<UPExprAST>(kCouldntParseBinOpExpr);
    }

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int next_prec = GetCurrentTokenPrecedence();
    if (tok_prec < next_prec) {
      rhs = ParseBinOpRHS(tok_prec + 1, std::move(rhs));
      if (!rhs) {
        return LogError<UPExprAST>(kCouldntParseBinOpExpr);
      }
    }

    // Merge lhs/rhs
    lhs = make_expr<BinExprAST>(bin_op, std::move(lhs),
                                          std::move(rhs));
  } // go to top of while loop and parse other binary expressions
}

/// numberexpr ::= number
UPExprAST Parser::ParseNumberExpr() {
#ifdef BRTO_DEBUG_LVL_2
  std::cerr << "Parsing nunmberexpr\n";
#endif
  UPExprAST result = make_expr<NumLitExprAST>(GetTokenVal<double>(curr_tok_));

  GetNextToken(); // consume the number

  return result;
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
UPExprAST Parser::ParseIfExpr() {
  GetNextToken();  // eat the if.

  // Parse the condition
  auto cond = ParseExpression();
  if (!cond) {
    return LogError<UPExprAST>(kCouldntParseIfExprErrStr);
  }

  if (curr_tok_.type != TokenType::then) {
    return LogError<UPExprAST>(kExpectedThenErrStr);
  }

  GetNextToken();  // eat the then

  auto then = ParseExpression();
  if (!then) {
    return LogError<UPExprAST>(kCouldntParseIfExprErrStr);
  }

  if (curr_tok_.type != TokenType::else_stm) {
    return LogError<UPExprAST>(kExpectedElseErrStr);
  }

  GetNextToken();

  auto else_expr = ParseExpression();
  if (!else_expr) {
    return LogError<UPExprAST>(kCouldntParseIfExprErrStr);
  }

  return make_expr<IfExprAST>(std::move(cond), std::move(then),
                              std::move(else_expr));
}

UPExprAST Parser::ParseForExpr() {
  GetNextToken();  // eat the for.

  if (curr_tok_.type != TokenType::identifier) {
    return LogError<UPExprAST>(kExpectedIdentAfterForErrStr);
  }

  std::string id_name = std::get<std::string>(curr_tok_.val);
  GetNextToken();  // eat identifier.

  if (std::get<char>(curr_tok_.val) != '=') {
    return LogError<UPExprAST>(kExpectedEqualAfterForErrStr);
  }

  GetNextToken();  // eat '='.

  auto start = ParseExpression();
  if (!start) {
    return LogError<UPExprAST>(kCouldntParseForExprErrStr);
  }

  if (std::get<char>(curr_tok_.val) != ',') {
    return LogError<UPExprAST>(kExpectedCommaAfterForErrStr);
  }

  GetNextToken();

  auto end = ParseExpression();
  if (!end) {
    return LogError<UPExprAST>(kCouldntParseForExprErrStr);
  }

  // The step value is optional.
  UPExprAST step;
  if (std::get<char>(curr_tok_.val) == ',') {
    GetNextToken();
    step = ParseExpression();
    if (!step) {
      return LogError<UPExprAST>(kCouldntParseForExprErrStr);
    }
  }

  if (curr_tok_.type != TokenType::in) {
    return LogError<UPExprAST>(kExpectedInAfterForErrStr);
  }

  GetNextToken();  // eat 'in'.

  auto body = ParseExpression();
  if (!body) {
      return LogError<UPExprAST>(kCouldntParseForExprErrStr);
  }

  return make_expr<ForExprAST>(id_name, std::move(start), std::move(end),
                               std::move(step), std::move(body));
}

} // namespace brt
