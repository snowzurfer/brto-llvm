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

#ifndef BRTO_AST_HPP
#define BRTO_AST_HPP

#include <string>
#include <memory>
#include <vector>
#include <iostream>
#include <variant>
#include <utility>
#include <iostream>
#include <ast_types.hpp>

namespace brt {

/// Emplace an instance of type T into a std::variant of type ASTNode
template <class T, typename... Args>
auto make_node(Args&&... args) {
  return std::make_unique<ASTNode>(T{std::forward<Args>(args)...});
}

/// Emplace an instance of type T into a std::variant of type ExprAST
template <class T, typename... Args>
    //std::enable_if<
      //(std::is_same_v<NumLitExprAST, T> ||
       //std::is_same_v<VarExprAST, T> ||
       //std::is_same_v<BinExprAST, T> ||
       //std::is_same_v<CallExprAST, T>)>>
auto make_expr(Args&&... args) {
  return std::make_unique<ExprAST>(T{std::forward<Args>(args)...});
}

/// Expression for numeric literals
class NumLitExprAST {
 public:
  NumLitExprAST(double val)
      : val_{val} {}
  NumLitExprAST() = default;

  NumLitExprAST &operator=(const NumLitExprAST &) = delete;
  NumLitExprAST(const NumLitExprAST &) = delete;
  NumLitExprAST(NumLitExprAST &&) = default;
  NumLitExprAST &operator=(NumLitExprAST &&) = default;
  ~NumLitExprAST() = default;

  const auto &val() const { return val_; }
  auto &val() { return val_; }

private:
  double val_;

}; // class NumLitExprAST

/// Expression for variables
class VarExprAST {
 public:
  VarExprAST(std::string name)
      : name_{std::move(name)} {}
  VarExprAST() = delete;

  VarExprAST &operator=(const VarExprAST &) = delete;
  VarExprAST(const VarExprAST &) = delete;
  VarExprAST(VarExprAST &&) = default;
  VarExprAST &operator=(VarExprAST &&) = default;
  ~VarExprAST() = default;

  const auto &name() const { return name_; }
  auto &name() { return name_; }

private:
  std::string name_;

}; // class VarExprAST

/// Expression class for binary expressions
class BinExprAST {
public:
  BinExprAST(char op, UPExprAST lhs, UPExprAST rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)}, op_(op) {}
  BinExprAST() = delete;

  BinExprAST &operator=(const BinExprAST &) = delete;
  BinExprAST(const BinExprAST &) = delete;
  BinExprAST(BinExprAST &&) = default;
  BinExprAST &operator=(BinExprAST &&) = default;
  ~BinExprAST() = default;

  const auto &lhs() const { return *lhs_.get(); }
  auto &lhs() { return *lhs_.get(); }
  const auto &rhs() const { return *rhs_.get(); }
  auto &rhs() { return *rhs_.get(); }
  char op() const { return op_; }
  char op() { return op_; }

private:
  UPExprAST lhs_, rhs_;
  char op_;

}; // class BinExprAST

/// Expression class for function calls
class CallExprAST {
public:
  CallExprAST(std::string callee, UPExprASTVec args)
      : callee_{std::move(callee)}, args_{std::move(args)} {}
  CallExprAST() = delete;

  CallExprAST &operator=(const CallExprAST &) = delete;
  CallExprAST(const CallExprAST &) = delete;

  CallExprAST(CallExprAST &&) = default;
  CallExprAST &operator=(CallExprAST &&) = default;
  ~CallExprAST() = default;

  const auto &callee() const { return callee_; }
  auto &callee() { return callee_; }
  const auto &args() const { return args_; }
  auto &args() { return args_; }

private:
  std::string callee_;
  UPExprASTVec args_;

}; // class CallExprAst

/// Expression class for if/then/else.
class IfExprAST {
 public:
  IfExprAST(UPExprAST cond, UPExprAST then, UPExprAST else_stm)
      : cond_{std::move(cond)}, then_{std::move(then)},
        else_{std::move(else_stm)} {}

  IfExprAST() = delete;
  IfExprAST &operator=(const IfExprAST &) = delete;
  IfExprAST(const IfExprAST&) = delete;
  IfExprAST(IfExprAST &&) = default;
  IfExprAST &operator=(IfExprAST &&) = default;
  ~IfExprAST() = default;

  const auto &cond() const { return *cond_.get(); }
  const auto &then() const { return *then_.get(); }
  const auto &else_expr() const { return *else_.get(); }
  auto &cond() { return *cond_.get(); }
  auto &then() { return *then_.get(); }
  auto &else_expr() { return *else_.get(); }

 private:
  UPExprAST cond_, then_, else_;

}; // class IfExprAST

/// Expression class for for loops
class ForExprAST {
 public:
  ForExprAST(std::string var_name, UPExprAST start, UPExprAST end,
             UPExprAST step, UPExprAST body)
      : var_name_{std::move(var_name)}, start_{std::move(start)},
        end_{std::move(end)}, step_{std::move(step)},
        body_{std::move(body)} {}

  ForExprAST() = delete;
  ForExprAST &operator=(const ForExprAST &) = delete;
  ForExprAST(const ForExprAST&) = delete;
  ForExprAST(ForExprAST &&) = default;
  ForExprAST &operator=(ForExprAST &&) = default;
  ~ForExprAST() = default;

  const auto &var_name() const { return var_name_; }
  const auto &start() const { return *start_.get(); }
  const auto &end() const { return *end_.get(); }
  const auto &step() const { return *step_.get(); }
  const auto &GetStepUP() const { return step_; }
  const auto &body() const { return *body_.get(); }
  auto &var_name() { return var_name_; }
  auto &start() { return *start_.get(); }
  auto &end() { return *end_.get(); }
  auto &step() { return *step_.get(); }
  auto &body() { return *body_.get(); }

 private:
  std::string var_name_;
  UPExprAST start_, end_, step_, body_;

}; // class ForExprAST

/// Class representing the prototype of a function, i.e. its signature
class ProtoAST {
public:
  ProtoAST(std::string name, StrVec args)
      : name_{std::move(name)}, args_{std::move(args)} {}

  ProtoAST() = delete;
  ProtoAST &operator=(const ProtoAST &) = delete;
  ProtoAST(const ProtoAST &) = delete;
  ProtoAST(ProtoAST &&) = default;
  ProtoAST &operator=(ProtoAST &&) = default;
  ~ProtoAST() = default;

  const auto &name() const { return name_; }
  auto &name() { return name_; }
  const auto &args() const { return args_; }
  auto &args() { return args_; }

private:
  std::string name_;
  StrVec args_;

}; // class ProtoAST

/// Class representing a function definition
class FuncAST {
public:
  FuncAST(UPASTNode proto, UPExprAST body)
      : proto_{std::move(proto)}, body_{std::move(body)} {}

  FuncAST() = delete;
  FuncAST &operator=(const FuncAST &) = delete;
  FuncAST(const FuncAST &) = delete;
  FuncAST(FuncAST &&) = default;
  FuncAST &operator=(FuncAST &&) = default;
  ~FuncAST() = default;

  const auto &proto() const { return *proto_.get(); }
  auto &proto() { return *proto_.get(); }
  const auto &body() const { return *body_.get(); }
  auto &body() { return *body_.get(); }
  auto &GetProtoNode() { return proto_; }

private:
  UPASTNode proto_;
  UPExprAST body_;

}; // class FuncAST

// LogError* - These are little helper functions for error handling.
// It's not recommended to use exceptions when working with LLVM
template <typename T>
T LogError(const std::string &str) {
  std::cerr << kErrMsgPrefix << str << "\n";
  return nullptr;
}


} // namespace brt

#endif
