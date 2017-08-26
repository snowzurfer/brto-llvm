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

namespace brt {

// Forward declarations
class NumLitExprAST;
class VarExprAST;
class BinExprAST;
class CallExprAST;
class ProtoAST;
class FuncAST;

/// Type used to represent all the expression production rules
using ASTNode = std::variant<NumLitExprAST, VarExprAST, BinExprAST, CallExprAST,
                             ProtoAST, FuncAST>;

/// Emplace an instance of type T into a std::variant of type ASTNode
template <class T, typename... Args>
auto make_node(Args&&... args) {
  return std::make_unique<ASTNode>(T{std::forward<Args>(args)...});
}
/// Use type aliasing to improve code syntax
using UPtrASTNode = std::unique_ptr<ASTNode>;
using UPtrASTNodeVec = std::vector<UPtrASTNode>;
using StrVec = std::vector<std::string>;

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

private:
  std::string name_;

}; // class VarExprAST

/// Expression class for binary expressions
class BinExprAST {
public:
  BinExprAST(char op, UPtrASTNode lhs, UPtrASTNode rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)}, op_(op) {}
  BinExprAST() = delete;

  BinExprAST &operator=(const BinExprAST &) = delete;
  BinExprAST(const BinExprAST &) = delete;
  BinExprAST(BinExprAST &&) = default;
  BinExprAST &operator=(BinExprAST &&) = default;
  ~BinExprAST() = default;

  const auto &lhs() const { return *lhs_.get(); }
  const auto &rhs() const { return *rhs_.get(); }
  char op() const { return op_; }

private:
  UPtrASTNode lhs_, rhs_;
  char op_;

}; // class BinExprAST

/// Expression class for function calls
class CallExprAST {
public:
  CallExprAST(std::string callee, UPtrASTNodeVec args)
      : callee_{std::move(callee)}, args_{std::move(args)} {}
  CallExprAST() = delete;

  CallExprAST &operator=(const CallExprAST &) = delete;
  CallExprAST(const CallExprAST &) = delete;

  CallExprAST(CallExprAST &&) = default;
  CallExprAST &operator=(CallExprAST &&) = default;
  ~CallExprAST() = default;

  const auto &callee() const { return callee_; }
  const auto &args() const { return args_; }

private:
  std::string callee_;
  UPtrASTNodeVec args_;

}; // class CallExprAst

/// Class representing the prototype of a function, i.e. its signature
class ProtoAST {
public:
  ProtoAST(std::string name, StrVec args)
      : name_{std::move(name)}, args_{std::move(args)} {}
  ProtoAST() = delete;

  const auto &name() const { return name_; }
  const auto &args() const { return args_; }

private:
  std::string name_;
  StrVec args_;

}; // class ProtoAST

/// Class representing a function definition
class FuncAST {
public:
  FuncAST(UPtrASTNode proto, UPtrASTNode body)
      : proto_{std::move(proto)}, body_{std::move(body)} {}

  const auto &proto() const { return *proto_.get(); }
  const auto &body() const { return *body_.get(); }

private:
  UPtrASTNode proto_, body_;

}; // class FuncAST

/// LogError* - These are little helper functions for error handling.
/// TODO Use throw/catch instead
//UPExprAST LogError(const std::string &str) {
  //std::cerr << "Error: " << str << "\n";
  //return nullptr;
//}
//ProtoAST::TUPtr LogErrorP(const std::string &str) {
  //std::cerr << "Error: " << str << "\n";
  //return nullptr;
//}

} // namespace brt

#endif
