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

namespace brt {

/// Base class for all the expression nodes
class ExprAST {
public:
  ExprAST() = default;
  ExprAST(const ExprAST &) = default;
  ExprAST &operator=(const ExprAST &) = default;
  ExprAST(ExprAST &&) = default;
  ExprAST &operator=(ExprAST &&) = default;

  virtual ~ExprAST() = default;

}; // class ExprAST

/// Expression class for numeric literals e.g. "2.0"
class NumberExprAST final : public ExprAST {
public:
  explicit NumberExprAST(double val) : ExprAST{}, val_{val} {}
  NumberExprAST() = delete;

  NumberExprAST &operator=(const NumberExprAST &) = default;
  NumberExprAST(const NumberExprAST &) = default;
  NumberExprAST(NumberExprAST &&) = default;
  NumberExprAST &operator=(NumberExprAST &&) = default;
  ~NumberExprAST() = default;

private:
  double val_;

}; // class NumberExprAST

/// Expression class for referencing a variable, e.g. "a"
class VariableExprAST final : public ExprAST {
public:
  explicit VariableExprAST(std::string name)
      : ExprAST{}, name_{std::move(name)} {}
  VariableExprAST() = delete;

  VariableExprAST &operator=(const VariableExprAST &) = default;
  VariableExprAST(const VariableExprAST &) = default;
  VariableExprAST(VariableExprAST &&) = default;
  VariableExprAST &operator=(VariableExprAST &&) = default;
  ~VariableExprAST() = default;

private:
  std::string name_;

}; // class VariableExprAST

class BinaryExprAST final : public ExprAST {
public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : ExprAST{}, lhs_{std::move(lhs)}, rhs_{std::move(rhs)}, op_(op) {}
  BinaryExprAST() = delete;

  BinaryExprAST &operator=(const BinaryExprAST &) = delete;
  BinaryExprAST(const BinaryExprAST &) = delete;
  BinaryExprAST(BinaryExprAST &&) = default;
  BinaryExprAST &operator=(BinaryExprAST &&) = default;
  ~BinaryExprAST() = default;

private:
  std::unique_ptr<ExprAST> lhs_, rhs_;
  char op_;

}; // class BinaryExprAST


/// Expression class for function calls
class CallExprAST final : public ExprAST {
public:
  CallExprAST(std::string callee, std::vector<std::unique_ptr<ExprAST>> args)
      : ExprAST{}, callee_{std::move(callee)}, args_{std::move(args)} {}
  CallExprAST() = delete;

  CallExprAST &operator=(const CallExprAST &) = delete;
  CallExprAST(const CallExprAST &) = delete;
  CallExprAST(CallExprAST &&) = default;
  CallExprAST &operator=(CallExprAST &&) = default;
  ~CallExprAST() = default;

private:
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST>> args_;

}; // class CallExprAst

/// Class representing the prototype of a function, i.e. its signature
class PrototypeAST {
public:
  PrototypeAST(std::string name, std::vector<std::string> args)
      : name_{std::move(name)}, args_{std::move(args)} {}
  PrototypeAST() = delete;

private:
  std::string name_;
  std::vector<std::string> args_;

}; // class PrototypeAST

/// Class representing a function definition
class FunctionAST {
public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : proto_{std::move(proto)}, body_{std::move(body)} {}

private:
  std::unique_ptr<PrototypeAST> proto_;
  std::unique_ptr<ExprAST> body_;

}; // class FunctionAST

/// LogError* - These are little helper functions for error handling.
/// TODO Use throw/catch instead
std::unique_ptr<ExprAST> LogError(const std::string &str) {
  std::cerr << "Error: " << str << "\n";
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const std::string &str) {
  std::cerr << "Error: " << str << "\n";
  return nullptr;
}

} // namespace brt

#endif
