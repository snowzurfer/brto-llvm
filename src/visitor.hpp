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

#ifndef BRT_VISITOR_HPP
#define BRT_VISITOR_HPP

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <string>
#include <map>
#include <variant>

// Forward declarations
namespace brt {

class NumLitExprAST;
class VarExprAST;
class BinExprAST;
class CallExprAST;
class ProtoAST;
class FuncAST;

} // namespace brt

namespace brt {

class Visitor {
 public:
  Visitor(llvm::LLVMContext &context, llvm::IRBuilder<> &builder,
          llvm::Module &module,
          std::map<std::string, llvm::Value *> &named_vals);

  using RT = std::variant<llvm::Value *, llvm::Function *>;

  RT operator()(const NumLitExprAST &arg);
  RT operator()(const VarExprAST &arg);
  RT operator()(const BinExprAST &arg);
  RT operator()(const CallExprAST &arg);
  RT operator()(const ProtoAST &arg);
  RT operator()(const FuncAST &arg);

 private:
  llvm::LLVMContext &context_;
  llvm::IRBuilder<> &builder_;
  llvm::Module &module_;
  std::map<std::string, llvm::Value *> &named_values_;

}; // class Visitor

// Exception classes
class VisitingErr : public std::runtime_error {
 public:
  explicit VisitingErr(const std::string &what_arg)
      : runtime_error(what_arg) {}
}; // class ParsingErr


} // namespace brt

#endif
