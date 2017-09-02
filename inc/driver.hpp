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

#ifndef BRTO_DRIVER_HPP
#define BRTO_DRIVER_HPP

#include <memory>
#include <map>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <KaleidoscopeJIT.h>
#include <variant>
#include <iostream>
#include <visitor.hpp>

// Forward declarations
namespace brt {

class Lexer;
class Parser;

} // namespace brt

namespace brt {

struct Compiler {
  Compiler();

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> module;
  std::map<std::string, llvm::Value *> named_values;
  std::unique_ptr<llvm::orc::KaleidoscopeJIT> kl_jit;
  std::unique_ptr<llvm::legacy::FunctionPassManager> fpm;
  std::map<std::string, UPASTNode> func_protos;

  void InitialiseModuleAndPassManager();
  // Return nullptr if it didn't succeed
  llvm::Function *GetFuncInCurrentModuleByName(const std::string &fn);
};

class Driver {
 public:
  Driver(std::istream &istream);

  Driver() = delete; // Unnecessary but good bc of rule of 5
  Driver &operator=(const Driver &) = delete;
  Driver(const Driver &) = delete;
  Driver(Driver &&) = delete;
  Driver &operator=(Driver &&) = delete;
  ~Driver() = default;

  // Return condition
  enum class RC { eof, not_eof }; // enum class RC

  RC Run();

 private:
  std::shared_ptr<Lexer> l_;
  std::unique_ptr<Parser> p_;
  std::shared_ptr<Compiler> c_;

  void ExecTopLvlModule();

}; // class Driver

} // namespace brt

using DrRC = brt::Driver::RC;

#endif
