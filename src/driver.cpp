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

#include <driver.hpp>
#include <parser.hpp>
#include <utility>
#include <visitor.hpp>
#include <ast.hpp>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

namespace brt {

using llvm::LLVMContext;
using llvm::IRBuilder;
using llvm::Module;
using llvm::Value;
using llvm::Function;
using llvm::orc::KaleidoscopeJIT;
using llvm::legacy::FunctionPassManager;

Compiler::Compiler()
    : context{},
      builder{context},
      module{},
      named_values{},
      kl_jit{},
      fpm{},
      func_protos{} {
  // Prepare environment to create code for the current native target
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  kl_jit = std::make_unique<KaleidoscopeJIT>();

  InitialiseModuleAndPassManager();
}

void Compiler::InitialiseModuleAndPassManager() {
  // Open a new module
  module = std::make_unique<Module>("My test JIT", context);
  module->setDataLayout(kl_jit->getTargetMachine().createDataLayout());

  // Create a new pass manager and attach it to the newly created module
  fpm = std::make_unique<FunctionPassManager>(module.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  fpm->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  fpm->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  fpm->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  fpm->add(llvm::createCFGSimplificationPass());

  fpm->doInitialization();
}

Driver::Driver(std::istream &istream)
    : l_{std::make_shared<Lexer>(istream)},
      p_{std::make_unique<Parser>(l_)},
      c_{std::make_unique<Compiler>()} {}

template <typename Tast>
auto RunFuncVisitor(std::shared_ptr<Compiler> c, Tast &ast) {
    auto ir = std::visit(FuncVisitor{std::move(c)}, ast);
//#ifdef BRTO_DEBUG_LVL_2
    if (ir) {
      ir->dump();
    };
//#endif
    return ir;
}

Driver::RC Driver::Run() {
  // Prime the lexer
  l_->GetNextToken();

  switch (l_->GetCurrToken().type) {
    case TokenType::eof: {
      return RC::eof;
    }
    /// top ::= definition | external | expression | ';'
    case TokenType::semicolon: {// ignore top-level semicolons.
      // Consume
      l_->GetNextToken();
    }
    case TokenType::def: {
      auto ast = p_->ParseDefinition();
      if (!ast) {
        break;
      }

      auto ir = RunFuncVisitor(c_, *ast.get());
      if (!ir) {
        break;
      }
      // We add each function to a new module; the JIT will pick the
      // most recent function from the modules automatically
      assert(c_->module.get());
      c_->kl_jit->addModule(std::move(c_->module));
      c_->InitialiseModuleAndPassManager();

      break;
    }
    case TokenType::ext: {
      auto ast = p_->ParseExtern();
      if (!ast) {
        break;
      }

      auto ir = RunFuncVisitor(c_, *ast.get());
      if (!ir) {
        break;
      }

      // We transfer ownership of the func prototype so that when it is called
      // later it is found
      c_->func_protos[std::get<ProtoAST>(*ast.get()).name()] =
        std::move(ast);

      break;
    }
    default: {
      auto ast = p_->ParseTopLevelExpr();
      if (!ast) {
        break;
      }

      auto ir = RunFuncVisitor(c_, *ast.get());
      if (!ir) {
        break;
      }
      ExecTopLvlModule();

      break;
    }
  }

  // TODO Do error recovery if something went wrong; consume whole line
  return RC::not_eof;
}

void Driver::ExecTopLvlModule() {
  // JIT the module with the anonymous expr, keeping a handle to it so
  // that we can free it later
  auto mh = c_->kl_jit->addModule(std::move(c_->module));
  c_->InitialiseModuleAndPassManager();

  // Search the JIT for the __anon_expr symbol
  auto expr_sym = c_->kl_jit->findSymbol("__anon_expr");
  assert(expr_sym);

  // Get the symbol's address and cast it to the required type
  // (which takes no arguments, returns a double) so we can call it as
  // a native function
  double (*fp)() = (double (*)())(intptr_t)expr_sym.getAddress();
  // Evaluate it
  std::cerr << ">>> " << fp() << "\n";

  // Delete the module containing the anon expr since it has by this
  // point been evaluated
  c_->kl_jit->removeModule(mh);
}

} // namespace brt

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef LLVM_ON_WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}
