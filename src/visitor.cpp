#include <visitor.hpp>
#include <driver.hpp>
#include <ast.hpp>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constants.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <cassert>

namespace brt {

const std::string kUnknVarNameErrStr = "Unkown variable name";
const std::string kIncorrectNumArgsErrStr =
  "Incorrect # of args passed";
const std::string kFuncRedefErrStr = "Function cannot be redefined";
const std::string kUnknFuncRefedErrStr = "Unkown function referenced";
const std::string kInvalidBinOpErrStr = "Invalid binary operator";
const std::string kBodyReadingErrErrStr = "Error reading function body";
const std::string kCouldntVisitFunctionASTErrStr =
  "Couldn't produce IR for Func AST";
const char kAddOp = '+';
const char kSubOp = '-';
const char kMulOp = '*';
const char kLThanOp = '<';

using llvm::Value;
using llvm::Function;
using NamedValsMap = std::map<std::string, Value *>;
using FuncProtosMap = std::map<std::string, Value *>;

Function *GetFuncInCurrentModuleByName(std::shared_ptr<Compiler> c,
                                       const std::string &fn) {
  // Check if the function has already been added to the current module
  if (auto f = c->module->getFunction(fn)) {
    return f;
  }

  // Otherwise check if we can generate the declaration for an existing
  // prototype
  auto f_idx = c->func_protos.find(fn);
  if (f_idx != c->func_protos.end()) {
    // Generate the IR for the function prototype and return it
    return std::visit(FuncVisitor{std::move(c)}, *f_idx->second.get());
  }

  return nullptr;
}


ExprVisitor::ExprVisitor(std::shared_ptr<Compiler> c)
    : c_{std::move(c)} {};

Value *ExprVisitor::operator()(std::nullptr_t &arg) {
  return nullptr;
}

Value *ExprVisitor::operator()(NumLitExprAST &arg) {
  return llvm::ConstantFP::get(c_->context, llvm::APFloat(arg.val()));
}

Value *ExprVisitor::operator()(VarExprAST &arg) {
  // Look this variable up in the function
  auto val = c_->named_values[arg.name()];

  if (!val) {
    return LogError<Value *>(kUnknVarNameErrStr);
  }

  return val;
}

Value *ExprVisitor::operator()(BinExprAST &arg) {
  using llvm::Type;

  auto lhval = std::visit(*this, arg.lhs());
  auto rhval = std::visit(*this, arg.rhs());
  assert(lhval && rhval);

  switch (arg.op()) {
    case kAddOp: {
      return c_->builder.CreateFAdd(lhval, rhval, "addtmp");
    }
    case kSubOp: {
      return c_->builder.CreateFSub(lhval, rhval, "subtmp");
    }
    case kMulOp: {
      return c_->builder.CreateFMul(lhval, rhval, "multmp");
    }
    case kLThanOp: {
      lhval = c_->builder.CreateFCmpULT(lhval, rhval, "ltcmptmp");
      // Convert bool to double 0.0/1.0
      return c_->builder.CreateUIToFP(lhval, Type::getDoubleTy(c_->context),
                                      "booltmp");
    }
    default: {
      return LogError<Value *>(kInvalidBinOpErrStr);
    }
  }
}

Value *ExprVisitor::operator()(CallExprAST &arg) {
  // Look up the function name in the current LLVM module function table
  auto callee_func = GetFuncInCurrentModuleByName(c_, arg.callee());
  if (!callee_func) {
    return LogError<Value *>(kUnknFuncRefedErrStr);
  }

  // If the arguments mismatch
  if (callee_func->arg_size() != arg.args().size()) {
    return LogError<Value *>(kIncorrectNumArgsErrStr);
  }

  std::vector<Value *> callee_args;
  callee_args.reserve(arg.args().size());
  for (auto &&arg : arg.args()) {
    callee_args.push_back(std::visit(*this, *arg.get()));
    if (!callee_args.back()) {
      return LogError<Value *>(kIncorrectNumArgsErrStr);
    }
  }

  return c_->builder.CreateCall(callee_func, std::move(callee_args), "calltmp");
}

FuncVisitor::FuncVisitor(std::shared_ptr<Compiler> c)
    : c_{std::move(c)} {};

Function *FuncVisitor::operator()(std::nullptr_t &arg) {
  return nullptr;
}

Function *FuncVisitor::operator()(ProtoAST &arg) {
  using llvm::Type;
  using llvm::FunctionType;

  // Make the function type: double(double, double) etc.
  std::vector<Type *> doubles{arg.args().size(),
                              Type::getDoubleTy(c_->context)};
  auto ft = FunctionType::get(Type::getDoubleTy(c_->context),
                              doubles, false);
  auto f = Function::Create(ft, Function::ExternalLinkage, arg.name(),
                            c_->module.get());

  // Set names for all arguments
  size_t idx = 0;
  for (auto &&f_arg : f->args()) {
    f_arg.setName(arg.args()[idx++]);
  }

  return f;
}

Function *FuncVisitor::operator()(FuncAST &arg) {
  using llvm::Function;
  using llvm::BasicBlock;

  // Transfer ownership of the prototype to the prototypes map but also keep a
  // reference to it so that it can be used below
  auto &p = std::get<ProtoAST>(arg.proto());
  c_->func_protos[p.name()] = std::move(arg.GetProtoNode());

  auto f = GetFuncInCurrentModuleByName(c_, p.name());
  if (!f) {
    return LogError<Function *>(kCouldntVisitFunctionASTErrStr);
  }

  // Create a new basic block to start insertion into
  auto bb = BasicBlock::Create(c_->context, "entry", f);
  c_->builder.SetInsertPoint(bb);

  // Record function arguments in the map
  c_->named_values.clear();
  for (auto &arg : f->args()) {
    c_->named_values[arg.getName()] = &arg;
  }

  if (auto ret_val = std::visit(ExprVisitor{c_}, arg.body())) {
    // Finish off function
    c_->builder.CreateRet(ret_val);

    // Validate the generated code
    llvm::verifyFunction(*f);

    // Run the optimizer on the function
    c_->fpm->run(*f);

    return f;
  }

  // Handle error generating body
  f->eraseFromParent();
  return LogError<Function *>(kCouldntVisitFunctionASTErrStr);
}

} // namespace brt
