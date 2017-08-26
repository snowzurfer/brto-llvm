#include <visitor.hpp>
#include <ast.hpp>
#include <llvm/IR/Type.h>
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
const char kAddOp = '+';
const char kSubOp = '-';
const char kMulOp = '*';
const char kLThanOp = '<';

Visitor::Visitor(llvm::LLVMContext &context, llvm::IRBuilder<> &builder,
                 llvm::Module &module,
                 std::map<std::string, llvm::Value *> &named_vals)
    : context_{context}, builder_{builder}, module_{module},
      named_values_{named_vals} {}

using llvm::Value;
using llvm::Function;

Visitor::RT Visitor::operator()(const NumLitExprAST &arg) {
  return llvm::ConstantFP::get(context_, llvm::APFloat(arg.val()));
}

Visitor::RT Visitor::operator()(const VarExprAST &arg) {
  // Look this variable up in the function
  Value *val = named_values_[arg.name()];

  if (!val) {
    throw VisitingErr{kUnknVarNameErrStr};
  }

  return val;
}

//template <typename T>
//class TD;

Visitor::RT Visitor::operator()(const BinExprAST &arg) {
  using llvm::Type;

  Visitor visitor{context_, builder_, module_, named_values_};

  //std::visit([](auto &&arg){TD<decltype(arg)> argType;}, arg.lhs());
  auto lhval = std::get<Value *>(std::visit(visitor, arg.lhs()));
  auto rhval = std::get<Value *>(std::visit(visitor, arg.rhs()));
  assert(lhval && rhval);

  switch (arg.op()) {
    case kAddOp: {
      return builder_.CreateFAdd(lhval, rhval, "addtmp");
    }
    case kSubOp: {
      return builder_.CreateFSub(lhval, rhval, "subtmp");
    }
    case kMulOp: {
      return builder_.CreateFMul(lhval, rhval, "multmp");
    }
    case kLThanOp: {
      lhval = builder_.CreateFCmpULT(lhval, rhval, "ltcmptmp");
      // Convert bool to double 0.0/1.0
      return builder_.CreateUIToFP(lhval, Type::getDoubleTy(context_),
                                   "booltmp");
    }
    default: {
      throw VisitingErr{kInvalidBinOpErrStr};
    }
  }
}

Visitor::RT Visitor::operator()(const CallExprAST &arg) {
  // Look up the function name in the LLVM global function table
  auto callee_func = module_.getFunction(arg.callee());
  if (!callee_func) {
    throw VisitingErr{kUnknFuncRefedErrStr};
  }

  // If the arguments mismatch
  if (callee_func->arg_size() != arg.args().size()) {
    throw VisitingErr{kIncorrectNumArgsErrStr};
  }

  std::vector<Value *> callee_args;
  callee_args.reserve(arg.args().size());
  for (auto &&arg : arg.args()) {
    Visitor visitor{context_, builder_, module_, named_values_};
    callee_args.push_back(std::get<Value *>(std::visit(visitor, *arg.get())));
    assert(callee_args.back());
  }

  return builder_.CreateCall(callee_func, std::move(callee_args), "calltmp");
}

Visitor::RT Visitor::operator()(const ProtoAST &arg) {
  using llvm::Type;
  using llvm::FunctionType;

  // Make the function type: double(double, double) etc.
  std::vector<Type *> doubles{arg.args().size(),
                              Type::getDoubleTy(context_)};
  auto ft = FunctionType::get(Type::getDoubleTy(context_), doubles, false);
  auto f = Function::Create(ft, Function::ExternalLinkage, arg.name(),
                            &module_);

  // Set names for all arguments
  size_t idx = 0;
  for (auto &&f_arg : f->args()) {
    f_arg.setName(arg.args()[idx++]);
  }

  return f;
}

Visitor::RT Visitor::operator()(const FuncAST &arg) {
  using llvm::Function;
  using llvm::BasicBlock;

  // Check for existing function declaration from using 'extern'
  auto f = module_.getFunction(std::get<ProtoAST>(arg.proto()).name());

  if (!f) {
    Visitor visitor{context_, builder_, module_, named_values_};
    f = std::get<Function *>(std::visit(visitor, arg.proto()));
  }
  assert(f);

  if (!f->empty()) {
    throw VisitingErr(kFuncRedefErrStr);
  }

  // Create a new basic block to start insertion into
  auto bb = BasicBlock::Create(context_, "entry", f);
  builder_.SetInsertPoint(bb);

  // Record function arguments in the map
  named_values_.clear();
  for (auto &arg : f->args()) {
    named_values_[arg.getName()] = &arg;
  }

  {
    Visitor visitor{context_, builder_, module_, named_values_};
    if (auto ret_val = std::get<Value *>(std::visit(visitor, arg.body()))) {
      // Finish off function
      builder_.CreateRet(ret_val);

      // Validate the generated code
      llvm::verifyFunction(*f);

      return f;
    }
  }

  // Handle error generating body
  f->eraseFromParent();
  throw VisitingErr(kBodyReadingErrErrStr);
}

} // namespace brt
