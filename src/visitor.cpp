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
const std::string kCouldntVisitIfExprASTErrStr =
  "Couldn't produce IR for If Expr AST";
const std::string kCouldntVisitForExprASTErrStr =
  "Couldn't produce IR for For loop Expr AST";
const std::string kCouldntVisitBinExprASTErrStr =
  "Couldn't produce IR for For Bin Expr AST";

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
  if (!lhval || !rhval) {
    return LogError<Value *>(kCouldntVisitBinExprASTErrStr);
  }

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

Value *ExprVisitor::operator()(IfExprAST &arg) {
  using llvm::BasicBlock;
  using llvm::APFloat;
  using llvm::Type;
  using llvm::ConstantFP;

  auto cond_val = std::visit(*this, arg.cond());
  if (!cond_val) {
    return LogError<Value *>(kCouldntVisitIfExprASTErrStr);
  }

  // Convert condition to a bool by comparing non-equal to 0.0
  cond_val = c_->builder.CreateFCmpONE(cond_val,
                                       ConstantFP::get(c_->context,
                                                       APFloat(0.0)),
                                       "ifcond");

  // Get current func object being built
  auto f = c_->builder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases. Insert the "then" block at the
  // end of the function
  auto then_bb = BasicBlock::Create(c_->context, "then", f);
  auto else_bb = BasicBlock::Create(c_->context, "else");
  auto merge_bb = BasicBlock::Create(c_->context, "ifcont");

  // Emit the condition; this works even if the else BB has not been inserted
  // into the function yet. It is standard to do so in LLVM
  c_->builder.CreateCondBr(cond_val, then_bb, else_bb);

  // Emit the then value
  c_->builder.SetInsertPoint(then_bb);

  auto then_val = std::visit(*this, arg.then());
  if (!then_val) {
    return LogError<Value *>(kCouldntVisitIfExprASTErrStr);
  }

  c_->builder.CreateBr(merge_bb);
  // Codegen of "then" can change the current block (e.g. nested if/then/else)
  // so we account this using the following line
  then_bb = c_->builder.GetInsertBlock();

  // Emit else block
  f->getBasicBlockList().push_back(else_bb);
  c_->builder.SetInsertPoint(else_bb);

  auto else_val = std::visit(*this, arg.else_expr());
  if (!else_val) {
    return LogError<Value *>(kCouldntVisitIfExprASTErrStr);
  }

  c_->builder.CreateBr(merge_bb);
  // Codegen of "else" can change the current block (e.g. nested if/then/else)
  // so we account this using the following line
  else_bb = c_->builder.GetInsertBlock();

  // Emit merge block
  f->getBasicBlockList().push_back(merge_bb);
  c_->builder.SetInsertPoint(merge_bb);
  auto phi_node = c_->builder.CreatePHI(Type::getDoubleTy(c_->context), 2,
                                        "iftmp");

  phi_node->addIncoming(then_val, then_bb);
  phi_node->addIncoming(else_val, else_bb);

  return phi_node;
}

Value *ExprVisitor::operator()(ForExprAST &arg) {
  using llvm::Type;
  using llvm::APFloat;
  using llvm::Constant;
  using llvm::ConstantFP;
  using llvm::BasicBlock;

  // Emit the start code, without 'variable' in scope
  auto start_val = std::visit(*this, arg.start());
  if (!start_val) {
    return LogError<Value *>(kCouldntVisitForExprASTErrStr);
  }

  // Create basic block for the loop header
  auto f = c_->builder.GetInsertBlock()->getParent();
  auto preheader_bb = c_->builder.GetInsertBlock();
  auto loop_bb = BasicBlock::Create(c_->context, "loop", f);

  // Insert explicit fall through from the current block to the loop BB
  c_->builder.CreateBr(loop_bb);

  // Start inserion in the loop BB
  c_->builder.SetInsertPoint(loop_bb);

  // Start the PHI node with an entry for start
  auto phi_var = c_->builder.CreatePHI(Type::getDoubleTy(c_->context), 2,
                                       arg.var_name());
  phi_var->addIncoming(start_val, preheader_bb);

  // Within the loop, the variable is defined equal to the PHI node. If it
  // shadows an existing variable, we have to restore it, so save it now
  auto old_var = c_->named_values[arg.var_name()];
  c_->named_values[arg.var_name()] = phi_var;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!std::visit(*this, arg.body())) {
    return LogError<Value *>(kCouldntVisitForExprASTErrStr);
  }

  // Emit the step value.
  Value *step_val = nullptr;
  if (arg.GetStepUP()) {
    step_val = std::visit(*this, arg.step());
    if (!step_val) {
      return LogError<Value *>(kCouldntVisitForExprASTErrStr);
    }
  } else {
    // If not specified, use 1.0.
    step_val = ConstantFP::get(c_->context, APFloat(1.0));
  }

  auto next_var = c_->builder.CreateFAdd(phi_var, step_val, "nextvar");

  // Compute the end condition.
  auto end_cond = std::visit(*this, arg.end());
  if (!end_cond) {
      return LogError<Value *>(kCouldntVisitForExprASTErrStr);
  }

  // Convert condition to a bool by comparing non-equal to 0.0.
  end_cond = c_->builder.CreateFCmpONE(end_cond, ConstantFP::get(c_->context,
                                                                 APFloat(0.0)),
                                       "loopcond");
  // Create the "after loop" block and insert it.
  auto loopend_bb = c_->builder.GetInsertBlock();
  auto *after_bb = BasicBlock::Create(c_->context, "afterloop", f);

  // Insert the conditional branch into the end of LoopEndBB.
  c_->builder.CreateCondBr(end_cond, loop_bb, after_bb);

  // Any new code will be inserted in AfterBB.
  c_->builder.SetInsertPoint(after_bb);

  // Add a new entry to the PHI node for the backedge.
  phi_var->addIncoming(next_var, loopend_bb);

  // Restore the unshadowed variable.
  if (old_var) {
    c_->named_values[arg.var_name()] = old_var;
  }
  else {
    c_->named_values.erase(arg.var_name());
  }

  // for expr always returns 0.0.
  return Constant::getNullValue(Type::getDoubleTy(c_->context));
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
    //c_->fpm->run(*f);

    return f;
  }

  // Handle error generating body
  f->eraseFromParent();
  return LogError<Function *>(kCouldntVisitFunctionASTErrStr);
}

} // namespace brt
