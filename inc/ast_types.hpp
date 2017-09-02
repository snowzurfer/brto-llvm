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

#ifndef BRTO_AST_TYPES_HPP
#define BRTO_AST_TYPES_HPP

#include <variant>
#include <utility>
#include <vector>
#include <string>
#include <memory>

namespace brt {

const std::string kErrMsgPrefix = "## ERR: ";

// Forward declarations
class NumLitExprAST;
class VarExprAST;
class BinExprAST;
class CallExprAST;
class ProtoAST;
class FuncAST;

/// Type used to represent all the expression production rules
using ExprAST = std::variant<std::nullptr_t, NumLitExprAST, VarExprAST,
                             BinExprAST, CallExprAST>;
using ASTNode = std::variant<std::nullptr_t, ProtoAST, FuncAST>;

/// Use type aliasing to improve code syntax
using UPASTNode = std::unique_ptr<ASTNode>;
using UPExprAST = std::unique_ptr<ExprAST>;
using UPExprASTVec = std::vector<UPExprAST>;
using StrVec = std::vector<std::string>;

} // namespace brt

#endif
