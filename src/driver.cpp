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
#include <llvm/IR/Verifier.h>

namespace brt {

const std::string kErrMsgPrefix = "## ERR: ";

Driver::Driver(std::istream &istream)
    : lexer_{std::make_shared<Lexer>(istream)},
      parser_{std::make_unique<Parser>(lexer_)},
      context_{std::make_unique<llvm::LLVMContext>()},
      builder_{std::make_unique<llvm::IRBuilder<>>(*context_.get())},
      module_{std::make_unique<llvm::Module>("my test JIT", *context_.get())},
      named_values_{} {};

Driver::RC Driver::Run() {
  // Prime the lexer
  lexer_->GetNextToken();

  try {
    switch (lexer_->GetCurrToken().type) {
      case TokenType::eof: {
        return RC::eof;
      }
      case TokenType::semicolon: {// ignore top-level semicolons.
        // Consume
        lexer_->GetNextToken();
      }
      default: {
        auto ast = parser_->Parse();
        Visitor visitor{*context_.get(), *builder_.get(), *module_.get(),
                        named_values_};
        auto ir_variant = std::visit(visitor, *ast.get());
#ifdef BRTO_DEBUG_LVL_2
        std::visit([](auto &&arg) { arg->print(llvm::errs()); }, ir_variant);
#endif
        break;
      }
    }
  }
  catch (const ParsingErr &e) {
    // TODO Consume until newline
    std::cerr << kErrMsgPrefix << e.what() << "\n";
  }
  catch (const VisitingErr &e) {
    std::cerr << kErrMsgPrefix << e.what() << "\n";
  }

  return RC::not_eof;
}

} // namespace brt
