
/*first line intentionally left blank.*/
/***************************************************Project Mutator****************************************************/
//-*-c++-*-
// a clang tool blueprint
/*Copyright (C) 2018 Farzad Sadeghi

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.*/
/*code structure inspired by Eli Bendersky's tutorial on Rewriters.*/
/**********************************************************************************************************************/
/*included modules*/
/*project headers*/
/*standard headers*/
#include <cassert>
#include <cstdlib>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
/*LLVM headers*/
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/LLVM.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/raw_ostream.h"
/**********************************************************************************************************************/
/*used namespaces*/
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
/**********************************************************************************************************************/
/**********************************************************************************************************************/
/**********************************************************************************************************************/
namespace {
  static llvm::cl::OptionCategory ObfuscatorCat("Obfuscator custom options");
  std::string TEMP_FILE="/tmp/generic_temp";
}
/**********************************************************************************************************************/
class CalledFunc : public MatchFinder::MatchCallback {
  public:
    CalledFunc(Rewriter &Rewrite) : Rewrite(Rewrite) {}

    virtual void run(const MatchFinder::MatchResult &MR) {}

  private:
    Rewriter &Rewrite;
};
/**********************************************************************************************************************/
class CalledVar : public MatchFinder::MatchCallback {
  public:
    CalledVar (Rewriter &Rewrite) : Rewrite(Rewrite) {}

    virtual void run(const MatchFinder::MatchResult &MR) {
      const VarDecl* VD = MR.Nodes.getNodeAs<clang::VarDecl>("vardecl");
    }

  private:
    Rewriter &Rewrite;
};
/**********************************************************************************************************************/
class FuncDecl : public MatchFinder::MatchCallback
{
public:
  FuncDecl (Rewriter &Rewrite) : Rewrite (Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/**********************************************************************************************************************/
class VDecl : public MatchFinder::MatchCallback
{
public:
  VDecl (Rewriter &Rewrite) : Rewrite (Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/**********************************************************************************************************************/
class ClassDecl : public MatchFinder::MatchCallback {
  public:
    ClassDecl (Rewriter &Rewrite) : Rewrite(Rewrite) {}

    virtual void run(const MatchFinder::MatchResult &MR) {}

  private:
    Rewriter &Rewrite;
};
/**********************************************************************************************************************/
class PPInclusion : public PPCallbacks
{
public:
  explicit PPInclusion (SourceManager *SM, Rewriter *Rewrite) : SM(*SM), Rewrite(*Rewrite) {}

  virtual void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) {}

  virtual void MacroExpands (const Token &MacroNameTok, const MacroDefinition &MD, SourceRange Range, const MacroArgs *Args) {}

  virtual void  InclusionDirective (SourceLocation HashLoc, const Token &IncludeTok, 
      StringRef FileName, bool IsAngled, CharSourceRange FilenameRange, const FileEntry *File, 
      StringRef SearchPath, StringRef RelativePath, const clang::Module *Imported) {}

private:
  const SourceManager &SM;
  Rewriter &Rewrite;
};
/**********************************************************************************************************************/
/**
 * @brief A Clang Diagnostic Consumer that does nothing.
 */
class BlankDiagConsumer : public clang::DiagnosticConsumer
{
  public:
    BlankDiagConsumer() = default;
    virtual ~BlankDiagConsumer() {}
    virtual void HandleDiagnostic(DiagnosticsEngine::Level DiagLevel, const Diagnostic &Info) override {}
};
/**********************************************************************************************************************/
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : funcDeclHandler(R), HandlerForVar(R), HandlerForClass(R), HandlerForCalledFunc(R), HandlerForCalledVar(R) {
#if 1
    Matcher.addMatcher(functionDecl().bind("funcdecl"), &funcDeclHandler);
    Matcher.addMatcher(varDecl(anyOf(unless(hasDescendant(expr(anything()))), hasDescendant(expr(anything()).bind("expr")))).bind("vardecl"), &HandlerForVar);
    Matcher.addMatcher(recordDecl(isClass()).bind("classdecl"), &HandlerForClass);
    Matcher.addMatcher(declRefExpr().bind("calledvar"), &HandlerForCalledVar);
#endif
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

private:
  FuncDecl funcDeclHandler;
  VDecl HandlerForVar;
  ClassDecl HandlerForClass;
  CalledFunc HandlerForCalledFunc;
  CalledVar HandlerForCalledVar;
  MatchFinder Matcher;
};
/**********************************************************************************************************************/
class ObfFrontendAction : public ASTFrontendAction {
public:
  ObfFrontendAction() {}
  ~ObfFrontendAction() {
    delete BDCProto;
    delete tee;
  }

  void EndSourceFileAction() override {
    std::error_code EC;
    std::string OutputFilename = TEMP_FILE;
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(llvm::outs());
    tee = new raw_fd_ostream(StringRef(OutputFilename), EC, sys::fs::F_None);
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(*tee);
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
    CI.getPreprocessor().addPPCallbacks(llvm::make_unique<PPInclusion>(&CI.getSourceManager(), &TheRewriter));
    DiagnosticsEngine &DE = CI.getPreprocessor().getDiagnostics();
    DE.setClient(BDCProto, false);
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter);
  }

private:
  BlankDiagConsumer* BDCProto = new BlankDiagConsumer;
  Rewriter TheRewriter;
  raw_ostream *tee = &llvm::outs();
};
/**********************************************************************************************************************/
/**********************************************************************************************************************/
/*Main*/
int main(int argc, const char **argv) {
  CommonOptionsParser op(argc, argv, ObfuscatorCat);
  const std::vector<std::string> &SourcePathList = op.getSourcePathList();
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());
  int ret = Tool.run(newFrontendActionFactory<ObfFrontendAction>().get());
  return ret;
}
/**********************************************************************************************************************/
/*last line intentionally left blank.*/

