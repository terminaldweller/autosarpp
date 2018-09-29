
/*first line intentionally left blank.*/
/*************************************************************************************************/
//-*-c++-*-
// autosar cpp 14 guideline compliance checker
/*Copyright (C) 2018 Farzad Sadeghi
 * Licensed under LGPL-3.0
 * */
/*************************************************************************************************/
/*included modules*/
/*project headers*/
/*standard headers*/
#include "./autosarpp.hpp"
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
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/LLVM.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
/*************************************************************************************************/
/*used namespaces*/
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
/*************************************************************************************************/
namespace {
static llvm::cl::OptionCategory APPCat("autosarpp custom options");
}
/*************************************************************************************************/
class CalledFunc : public MatchFinder::MatchCallback {
public:
  CalledFunc(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/*************************************************************************************************/
class CalledVar : public MatchFinder::MatchCallback {
public:
  CalledVar(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {
    const VarDecl *VD = MR.Nodes.getNodeAs<clang::VarDecl>("vardecl");
  }

private:
  Rewriter &Rewrite;
};
/*************************************************************************************************/
class FuncDecl : public MatchFinder::MatchCallback {
public:
  FuncDecl(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/*************************************************************************************************/
class VDecl : public MatchFinder::MatchCallback {
public:
  VDecl(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/*************************************************************************************************/
class ClassDecl : public MatchFinder::MatchCallback {
public:
  ClassDecl(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &MR) {}

private:
  Rewriter &Rewrite;
};
/*************************************************************************************************/
class PPInclusion : public PPCallbacks {
public:
  explicit PPInclusion(SourceManager *SM, Rewriter *Rewrite)
      : SM(*SM), Rewrite(*Rewrite) {}

  virtual void MacroDefined(const Token &MacroNameTok,
                            const MacroDirective *MD) {}

  virtual void MacroExpands(const Token &MacroNameTok,
                            const MacroDefinition &MD, SourceRange Range,
                            const MacroArgs *Args) {}

#if __clang_major__ <= 6
  virtual void InclusionDirective(SourceLocation HashLoc,
                                  const Token &IncludeTok, StringRef FileName,
                                  bool IsAngled, CharSourceRange FilenameRange,
                                  const FileEntry *File, StringRef SearchPath,
                                  StringRef RelativePath,
                                  const clang::Module *Imported) {
#elif __clang_major__ >= 8
  virtual void InclusionDirective(SourceLocation HashLoc,
                                  const Token &IncludeTok, StringRef FileName,
                                  bool IsAngled, CharSourceRange FilenameRange,
                                  const FileEntry *File, StringRef SearchPath,
                                  StringRef RelativePath,
                                  const clang::Module *Imported,
                                  SrcMgr::CharacteristicKind FileType) {
#endif
  }

private:
  const SourceManager &SM;
  Rewriter &Rewrite;
};
/*************************************************************************************************/
// brief A Clang Diagnostic Consumer that does nothing.
class BlankDiagConsumer : public clang::DiagnosticConsumer {
public:
  BlankDiagConsumer() = default;
  virtual ~BlankDiagConsumer() {}
  virtual void HandleDiagnostic(DiagnosticsEngine::Level DiagLevel,
                                const Diagnostic &Info) override {}
};
/*************************************************************************************************/
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R)
      : funcDeclHandler(R), HandlerForVar(R), HandlerForClass(R),
        HandlerForCalledFunc(R), HandlerForCalledVar(R) {
#if 1
    Matcher.addMatcher(functionDecl().bind("funcdecl"), &funcDeclHandler);
    Matcher.addMatcher(
        varDecl(anyOf(unless(hasDescendant(expr(anything()))),
                      hasDescendant(expr(anything()).bind("expr"))))
            .bind("vardecl"),
        &HandlerForVar);
    Matcher.addMatcher(recordDecl(isClass()).bind("classdecl"),
                       &HandlerForClass);
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
/*************************************************************************************************/
class AppFrontendAction : public ASTFrontendAction {
public:
  AppFrontendAction() {}
  ~AppFrontendAction() { delete BDCProto; }

  void EndSourceFileAction() override {
    std::error_code EC;
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
        .write(llvm::outs());
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    CI.getPreprocessor().addPPCallbacks(
        llvm::make_unique<PPInclusion>(&CI.getSourceManager(), &TheRewriter));
    DiagnosticsEngine &DE = CI.getPreprocessor().getDiagnostics();
    DE.setClient(BDCProto, false);
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter);
  }

private:
  BlankDiagConsumer *BDCProto = new BlankDiagConsumer;
  Rewriter TheRewriter;
};
/*************************************************************************************************/
/*************************************************************************************************/
/*Main*/
int main(int argc, const char **argv) {
  CommonOptionsParser op(argc, argv, APPCat);
  const std::vector<std::string> &SourcePathList = op.getSourcePathList();
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());
  int ret = Tool.run(newFrontendActionFactory<AppFrontendAction>().get());
  return ret;
}
/*************************************************************************************************/
