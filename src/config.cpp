#include "config.hpp"

#include <vector>
#include <string_view>
#include <filesystem>
#include <fmt/core.h>
#include "log.hpp"

namespace fs = std::filesystem;
using namespace std::string_view_literals;
using argsType = std::vector<std::string_view>;
using argsLenType = argsType::size_type;

static const std::string_view HELP_STR = R"HELP(
OVERVIEW: hclang, a HolyC compiler targeting LLVM.

USAGE: hclang [options] file...

OPTIONS:
  -o <file>        Write output to <file>
  -help            Print this help message
  -ast-dump        Print out the AST for input file
  -emit-llvm       Do not compile to machine code, instead output LLVM bytecode
  -fsyntax-only    Do not compile, just verify validity of input file (emits
                   warnings and errors)
  -v               Verbose. Enables logging
  -c               Compile, but do not link

HolyC was created by Terry A. Davis.

For bug reporting or contributions please see the following URLs:
   <https://github.com/Ma11ock/holyc>
)HELP";

template<typename T>
static T getNextArg(const argsType &args, argsLenType &curIndex) {
}

template<>
std::string_view getNextArg<std::string_view>(const argsType &args,
                                              argsLenType &curIndex) {
    if(curIndex >= args.size()) {
        throw std::invalid_argument("argument to '-o' is missing (expected a file path)");
    }

    return args[curIndex++];
}

hclang::Config::Config(const argsType &args)
    : mSourcePaths({}),mOutputPath(),mDumpAst(false),mEmitLLVM(false),
      mSyntaxOnly(false) {

    for(argsLenType i = 0; i < args.size(); i++) {
        const auto &arg = args[i];

        if(arg == "-o") { // Output file.
            mOutputPath = getNextArg<std::string_view>(args, i);
        } else if(arg == "-ast-dump") {
            mDumpAst = true;
        } else if(arg == "-emit-llvm") {
            mEmitLLVM = true;
        } else if(arg == "-fsyntax-only") {
            mSyntaxOnly = true;
        } else if(arg == "-v") {
            hclang::log::isEnabled = true;
        } else if(arg == "-help") {
            mHelp = true;
            fmt::print("{}", HELP_STR);
        } else if(arg == "-c") {
            mCompilationUnit = true;
        } else { // No flag.
            mSourcePaths.push_back(arg);
        }
    }


    if(mSourcePaths.empty()) {
        throw std::invalid_argument("no input files");
    }
}
