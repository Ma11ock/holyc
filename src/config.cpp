#include "config.hpp"

#include <vector>
#include <string_view>
#include <filesystem>
#include <fmt/core.h>

namespace fs = std::filesystem;
using namespace std::string_view_literals;
using argsType = std::vector<std::string_view>;
using argsLenType = argsType::size_type;

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
    : mSourcePaths({}),mOutputPath("a.out"),mDumpAst(false),mEmitLLVM(false),
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
        } else { // No flag.
            mSourcePaths.push_back(arg);
        }
    }

    if(mSourcePaths.empty()) {
        throw std::invalid_argument("no input files");
    }
}
