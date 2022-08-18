#ifndef SLANG_CONFIG_HPP
#define SLANG_CONFIG_HPP

#include <filesystem>
#include <string_view>
#include <vector>

namespace hclang {
class Config {
    public:
    Config(const std::vector<std::string_view> &args);
    virtual ~Config() = default;

    inline const std::vector<std::filesystem::path> &getSourcePaths() const {
        return mSourcePaths;
    }

    inline const std::filesystem::path &getOutputPath() const {
        return mOutputPath;
    }

    inline bool shouldDumpAst() const {
        return mDumpAst;
    }

    inline bool shouldEmitLLVM() const {
        return mEmitLLVM;
    }

    inline bool syntaxOnly() const {
        return mSyntaxOnly;
    }

    inline bool wasHelp() const {
        return mHelp;
    }

    inline bool isCompilationUnit() const {
        return mCompilationUnit;
    }

    inline bool shouldSaveTmps() const {
        return mSaveTmps;
    }

    std::vector<char *> makeClangArgv(const std::vector<std::filesystem::path> &sources = {}) const;

    protected:
    std::vector<std::filesystem::path> mSourcePaths;
    std::filesystem::path mOutputPath;
    bool mDumpAst;
    bool mEmitLLVM;
    bool mSyntaxOnly;
    bool mHelp;
    bool mCompilationUnit;
    bool mSaveTmps;
};

}  // namespace hclang

#endif /* SLANG_CONFIG_HPP */
