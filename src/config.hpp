#ifndef SLANG_CONFIG_HPP
#define SLANG_CONFIG_HPP

#include <vector>
#include <string_view>
#include <filesystem>

namespace slang {
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

    protected:
        std::vector<std::filesystem::path> mSourcePaths;
        std::filesystem::path mOutputPath;
    };
}

#endif /* SLANG_CONFIG_HPP */
