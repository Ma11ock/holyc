#include "util.hpp"

namespace fs = std::filesystem;
using path = fs::path;

path util::mkTmp(std::string_view prefix, std::string_view suffix) {
    // Try to make a random filepath 10 times. If it fails, throw.
    for(int i = 0; i < 10; i++) {
        auto maybeResult = (fs::temp_directory_path()
                            / path(prefix) / path(randomAlNumString(8)))
            .replace_extension(path(suffix));
        if(!fs::exists(maybeResult)) {
            return maybeResult;
        }
    }
    throw std::runtime_error("Could not create a temporary file...");
}

std::string util::randomAlNumString(std::size_t len) {
    static const std::array alphaNum = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E',
        'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
        'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
        'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
        'y', 'z'
    };
    static auto stringRandomDevice = seededMT();
    static std::uniform_int_distribution<std::size_t> dist(0, alphaNum.size());

    std::string result;
    result.resize(len);
    for(std::size_t i = 0; i < len; i++) {
        result[i] = alphaNum[dist(stringRandomDevice)];
    }

    return result;
}
