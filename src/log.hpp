#ifndef HCLANG_LOG_HPP
#define HCLANG_LOG_HPP

#include <fmt/core.h>
#include <cstdlib>
#include <iostream>

namespace hclang {
    namespace log {
        inline bool isEnabled = false;
    }
}

template<class... Args>
inline void log(std::string_view formatStr, Args&&...args) {
    if(hclang::log::isEnabled) {
        fmt::print(formatStr, std::forward<Args>(args)...);
    }
}

template<class... Args>
inline void elog(std::string_view formatStr, Args&&...args) {
    if(hclang::log::isEnabled) {
        fmt::print(stderr, formatStr, std::forward<Args>(args)...);
    }
}

#endif /* HCLANG_LOG_HPP */
