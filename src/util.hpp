#ifndef HCLANG_UTIL
#define HCLANG_UTIL

#include <fmt/core.h>

#ifdef __unix__
#include <cerrno>
using errorRetType = int;
#elif defined(WIN32) // Win32
extern "C" {
#include <windows.h>
using errorRetType = DWORD;
}
#endif // __unix__

namespace util {
    inline errorRetType getError() {
#ifdef __unix__
        return errno;
#elif defined(WIN32) // Win32
        return GetLastError();
#endif // __unix__
    }
    template <typename... Args>
    inline void printIndented(std::uint32_t indent, std::string_view formatStr, Args&&... args) {
        fmt::print("{:{}}", "", indent);
        fmt::print(formatStr, std::forward<Args>(args)...);
    }

}

#endif /* HCLANG_UTIL */
