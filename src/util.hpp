#ifndef HCLANG_UTIL
#define HCLANG_UTIL

#include <fmt/core.h>
#include <cstdint>

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

    template<typename T>
    void oracle(T &output) {
        T value;
        output = value;
    }
}

/**
 * Make binary integer functions for a type `cname`, using member `membername`,
 * for integer type `itype`. This is kept separate from the unary functions so
 * that you can define the binary functions for multiple integer types without redefinition
 * errors.
 * Call this macro in the body of a struct or class like so:
 * struct mybitfield {
 *  int value;
 *   MAKE_INTEGER_FUNCS_BINARY(mybitfield, value, int)
 *  };
 * @see MAKE_INTEGER_FUNCS_UNARY().
 * @param cname Class name these function definitions will return.
 * @param membername The member of `cname` that will be used for integer operations.
 * @param itype The integer type to define these functions for.
 */
#define MAKE_INTEGER_FUNCS_BINARY(cname, membername, itype) \
    inline operator itype() {                               \
        return value;                                       \
    }                                                       \
    inline cname operator|(itype v) const {                 \
        return membername | v;                              \
    }                                                       \
    inline cname operator|=(itype v) {                      \
        return membername |= v;                             \
    }                                                       \
    inline cname operator&(itype v) const {                 \
        return membername & v;                              \
    }                                                       \
    inline cname operator&=(itype v) {                      \
        return membername &= v;                             \
    }                                                       \
    inline cname operator^(itype v) const {                 \
        return membername ^ v;                              \
    }                                                       \
    inline cname operator^=(itype v) {                      \
        return membername ^= v;                             \
    }                                                       \
    inline cname operator<<(itype v) const {                \
        return membername << v;                             \
    }                                                       \
    inline cname operator<<=(itype v) {                     \
        return membername <<= v;                            \
    }                                                       \
    inline cname operator>>(itype v) const {                \
        return membername >> v;                             \
    }                                                       \
    inline cname operator>>=(itype v) {                     \
        return membername >>= v;                            \
    }                                                       \
    inline cname operator=(itype v) {                       \
        membername = v;                                     \
        return *this;                                       \
    }                                                       \
    inline cname operator+(itype v) {                       \
        return membername + v;                              \
    }                                                       \
    inline cname operator-(itype v) {                       \
        return membername - v;                              \
    }                                                       \
    inline cname operator*(itype v) {                       \
        return membername * v;                              \
    }                                                       \
    inline cname operator/(itype v) {                       \
        return membername / v;                              \
    }                                                       \
    inline cname operator%(itype v) {                       \
        return membername % v;                              \
    }                                                       \
    inline cname operator+=(itype v) {                      \
        membername += v;                                    \
        return *this;                                       \
    }                                                       \
    inline cname operator-=(itype v) {                      \
        membername -= v;                                    \
        return *this;                                       \
    }                                                       \
    inline cname operator*=(itype v) {                      \
        membername *= v;                                    \
        return *this;                                       \
    }                                                       \
    inline cname operator/=(itype v) {                      \
        membername /= v;                                    \
        return *this;                                       \
    }                                                       \
    inline cname operator%=(itype v) {                      \
        membername %= v;                                    \
        return *this;                                       \
    }                                                       \
    inline bool operator==(itype v) const {                 \
        return membername == v;                             \
    }                                                       \
    inline bool operator!=(itype v) const {                 \
        return membername != v;                             \
    }                                                       \

/**
 * Make unary integer functions for a type `cname`, using member `membername`.
 * Call this macro in the body of a struct or class like so:
 * struct mybitfield {
 *  int value;
 *   MAKE_INTEGER_FUNCS_UNARY(mybitfield, value)
 *  };
 * @see MAKE_INTEGER_FUNCS_BINARY().
 * @param cname Class name these function definitions will return.
 * @param membername The member of `cname` that will be used for integer operations.
 */
#define MAKE_INTEGER_FUNCS_UNARY(cname, membername) \
    inline cname operator~() const {                \
        return ~membername;                         \
    }                                               \
    inline cname operator-() const {                \
        return -membername;                         \
    }                                               \
    inline cname operator+() const {                \
        return +membername;                         \
    }                                               \
    inline bool operator!() const {                 \
        return !membername;                         \
    }                                               \
    inline cname operator++() {                     \
        ++membername;                               \
        return *this;                               \
    }                                               \
    inline cname operator--() {                     \
        --membername;                               \
        return *this;                               \
    }                                               \
    inline cname operator--(int) {                  \
        membername--;                               \
        return *this;                               \
    }                                               \
    inline cname operator++(int) {                  \
        membername++;                               \
        return *this;                               \
    }

#endif /* HCLANG_UTIL */
