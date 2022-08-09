#ifndef HCLANG_UTIL
#define HCLANG_UTIL

#include <fmt/core.h>
#include <cstdint>
#include <string>
#include <algorithm>
#include <filesystem>
#include <random>
#include <array>
#include <cstring>

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

    inline bool prefix(std::string_view s, std::string_view prefix) {
        if(prefix.size() > s.size()) {
            return false;
        }
        return s.rfind(prefix, 0) == 0;
    }

    inline bool postfix(std::string_view s, std::string_view postfix) {
        if(postfix.size() > s.size()) {
            return false;
        }
        return std::equal(postfix.rbegin(), postfix.rend(), s.rbegin());
    }

    std::filesystem::path mkTmp(std::string_view prefix, std::string_view suffix);

    std::string randomAlNumString(std::size_t len);

    /**
     * Create a fully-seeded mt19937.
     *
     * @return A fully-seeded mt19937.
     */
    inline std::mt19937 seededMT()
    {
        std::random_device source;
        std::array<std::random_device::result_type, (std::mt19937::state_size - 1) / sizeof(source()) + 1> randomData;
        std::generate(std::begin(randomData), std::end(randomData), std::ref(source));
        std::seed_seq seeds(std::begin(randomData), std::end(randomData));
        return std::mt19937(seeds);
    }

    inline char *strdup(const char *str) {
        if(!str) {
            return nullptr;
        }
        char *result = new char[std::strlen(str) + 1];
        std::strcpy(result, str);
        return result;
    }

    /// Helper compile time constants to find out if a type exists.
    template<typename T, typename = void>
    constexpr bool isDefined = false;
    /// Helper compile time constants to find out if a type exists.
    template<typename T>
    constexpr bool isDefined<T, decltype(typeid(T), void())> = true;
}

#define MAKE_FMT_STYLE_SPEC(what)                                       \
    template<>                                                          \
    struct fmt::formatter<fmt::detail::styled_arg<what>> {              \
        template<typename ParseContext>                                 \
        constexpr auto parse(ParseContext& ctx) { return ctx.begin(); } \
                                                                        \
        template<typename FormatContext>                                \
        auto format(const fmt::detail::styled_arg<what> &arg,           \
                    FormatContext &ctx) {                               \
            return fmt::format_to(ctx.out(), arg.style, "{}", arg.value); \
        }                                                               \
    };

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
