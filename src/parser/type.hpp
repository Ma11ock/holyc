#ifndef SLANG_TYPE_HPP
#define SLANG_TYPE_HPP

#include <cstdint>
#include <variant>

namespace slang {

    enum class TypeSpecifier {
        // Algol 60 primitives.
        Boolean,
        Character,
        Integer,
        Real,
        Text,
        Array,
        Ref,
        // Simula classes. TODO
        Class,
    };

    // ALGOL60 primitives to C++ primitives.
    using character = char;
    using boolean = bool;
    using real = double;
    using integer = std::int64_t;
    using text = std::string;

    class Type {
    public:
        Type() = default;
        virtual ~Type() = default;
        virtual TypeSpecifier getType() const = 0;
    };
    class Array : public Type {
    public:
        Array(std::size_t startIndex, std::size_t endIndex);

        virtual ~Array() = default;
    protected:
        std::size_t mStartIndex;
        std::size_t mEndIndex;
    };

    class ConstArrayData {
    public:
        ConstArrayData(std::size_t startIndex, std::size_t endIndex, TypeSpecifier type)
            : mStartIndex(startIndex),mEndIndex(endIndex),mArrayType(type) {}
        ~ConstArrayData() = default;
        inline std::size_t getStartIndex() const { return mStartIndex; }
        inline std::size_t getEndIndex() const { return mEndIndex; }
        inline TypeSpecifier getArrayType() const { return mArrayType; }

    protected:
        std::size_t mStartIndex;
        std::size_t mEndIndex;
        TypeSpecifier mArrayType;
    };

    class DynamicArrayData {
    public:

    };
}

#endif /* SLANG_TYPE_HPP */
