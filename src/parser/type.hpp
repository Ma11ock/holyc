#ifndef TYPE_HPP
#define TYPE_HPP

#include "ast.hpp"

namespace hclang {
    // Type handler.
    // class Type {
    // public:
    //     enum class Category {
    //         Intrinsic,
    //         Enum,
    //         Class,
    //         Union,
    //         Pointer,
    //     };
    //     Type() : mId(nullptr) {}
    //     Type(std::shared_ptr<Identifier> id) : mId(id) {}
    //     Type(const std::string &id) : mId(std::make_shared<Identifier>(id)) {}
    //     Type(std::string_view id) : mId(std::make_shared<Identifier>(id)) {}
    //     virtual ~Type() = default;

    //     virtual Category getCategory() const = 0;
    // protected:
    //     std::shared_ptr<Identifier> mId;
    // };

    // using Ty = std::shared_ptr<Type>;

    // class IntrinsicInt : public Type {
    // public:
    //     IntrinsicInt(unsigned int bitLen, bool isUnsigned = false)
    //         : Type(stringifyIntrinsic(bitLen, isUnsigned)),mBitLen(bitLen),mUnsigned(isUnsigned) {}
    //     IntrinsicInt(std::shared_ptr<Identifier> id, unsigned int bitLen, bool isUnsigned = false)
    //         : Type(id),mBitLen(bitLen),mUnsigned(isUnsigned) {}
    //     IntrinsicInt(const std::string &id, unsigned int bitLen, bool isUnsigned = false)
    //         : Type(std::make_shared<Identifier>(id)),mBitLen(bitLen),mUnsigned(isUnsigned) {}
    //     IntrinsicInt(std::string_view id, unsigned int bitLen, bool isUnsigned = false)
    //         : Type(std::make_shared<Identifier>(id)),mBitLen(bitLen),mUnsigned(isUnsigned) {}
    //     virtual ~IntrinsicInt() = default;

    //     virtual Category getCategory() const { return Type::Category::Intrinsic; }
    //     inline unsigned int getBitLen() const { return mBitLen; }
    //     inline bool isUnsigned() const { return mUnsigned; }
    //     inline bool lenIsPow2() const { return (mBitLen & (mBitLen - 1)) == 0; }

    //     virtual LLV toLLVM() const;
    //     virtual std::string stringify() const;

    //     static inline std::string stringifyIntrinsic(unsigned int bitlen,
    //                                                  bool isUnsigned) {
    //         return fmt::format("{}{}i", isUnsigned, bitlen);
    //     }

    //     static inline std::shared_ptr<IntrinsicInt> makeU16() {
    //         return std::make_shared<IntrinsicInt>("U16i", 16, true);
    //     }
    //     static inline std::shared_ptr<IntrinsicInt> makeU32() {
    //         return std::make_shared<IntrinsicInt>("U32i", 32, true);
    //     }
    // protected:
    //     unsigned int mBitLen;
    //     bool mUnsigned;
    // };

    // class Boolean : public Type {
    // public:
    //     Boolean() : Type("Boolean"sv) {}
    //     Boolean(std::shared_ptr<Identifier> id)
    //         : Type(id) {}
    //     Boolean(const std::string &id)
    //         : Type(std::make_shared<Identifier>(id)) {}
    //     Boolean(std::string_view id)
    //         : Type(std::make_shared<Identifier>(id)) {}
    //     virtual ~Boolean() = default;

    //     virtual Category getCategory() const { return Type::Category::Intrinsic; }
    //     virtual LLV toLLVM() const;
    //     virtual std::string stringify() const;
    // };

    // class Pointer : public Type {
    // public:
    //     Pointer(Ty type) : Type("Pointer"sv),mType(type) {}
    //     Pointer(std::shared_ptr<Identifier> id, Ty type)
    //         : Type(id),mType(type) {}
    //     Pointer(const std::string &id, Ty type)
    //         : Type(std::make_shared<Identifier>(id)),mType(type) {}
    //     Pointer(std::string_view id, Ty type)
    //         : Type(std::make_shared<Identifier>(id)),mType(type) {}
    //     virtual ~Pointer() = default;

    //     virtual Category getCategory() const { return Type::Category::Pointer; }
    // protected:
    //     Ty mType;
    // };

    // const inline Ty BOOLEAN_TYPE = std::make_shared<Boolean>();
    // const inline Ty U16_TYPE = IntrinsicInt::makeU16();
    // const inline Ty U32_TYPE = IntrinsicInt::makeU32();

}

#endif /* TYPE_HPP */
