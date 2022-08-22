/***************************************************************************
 * @file llvm.cpp                                                          *
 * @brief LLVM code generation from abstract syntax tree. Uses LLVM 14.1.  *
 ***************************************************************************/
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <stdexcept>
#include <string_view>

#include "../log.hpp"
#include "ast.hpp"
#include "fmt/core.h"
#include "lexer/lexer.hpp"
#include "parser.hpp"
#include "symbols.hpp"

extern "C" {
#include <sys/wait.h>
#include <unistd.h>
}

// Aliases.

using O = hclang::Operator;
using hct = hclang::HCType;

/**
 * Stack of LLVM blocks for managing branching.
 */
class BlockStack {
    public:
    /**
     * Struct for blocks, and an optional "merge" block, which is pushed to the
     * stack after a pop.
     */
    struct blockPair {
        /// Current block.
        llvm::BasicBlock *block;
        /// Merge block. Pushed to stack after a pop if not null.
        llvm::BasicBlock *merge = nullptr;
    };

    /**
     * Default constructor. Create block stack.
     */
    BlockStack() = default;

    /**
     * Default destructor.
     */
    ~BlockStack() = default;

    /**
     * Get the most recent merge block.
     * @return The most recent merge block, or NULL if there isn't one.
     */
    inline llvm::BasicBlock *peekMerge() const {
        for (const auto &pair : mBlockStack) {
            if (pair.merge) {
                return pair.merge;
            }
        }
        return nullptr;
    }

    /**
     * Get the top of the block stack.
     * @return The top of the block stack.
     */
    inline blockPair peek() const {
        if (mBlockStack.empty())
            return blockPair{nullptr, nullptr};
        return mBlockStack.front();
    }

    /**
     * Pop the block stack. If the top of the stack has a merge block,
     * push that to the stop after popping.
     * Note: Does not allow popping off the entry block.
     * @param builder LLVM builder to set the insertion point of.
     * @return The block pair that was popped off the stack. Or a pair of
     * NULL if the stack was empty or just the entry block was present.
     * @see blockPair
     */
    inline blockPair pop(llvm::IRBuilder<> &builder) {
        // Do not allow entry to popped and ignore if stack is empty.
        if (mBlockStack.empty()) {
            return blockPair{nullptr, nullptr};
        }

        auto result = mBlockStack.back();
        mBlockStack.pop_front();
        if (result.merge) {
            push(result.merge, builder, nullptr);
        } else {
            setInsertPoint(builder);
        }
        return result;
    }

    /**
     * Push block onto the stack, and set the insertion point.
     * @param pair The block pair to push onto the stack.
     * @parm builder Builder to set insertion point to.
     * @return The block pair that was pushed onto the stack. Or a pair
     * of NULL if `pair.block` was NULL.
     * @see blockPair
     */
    inline blockPair push(blockPair pair, llvm::IRBuilder<> &builder) {
        if (!pair.block) {
            return blockPair{nullptr, nullptr};
        }
        mBlockStack.push_front(pair);
        builder.SetInsertPoint(pair.block);
        return pair;
    }

    /**
     * Construct a new block of name `name` and push it onto the stack.
     * @param name The name of the block to push.
     * @param context LLVMContext to use to make a BasicBlock.
     * @parm builder Builder to set insertion point to.
     * @param func Function to insert block into.
     * @param merge Optional merge block.
     * @return The block pair that was pushed onto the stack. Or a pair
     * of NULL if `pair.block` was NULL.
     * @see blockPair
     */
    inline blockPair push(std::string_view name, llvm::LLVMContext &context,
                          llvm::IRBuilder<> &builder, llvm::Function *func,
                          llvm::BasicBlock *merge = nullptr) {
        return push(blockPair{llvm::BasicBlock::Create(context, name, func), merge}, builder);
    }

    /**
     * Push block onto the stack, and set an insertion point.
     * @param block The block to push to the stack and set insertion point to.
     * @parm builder Builder to set insertion point to.
     * @param merge Optional merge block.
     * @return The block pair that was pushed onto the stack. Or a pair
     * or NULL if `pair.block` was NULL.
     * @see blockPair
     */
    inline blockPair push(llvm::BasicBlock *block, llvm::IRBuilder<> &builder,
                          llvm::BasicBlock *merge = nullptr) {
        return push(blockPair{block, merge}, builder);
    }

    /**
     * Set the insertion point of the program to the top of the stack.
     * If the stack is empty, do nothing.
     * @param builder LLVM builder to set insert point of.
     */
    inline void setInsertPoint(llvm::IRBuilder<> &builder) const {
        if (mBlockStack.empty()) {
            return;
        }
        builder.SetInsertPoint(peek().block);
    }

    protected:
    /// Stack of blocks for the current function.
    std::list<blockPair> mBlockStack;
};

// I really wish I was using Rust right now :/.
/**
 * Object for the LLVM symbol table. Holds a variable or function information.
 */
struct llvmSymbol {
    llvm::Value *variable;
    llvm::FunctionType *funcType;
    llvm::Function *func;

    bool isVar() const {
        return static_cast<bool>(variable);
    }

    bool isFunc() const {
        return funcType && func;
    }

    llvmSymbol() : variable(nullptr), funcType(nullptr), func(nullptr) {
    }

    llvmSymbol(llvm::Value *v) : variable(v) {
    }

    llvmSymbol(llvm::FunctionType *funcType, llvm::Function *func)
        : variable(nullptr), funcType(funcType), func(func) {
    }

    llvmSymbol &operator=(llvm::Value *v) {
        variable = v;
        funcType = nullptr;
        func = nullptr;
        return *this;
    }

    operator bool() const {
        return variable || funcType || func;
    }
};

namespace hclang {
/**
 * Parser context information used for LLVM code generation.
 */
struct parserContext {
    /// Symbol table with LLVM values.
    SymbolTable<llvmSymbol> &symbolTable;
    /// Stack used for managing branches.
    BlockStack &blockStack;
    /// LLVM context.
    llvm::LLVMContext &context;
    /// Builder context.
    llvm::IRBuilder<> &builder;
    /// Module for current object file.
    llvm::Module *module;
};

/**
 * Casts LLVM bytecode object into different type of bytecode object.
 */
class LLVMCast : public hclang::GrammarRule {
    public:
    /**
     * Value constructor. Sets all members.
     * @param type HCType that the LLVM object represents.
     * @param expr LLVM bytecode object to cast.
     */
    LLVMCast(typeInfo type, LLV expr) : mExpr(expr), mType(type) {
    }

    /**
     * Default destructor.
     */
    virtual ~LLVMCast() = default;

    /**
     * Generate LLVM bytecode.
     * @return LLVM object representing this production rule.
     */
    virtual LLV toLLVM(parserContext &pc) const;

    /// Pretty print this grammar rule (does not print children).
    virtual void pprint() const {
    }

    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::string_view getClassName() const {
        return "LLVMCast";
    }

    /**
     * Get class name.
     * @return Class name.
     */
    virtual std::list<GR> getChildren() const {
        return {};
    }

    /**
     * Perform semantic parsing of `this` and children. Ensures rules of
     * HolyC, like the type system, are followed.
     */
    virtual void parseSemantics(semanticContext &sc) {
    }

    protected:
    /// LLVM expression to create cast for.
    LLV mExpr;
    /// HolyC type of `mExpr`.
    typeInfo mType;
};
}  // namespace hclang

// Static functions.

/**
 * Get an LLVM type that corresponds to HolyC type `ti`.
 * @param ti HolyC type to convert into an LLVM type.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM type that is equivalent to `ti`.
 */
static llvm::Type *llvmTypeFrom(const hclang::typeInfo &ti, hclang::parserContext &pc) {
    switch (ti.type) {
    case hct::U64i:
    case hct::I64i:
        return llvm::Type::getInt64Ty(pc.context);
        break;
    case hct::U32i:
    case hct::I32i:
        return llvm::Type::getInt32Ty(pc.context);
        break;
    case hct::U16i:
    case hct::I16i:
        return llvm::Type::getInt16Ty(pc.context);
        break;
    case hct::U8i:
    case hct::I8i:
        return llvm::Type::getInt8Ty(pc.context);
        break;
    case hct::U0i:
    case hct::I0i:
        return llvm::Type::getVoidTy(pc.context);
        break;
    case hct::F64:
        return llvm::Type::getDoubleTy(pc.context);
        break;
    case hct::Pointer:
        return llvm::PointerType::get(llvmTypeFrom(*ti.pointer, pc), 0);
        break;
    default:
        break;
    }
    return nullptr;
}

/**
 * Generate a floating point (double) constant.
 * @param val Double to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM f64 value equal to `val`.
 */
static inline llvm::Value *f64Constant(double val, hclang::parserContext &pc) {
    return llvm::ConstantFP::get(pc.context, llvm::APFloat(val));
}

/**
 * Generate a floating point (double) constant.
 * @param val Double to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i1 value equal to `val`.
 */
static inline llvm::Value *integerConstant(bool val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(1, static_cast<std::uint8_t>(val)));
}

/**
 * Generate a 1 byte unsigned integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i8 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::uint8_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(8, val, false));
}

/**
 * Generate a 1 byte signed integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i8 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::int8_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(8, val, true));
}

/**
 * Generate a 2 byte unsigned integer constant.
 * @param val Integer to make constant from.
 * @return LLVM i16 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::uint16_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(16, val, false));
}

/**
 * Generate a 2 byte signed integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i16 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::int16_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(16, val, true));
}

/**
 * Generate a 4 byte unsigned integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i32 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::uint32_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(32, val, false));
}

/**
 * Generate a 4 byte signed integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i32 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::int32_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(32, val, true));
}

/**
 * Generate a 8 byte unsigned integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i64 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::uint64_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(64, val, false));
}

/**
 * Generate a 8 byte signed integer constant.
 * @param val Integer to make constant from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i64 value equal to `val`.
 */
static inline llvm::Value *integerConstant(std::int64_t val, hclang::parserContext &pc) {
    return llvm::ConstantInt::get(pc.context, llvm::APInt(64, val, true));
}

/**
 * Cast a 64 bit integer to a 1 bit "boolean".
 * @param val Integer to make boolean from.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM i1 value. 1 if any bit in `expr` is 1, 0 otherwise.
 */
static inline llvm::Value *u64ToI1(llvm::Value *expr, hclang::parserContext &pc) {
    return pc.builder.CreateICmpNE(expr, integerConstant(UINT64_C(0), pc), "i64toi1");
}

/**
 * Allocate space on the stack for a local variable (1 byte).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
inline llvm::Value *generateEntryBlockAlloca(llvm::Type *type, const hclang::Identifier &id,
                                             hclang::parserContext &pc) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }

    auto nId = id.getId();
    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(type, nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable.
 * DUMMY FUNCTION. JUST RETURNS NULL.
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <typename T>
inline static llvm::Value *generateEntryBlockAlloca(const hclang::Identifier &id,
                                                    hclang::parserContext &pc) {
    return nullptr;
}

/**
 * Allocate space on the stack for a local variable (1 byte).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <>
llvm::Value *generateEntryBlockAlloca<std::int8_t>(const hclang::Identifier &id,
                                                   hclang::parserContext &pc) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }

    auto nId = id.getId();
    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(llvm::Type::getInt8Ty(pc.context), nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable (2 bytes).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <>
llvm::Value *generateEntryBlockAlloca<std::int16_t>(const hclang::Identifier &id,
                                                    hclang::parserContext &pc) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }
    auto nId = id.getId();

    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(llvm::Type::getInt16Ty(pc.context), nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable (4 bytes).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <>
llvm::Value *generateEntryBlockAlloca<std::int32_t>(const hclang::Identifier &id,
                                                    hclang::parserContext &pc) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }
    auto nId = id.getId();

    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(llvm::Type::getInt32Ty(pc.context), nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable (8 bytes).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <>
llvm::Value *generateEntryBlockAlloca<std::int64_t>(const hclang::Identifier &id,
                                                    hclang::parserContext &pc) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }
    auto nId = id.getId();

    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(llvm::Type::getInt64Ty(pc.context), nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable (8 bytes / pointer).
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
llvm::Value *generateEntryBlockAllocaPtr(const hclang::Identifier &id, hclang::parserContext &pc,
                                         hclang::typeInfo ti) {
    if (auto symbol = pc.symbolTable.find(id); symbol && symbol.isVar()) {
        return symbol.variable;
    } else if (symbol && !symbol.isVar()) {
        throw std::invalid_argument(fmt::format("id {} is a function, expected variable"));
    }
    auto nId = id.getId();

    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(&curFn->getEntryBlock(), curFn->getEntryBlock().begin());
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(llvmTypeFrom(ti, pc), nullptr, name);
    pc.symbolTable[id] = alloca;
    return alloca;
}

/**
 * Allocate space on the stack for a local variable.
 * Note: <T> must be a type for which there is a template specialization.
 * @param id Identifier for the local variable.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM value pointing to stack-allocated space.
 */
template <typename T>
static inline llvm::Value *generateEntryBlockAlloca(std::string_view id,
                                                    hclang::parserContext &pc) {
    return generateEntryBlockAlloca<T>(hclang::Identifier(id), pc.symbolTable);
}

/**
 * Create a load instruction from an Lvalue.
 * Note: This is a dummy function and always returns NULL.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
template <typename T>
static inline llvm::Value *readLvalue(std::string_view name, hclang::parserContext &pc) {
    return nullptr;
}

/**
 * Create a 1 byte load instruction from an Lvalue.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
template <>
llvm::Value *readLvalue<std::uint8_t>(std::string_view name, hclang::parserContext &pc) {
    llvm::Value *ptr = pc.symbolTable.find(hclang::Identifier(name)).variable;
    if (!ptr) {
        ptr = integerConstant(UINT32_C(0), pc);
    }
    return pc.builder.CreateLoad(llvm::Type::getInt8Ty(pc.context), ptr, name);
}

/**
 * Create a 2 byte load instruction from an Lvalue.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
template <>
llvm::Value *readLvalue<std::uint16_t>(std::string_view name, hclang::parserContext &pc) {
    llvm::Value *ptr = pc.symbolTable.find(hclang::Identifier(name)).variable;
    if (!ptr) {
        ptr = integerConstant(UINT32_C(0), pc);
    }
    return pc.builder.CreateLoad(llvm::Type::getInt16Ty(pc.context), ptr, name);
}

/**
 * Create a 4 byte load instruction from an Lvalue.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
template <>
llvm::Value *readLvalue<std::uint32_t>(std::string_view name, hclang::parserContext &pc) {
    llvm::Value *ptr = pc.symbolTable.find(hclang::Identifier(name)).variable;
    if (!ptr) {
        ptr = integerConstant(UINT32_C(0), pc);
    }
    return pc.builder.CreateLoad(llvm::Type::getInt32Ty(pc.context), ptr, name);
}

/**
 * Create a 8 byte load instruction from an Lvalue.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
template <>
llvm::Value *readLvalue<std::uint64_t>(std::string_view name, hclang::parserContext &pc) {
    llvm::Value *ptr = pc.symbolTable.find(hclang::Identifier(name)).variable;
    if (!ptr) {
        ptr = integerConstant(UINT32_C(0), pc);
    }
    return pc.builder.CreateLoad(llvm::Type::getInt64Ty(pc.context), ptr, name);
}

/**
 * Create a 8 byte load instruction from an Lvalue.
 * @param name Identifier to create a load for.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of variable `name`.
 */
llvm::Value *readLvaluePtr(std::string_view name, hclang::parserContext &pc, hclang::typeInfo ti) {
    llvm::Value *ptr = pc.symbolTable.find(hclang::Identifier(name)).variable;
    if (!ptr) {
        ptr = integerConstant(UINT32_C(0), pc);
    }
    llvm::Value *refTable[1] = {llvm::ConstantInt::get(llvm::Type::getInt64Ty(pc.context), 0)};
    ti = *ti;
    auto tmp =
        pc.builder.CreateGEP(llvmTypeFrom(ti, pc), ptr, refTable, fmt::format("{}torvalptr", name));
    tmp = pc.builder.CreateLoad(llvmTypeFrom(ti, pc), tmp, fmt::format("{}torvalderef", name));
    return pc.builder.CreateGEP(llvmTypeFrom(*ti, pc), tmp, refTable,
                                fmt::format("{}torval", name));
}

/**
 * Create a load instruction from an Lvalue.
 * @param dec Declaration object with an identifier to load.
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM LValue of `dec`.
 */
template <typename T>
static inline llvm::Value *readLvalue(hclang::decl dec, hclang::parserContext &pc) {
    return readLvalue<T>(dec->getIdRef().getId(), pc);
}

/**
 * Make a store instruction.
 * @param expr Value to store.
 * @param variable Lvalue to store into.
 * @param load The current value of `variable`. Used if `op` is not
 * Operator::Assignment
 * @see hclang::Operator
 * @param op Assignment operator.
 * @param pc Context object that contains the global LLVM objects.
 * @return Store instruction. NULL if any argument is invalid.
 */
static llvm::Value *assignment(llvm::Value *expr, llvm::Value *variable, llvm::Value *load,
                               hclang::Operator op, hclang::parserContext &pc) {
    if (!expr || !variable) {
        return nullptr;
    }

    // Create a store, return it.
    if (op == O::Assignment) {
        pc.builder.CreateStore(expr, variable, false);
        return expr;
    }

    // All other assigns depend on the current value of variable.
    if (!load) {
        return nullptr;
    }

    llvm::Value *result = nullptr;
    switch (op) {
    case O::AddAssignment:
    case O::PrefixPlusPlus: {
        result = pc.builder.CreateAdd(load, expr, "addeqtmp");
        pc.builder.CreateStore(result, variable, false);
    } break;
    case O::SubtractAssignment:
    case O::PrefixMinusMinus: {
        result = pc.builder.CreateSub(load, expr, "subeqtmp");
        pc.builder.CreateStore(result, variable, false);
    } break;
    case O::PostfixPlusPlus: {
        result = pc.builder.CreateAdd(load, expr, "addeqtmp");
        pc.builder.CreateStore(result, variable, false);
        result = load;
    } break;
    case O::PostfixMinusMinus: {
        result = pc.builder.CreateSub(load, expr, "addeqtmp");
        pc.builder.CreateStore(result, variable, false);
        result = load;
    } break;
    default:
        break;
    }

    return result;
}

/**
 * Create a binary operation instruction for a pair of LLVM expressions.
 * @param lhs Left hand side of the operation.
 * @param rhs Right hand side of the operation.
 * @param op Operation to perform. Should be a binary operator.
 * @see hclang::Operator
 * @param pc Context object that contains the global LLVM objects.
 * @param nowrap True if wrapping should be disabled, false if not.
 * @param signedOp True if the operation is signed, false if unsigned.
 * @param exact True if operation is "exact" (no remainder).
 * @param isPointerOp True if operation is some form of pointer arithmetic.
 * @return LLVM instruction generated from the operation. NULL if a parameter is
 * invalid.
 */
static llvm::Value *binaryOperation(llvm::Value *lhs, llvm::Value *rhs, hclang::Operator op,
                                    hclang::parserContext &pc, bool nowrap = false,
                                    bool signedOp = false, bool exact = false) {
    if (!lhs || !rhs) {
        return nullptr;
    }

    bool hasNUW = nowrap && !signedOp;
    bool hasNSW = nowrap && signedOp;
    switch (op) {
    case O::Multiply:
        return pc.builder.CreateMul(lhs, rhs, "multmp", hasNUW, hasNSW);
        break;
    case O::Divide:
        if (signedOp) {
            return pc.builder.CreateSDiv(lhs, rhs, "sdivtmp", exact);
        }
        return pc.builder.CreateUDiv(lhs, rhs, "udivtmp", exact);
        break;
    case O::Add:
        return pc.builder.CreateAdd(lhs, rhs, "addtmp", hasNUW, hasNSW);
        break;
    case O::Subtract:
        return pc.builder.CreateSub(lhs, rhs, "subtmp", hasNUW, hasNSW);
        break;
    case O::Modulo:
        if (signedOp) {
            return pc.builder.CreateSRem(lhs, rhs, "smodtmp");
        }
        return pc.builder.CreateURem(lhs, rhs, "umodtmp");
        break;
    case O::Leftshift:
        return pc.builder.CreateShl(lhs, rhs, "shltmp", hasNUW, hasNSW);
        break;
    case O::Rightshift:
        if (signedOp) {
            return pc.builder.CreateAShr(lhs, rhs, "ashrtmp", exact);
        }
        return pc.builder.CreateLShr(lhs, rhs, "lshrtmp", exact);
        break;
    case O::BitwiseAnd:
    case O::LogicalAnd:
        return pc.builder.CreateAnd(lhs, rhs, "bitandtmp");
        break;
    case O::BitwiseOr:
    case O::LogicalOr:
        return pc.builder.CreateOr(lhs, rhs, "bitortmp");
        break;
    case O::BitwiseXor:
        return pc.builder.CreateXor(lhs, rhs, "bitxortmp");
        break;
        // All comparisons need to be cast to a 64 bit integer.
    case O::Equals:
        return pc.builder.CreateICmpEQ(lhs, rhs, "eqtmp");
        break;
    case O::NotEquals:
        return pc.builder.CreateICmpNE(lhs, rhs, "neqtmp");
        break;
    case O::GreaterThanEqual:
        if (signedOp) {
            return pc.builder.CreateICmpSGE(lhs, rhs, "gtetmp");
        }
        return pc.builder.CreateICmpUGE(lhs, rhs, "gtetmp");
        break;
    case O::LessThanEqual:
        if (signedOp) {
            return pc.builder.CreateICmpSLE(lhs, rhs, "ltetmp");
        }
        return pc.builder.CreateICmpSLE(lhs, rhs, "ltetmp");
        break;
    case O::GreaterThan:
        if (signedOp) {
            return pc.builder.CreateICmpSGT(lhs, rhs, "gttmp");
        }
        return pc.builder.CreateICmpUGT(lhs, rhs, "gttmp");
        break;
    case O::LessThan:
        if (signedOp) {
            return pc.builder.CreateICmpSLT(lhs, rhs, "lttmp");
        }
        return pc.builder.CreateICmpSLT(lhs, rhs, "lttmp");
        break;
    default:
        break;
    }

    return nullptr;
}

/**
 * Create a binary operation instruction for a pair of LLVM expressions.
 * @param lhs Left hand side of the operation.
 * @param rhs Right hand side of the operation.
 * @param op Operation to perform. Should be a binary operator.
 * @see hclang::Operator
 * @param pc Context object that contains the global LLVM objects.
 * @param nowrap True if wrapping should be disabled, false if not.
 * @param signedOp True if the operation is signed, false if unsigned.
 * @param exact True if operation is "exact" (no remainder).
 * @param isPointerOp True if operation is some form of pointer arithmetic.
 * @return LLVM instruction generated from the operation. NULL if a parameter is
 * invalid.
 */
static llvm::Value *binaryPtrOperation(llvm::Value *lhs, llvm::Value *rhs, hclang::Operator op,
                                       hclang::parserContext &pc, hclang::typeInfo ptrTy,
                                       bool lhsIsPtr) {
    llvm::Value *index = lhsIsPtr ? rhs : lhs;
    llvm::Value *ptr = lhsIsPtr ? lhs : rhs;
    switch (op) {
    case O::Add: {
        llvm::Value *ptrTable[1] = {index};
        return pc.builder.CreateGEP(llvmTypeFrom(*ptrTy, pc), ptr, ptrTable, "ptrarith");
    } break;
    case O::Subtract: {
        auto sub = pc.builder.CreateSub(llvm::ConstantInt::get(index->getType(), 0), index);
        llvm::Value *ptrTable[2] = {llvm::ConstantInt::get(sub->getType(), 0), sub};
        return pc.builder.CreateGEP(llvmTypeFrom(ptrTy, pc), ptr, ptrTable, "ptrarith");
    } break;
    default:
        break;
    }

    return nullptr;
}

/**
 * Create a unary operation instruction for an LLVM expression.
 * @param expr Expression argument for the operator `op`.
 * @param op Operation to perform. Should be a binary operator.
 * @see hclang::Operator
 * @param pc Context object that contains the global LLVM objects.
 * @return LLVM instruction generated from the operation. NULL if a parameter is
 * invalid.
 */
static llvm::Value *unaryOperation(llvm::Value *expr, hclang::Operator op,
                                   hclang::parserContext &pc, hclang::typeInfo ti = {},
                                   bool isLValue = false) {
    if (!expr) {
        return nullptr;
    }

    switch (op) {
    case O::Negative:
        return pc.builder.CreateNeg(expr, "negatetmp");
        break;
    case O::BitwiseNot:
        return pc.builder.CreateNot(expr, "nottmp");
        break;
    case O::Positive:
        // If Positive, `expr` should be an rvalue.
        return expr;
        break;
    case O::AddressOf:
        // If AddressOf, `expr` should be an lvalue.
        return expr;
        break;
    case O::Dereference: {
        llvm::Value *ptrTable[1] = {llvm::ConstantInt::get(llvm::Type::getInt64Ty(pc.context), 0)};
        auto endTy = *ti;
        auto tmp = pc.builder.CreateGEP(llvmTypeFrom(*ti, pc), expr, ptrTable, "deferptrtmp");
        if (isLValue) {
            tmp = pc.builder.CreateLoad(llvmTypeFrom(endTy, pc), tmp, "dereftmp");
            endTy = *endTy;
        }
        return pc.builder.CreateLoad(llvmTypeFrom(endTy, pc), tmp, "dereftmp");
    } break;
    default:
        break;
    }
    return nullptr;
}

/**
 * Make an LLVM noop instruction.
 * @param pc Context object that contains the global LLVM objects.
 * @return An LLVM instruction that does nothing and has no side effects.
 */
static inline llvm::Value *noOp(hclang::parserContext &pc) {
    return binaryOperation(integerConstant(UINT32_C(0), pc), integerConstant(UINT32_C(0), pc),
                           hclang::Operator::Add, pc);
}

// ParseTree implementation.

void hclang::ParseTree::compile(const hclang::fs::path &path) const {
    // LLVM context.
    llvm::LLVMContext context;
    // Builder context.
    llvm::IRBuilder<> builder(context);
    // Main module.
    llvm::Module *module = nullptr;
    // "Main" function, contains all global code.
    llvm::Function *mainFunc = nullptr;

    module = new llvm::Module("squish", context);

    llvm::FunctionType *mainFuncPrototype =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);

    mainFunc = llvm::Function::Create(mainFuncPrototype, llvm::GlobalValue::ExternalLinkage, "main",
                                      module);

    BlockStack blockStack;
    blockStack.push("entry", context, builder, mainFunc);
    // AST->LLVM.
    SymbolTable<llvmSymbol> symbolTable;
    symbolTable.pushTable();
    parserContext pc{symbolTable, blockStack, context, builder, module};
    // Create block stack and entry block for main.
    mProgram.toLLVM(pc);

    // Insert implicit return 0 for main.
    builder.CreateRet(integerConstant(INT32_C(0), pc));

    // Verify.
    llvm::verifyModule(*module);

    std::string realPath = path.u8string();
    if (realPath.empty()) {
        realPath = "a.out";
    }

    if (mConfig.syntaxOnly()) {
        return;
    }

    // Emit LLVM to a tmp file.
    std::string llpath;
    if (mConfig.shouldEmitLLVM()) {
        llpath = fs::path(mConfig.getOutputPath()).replace_extension(".ll");
    } else {
        llpath = util::mkTmp("", ".ll").u8string();
    }
    ::log("Writing LLVM bytecode to {}\n", llpath);
    {
        std::error_code ec;
        llvm::raw_fd_ostream outStream(llpath, ec);
        module->print(outStream, nullptr);
        if (ec) {
            throw std::runtime_error(fmt::format("Could not write to {}: (error code {}:{})",
                                                 llpath, ec.value(), ec.message()));
            return;
        }
        if (mConfig.shouldEmitLLVM()) {
            return;
        }
    }

    // Invoke clang.
    if (auto pid = fork(); pid == 0) {
        ::log("Invoking clang with:\n\"");
        auto argv = mConfig.makeClangArgv({hclang::fs::path(llpath)});
        for (auto arg : argv) {
            const char *printS = arg ? arg : "";
            ::log("{} ", printS);
        }
        ::log("\"\n");
        if (execvp("clang", argv.data())) {
            // exec* only return on error
            // TODO.
        }
    } else if (pid == -1) {
        // TODO
    } else {
        int status = 0;
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            if (status != 0) {
                fmt::print(stderr, "Clang exited with status {}\n", status);
            }
        } else {
            // Terminated by signal.
            fmt::print(stderr, "Clang was terminated by signal {}\n", status);
        }
    }

    // Remove temporary file.
    if (!mConfig.shouldSaveTmps()) {
        unlink(llpath.c_str());
    }
}

hclang::LLV hclang::StringConstant::toLLVM(parserContext &pc) const {
    // TODO make sure that LLVM ensures the string is NULL terminated.
    return pc.builder.CreateGlobalStringPtr(mStr, "globalstr", 0, pc.module);
}

hclang::LLV hclang::IntegerConstant::toLLVM(parserContext &pc) const {
    switch (mType.type) {
    case hct::I8i:
        return integerConstant(static_cast<std::int8_t>(mValue), pc);
        break;
    case hct::U8i:
        return integerConstant(static_cast<std::uint8_t>(mValue), pc);
        break;
    case hct::I16i:
        return integerConstant(static_cast<std::int16_t>(mValue), pc);
        break;
    case hct::U16i:
        return integerConstant(static_cast<std::uint16_t>(mValue), pc);
        break;
    case hct::I32i:
        return integerConstant(static_cast<std::int32_t>(mValue), pc);
        break;
    case hct::U32i:
        return integerConstant(static_cast<std::uint32_t>(mValue), pc);
        break;
    case hct::I64i:
        return integerConstant(static_cast<std::int64_t>(mValue), pc);
        break;
    case hct::U64i:
        return integerConstant(mValue, pc);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::VariableDeclaration::toLLVM(parserContext &pc) const {
    using hct = hclang::HCType;
    switch (mType.type) {
    case hct::I8i:
    case hct::U8i:
        return generateEntryBlockAlloca<std::int8_t>(mId, pc);
        break;
    case hct::I16i:
    case hct::U16i:
        return generateEntryBlockAlloca<std::int16_t>(mId, pc);
        break;
    case hct::I32i:
    case hct::U32i:
        return generateEntryBlockAlloca<std::int32_t>(mId, pc);
        break;
    case hct::I64i:
    case hct::U64i:
        return generateEntryBlockAlloca<std::int64_t>(mId, pc);
        break;
    case hct::Pointer:
        return generateEntryBlockAllocaPtr(mId, pc, mType);
        break;
    case hct::Class:
        break;
    case hct::Enum:
        break;
    case hct::Union:
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::VariableInitialization::toLLVM(parserContext &pc) const {
    auto alloca = hclang::VariableDeclaration::toLLVM(pc);
    auto rhs = mRhs->toLLVM(pc);

    return assignment(rhs, alloca, nullptr, hclang::Operator::Assignment, pc);
}

hclang::LLV hclang::Program::toLLVM(parserContext &pc) const {
    for (const auto &pd : mStatements) {
        auto realPd = getPD(pd);
        if (realPd != nullptr) {
            realPd->toLLVM(pc);
        }
    }
    return nullptr;
}

hclang::LLV hclang::BinaryOperator::toLLVM(parserContext &pc) const {
    llvm::Value *result = nullptr;
    // Only LHS or RHS should be pointer.
    if (mType.isPointer()) {
        auto lhsIsPtr = mLhs->getType().isPointer();
        typeInfo ptrTy;
        if (lhsIsPtr) {
            ptrTy = mLhs->getType();
        } else {
            ptrTy = mRhs->getType();
        }
        result = binaryPtrOperation(mLhs->toLLVM(pc), mRhs->toLLVM(pc), mOp, pc, ptrTy, lhsIsPtr);
    } else {
        result = binaryOperation(mLhs->toLLVM(pc), mRhs->toLLVM(pc), mOp, pc);
    }
    // HACK: for LLVM
    // If comparison, insert a cast.
    if (isComparison(mOp)) {
        result = hclang::LLVMCast(mLhs->getType(), result).toLLVM(pc);
    }

    return result;
}

hclang::LLV hclang::UnaryOperator::toLLVM(parserContext &pc) const {
    // HACK: necessary because LLVM alloca's are pointers, but tmp values are not.
    bool isLValue = false;
    auto ty = mExpr->getType();
    if (mExpr->isLValue()) {
        ty = ty.pointerTo();
        isLValue = true;
    }
    return unaryOperation(mExpr->toLLVM(pc), mOp, pc, ty, isLValue);
}

hclang::LLV hclang::UnaryAssignment::toLLVM(parserContext &pc) const {
    auto type = mExpr->getType();

    auto load = LToRValue(mExpr).toLLVM(pc);

    auto expr = mExpr->toLLVM(pc);
    switch (type.type) {
    case hct::U8i:
    case hct::I8i:
        return assignment(integerConstant(UINT8_C(1), pc), expr, load, mOp, pc);
        break;
    case hct::U16i:
    case hct::I16i:
        return assignment(integerConstant(UINT16_C(1), pc), expr, load, mOp, pc);
        break;
    case hct::U32i:
    case hct::I32i:
        return assignment(integerConstant(UINT32_C(1), pc), expr, load, mOp, pc);
        break;
    case hct::U64i:
    case hct::I64i:
        return assignment(integerConstant(UINT64_C(1), pc), expr, load, mOp, pc);
        break;
    case hct::F64:
        return assignment(f64Constant(1.0, pc), expr, load, mOp, pc);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::DeclarationStatement::toLLVM(parserContext &pc) const {
    for (const auto &dec : mDecls) {
        dec->toLLVM(pc);
    }
    return nullptr;
}

hclang::LLV hclang::Cast::toLLVM(parserContext &pc) const {
    // TODO check expression's type.
    if (!mType.isIntrinsic() || mType.isVoid()) {
        throw std::invalid_argument(fmt::format(
            "Cast: Type to cast is of type {}, but it needs to be an intrinsic", mType));
    }
    if (auto fromType = mExpr->getType(); !fromType.isIntrinsic() || fromType.isVoid()) {
        throw std::invalid_argument(fmt::format(
            "Cast: Type to cast is of type {}, but it needs to be an intrinsic", fromType));
    }
    if (!mExpr) {
        throw std::invalid_argument("Cast: Child is null");
    }

    auto exprType = mExpr->getType();
    auto exprLLVM = mExpr->toLLVM(pc);
    if (exprType.isInteger()) {
        // int-><type>
        switch (mType.type) {
        case hct::I8i:
        case hct::U8i:
            return pc.builder.CreateIntCast(exprLLVM, llvm::Type::getInt8Ty(pc.context), true,
                                            "i8cast");
        case hct::I16i:
        case hct::U16i:
            return pc.builder.CreateIntCast(exprLLVM, llvm::Type::getInt16Ty(pc.context), true,
                                            "i16cast");
        case hct::I32i:
        case hct::U32i:
            return pc.builder.CreateIntCast(exprLLVM, llvm::Type::getInt32Ty(pc.context), true,
                                            "i32cast");
        case hct::I64i:
        case hct::U64i:
            return pc.builder.CreateIntCast(exprLLVM, llvm::Type::getInt64Ty(pc.context), true,
                                            "i64cast");
            break;
        case hct::F64:
            // int->f64
            if (exprType.isSigned()) {
                return pc.builder.CreateSIToFP(exprLLVM, llvm::Type::getDoubleTy(pc.context), "");
            }
            return pc.builder.CreateUIToFP(exprLLVM, llvm::Type::getDoubleTy(pc.context), "pcast");
            break;
        case hct::Pointer:
            return pc.builder.CreateIntToPtr(exprLLVM, llvmTypeFrom(mType, pc));
            break;
        default:
            break;
        }
    } else if (exprType.isPointer()) {
        // ptr-><type>
        switch (mType.type) {
        case hct::I8i:
        case hct::U8i:
            return pc.builder.CreatePtrToInt(exprLLVM, llvm::Type::getInt8Ty(pc.context),
                                             "ptrtoi8cast");
            break;
        case hct::I16i:
        case hct::U16i:
            return pc.builder.CreatePtrToInt(exprLLVM, llvm::Type::getInt16Ty(pc.context),
                                             "ptrtoi16cast");
            break;
        case hct::I32i:
        case hct::U32i:
            return pc.builder.CreatePtrToInt(exprLLVM, llvm::Type::getInt32Ty(pc.context),
                                             "ptrtoi32cast");
            break;
        case hct::I64i:
        case hct::U64i:
            return pc.builder.CreatePtrToInt(exprLLVM, llvm::Type::getInt64Ty(pc.context),
                                             "ptrtoi64cast");
            break;
        case hct::F64: {
            // ptr->f64
            auto p2u64 = pc.builder.CreatePtrToInt(exprLLVM, llvm::Type::getInt64Ty(pc.context),
                                                   "ptrtou64cast");
            return pc.builder.CreateUIToFP(p2u64, llvm::Type::getDoubleTy(pc.context),
                                           "ptrtof64cast");
        } break;
        case hct::Pointer:
            // Ptr (to something) to ptr (to something else)
            return pc.builder.CreateBitCast(exprLLVM, llvmTypeFrom(mType, pc), "ptrtoptrcast");
            break;
        default:
            break;
        }
    } else if (exprType.isFloat()) {
        // f64-><type>
        switch (mType.type) {
        case hct::I8i:
            return pc.builder.CreateFPToSI(exprLLVM, llvm::Type::getInt8Ty(pc.context),
                                           "fptoi8cast");
            break;
        case hct::U8i:
            return pc.builder.CreateFPToUI(exprLLVM, llvm::Type::getInt8Ty(pc.context),
                                           "fptoi8cast");
            break;
        case hct::I16i:
            return pc.builder.CreateFPToSI(exprLLVM, llvm::Type::getInt16Ty(pc.context),
                                           "fptoi16cast");
            break;
        case hct::U16i:
            return pc.builder.CreateFPToUI(exprLLVM, llvm::Type::getInt16Ty(pc.context),
                                           "fptoi16cast");
            break;
        case hct::I32i:
            return pc.builder.CreateFPToSI(exprLLVM, llvm::Type::getInt32Ty(pc.context),
                                           "fptoi32cast");
            break;
        case hct::U32i:
            return pc.builder.CreateFPToUI(exprLLVM, llvm::Type::getInt32Ty(pc.context),
                                           "fptoi32cast");
            break;
        case hct::I64i:
            return pc.builder.CreateFPToSI(exprLLVM, llvm::Type::getInt64Ty(pc.context),
                                           "fptoi64cast");
            break;
        case hct::U64i:
            return pc.builder.CreateFPToUI(exprLLVM, llvm::Type::getInt64Ty(pc.context),
                                           "fptoi64cast");
            break;
        case hct::Pointer: {
            auto ftou = pc.builder.CreateFPToUI(exprLLVM, llvm::Type::getInt64PtrTy(pc.context),
                                                "f64tou64cast");
            return pc.builder.CreateIntToPtr(ftou, llvmTypeFrom(mExpr->getType(), pc),
                                             "f64topcast");
        } break;
        default:
            break;
        }
    }
    return exprLLVM;
}

hclang::LLV hclang::DeclarationReference::toLLVM(parserContext &pc) const {
    return pc.symbolTable[getIdRef()].variable;
}

hclang::LLV hclang::LToRValue::toLLVM(parserContext &pc) const {
    // TODO struct, enum, union, etc.
    auto underlyingDecl = mDeclRef->getDeclRef();
    switch (mDeclRef->getType().type) {
    case hct::U8i:
    case hct::I8i:
        return readLvalue<std::uint8_t>(underlyingDecl, pc);
        break;
    case hct::U16i:
    case hct::I16i:
        return readLvalue<std::uint16_t>(underlyingDecl, pc);
        break;
    case hct::U32i:
    case hct::I32i:
        return readLvalue<std::uint32_t>(underlyingDecl, pc);
        break;
    case hct::U64i:
    case hct::I64i:
        return readLvalue<std::uint64_t>(underlyingDecl, pc);
        break;
    case hct::Pointer:
        return readLvaluePtr(mDeclRef->getIdRef().getId(), pc,
                             underlyingDecl->getType().pointerTo());
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::If::toLLVM(parserContext &pc) const {
    // Create the blocks that will house the if-else code.

    // Merges have to be handled manually.
    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    auto condTrue = llvm::BasicBlock::Create(pc.context, "cond_true", curFn);
    auto condFalse = llvm::BasicBlock::Create(pc.context, "cond_false", curFn);
    auto condElse = condFalse;
    auto merge = llvm::BasicBlock::Create(pc.context, "if_cont", curFn);

    pc.builder.CreateCondBr(u64ToI1(mConditional->toLLVM(pc), pc), condTrue, condFalse);

    pc.blockStack.push(condTrue, pc.builder);
    mBody->toLLVM(pc);
    pc.builder.CreateBr(merge);
    pc.blockStack.pop(pc.builder);

    for (auto &elif : mElseIfs) {
        auto cond = elif->getConditional();
        auto body = elif->getBody();

        auto condElif = llvm::BasicBlock::Create(pc.context, "cond_elif", curFn);
        auto condElifElse = llvm::BasicBlock::Create(pc.context, "cond_elif_else", curFn);

        pc.blockStack.push(condElse, pc.builder);
        pc.builder.CreateCondBr(cond->toLLVM(pc), condElif, condElifElse);
        pc.blockStack.pop(pc.builder);

        pc.blockStack.push(condElif, pc.builder);
        body->toLLVM(pc);
        pc.builder.CreateBr(merge);
        pc.blockStack.pop(pc.builder);

        condElse = condElifElse;
    }

    // Else
    pc.blockStack.push(condElse, pc.builder);
    if (mElseBody) {
        mElseBody->toLLVM(pc);
    }
    pc.builder.CreateBr(merge);
    pc.blockStack.pop(pc.builder);

    pc.blockStack.push(merge, pc.builder);
    return nullptr;
}

hclang::LLV hclang::While::toLLVM(parserContext &pc) const {
    // Create the blocks that will house the loop branches.
    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    auto whileLoop = llvm::BasicBlock::Create(pc.context, "while_loop", curFn);
    auto merge = llvm::BasicBlock::Create(pc.context, "while_cont", curFn);

    pc.builder.CreateBr(whileLoop);

    pc.blockStack.push(whileLoop, pc.builder);
    mBody->toLLVM(pc);
    pc.builder.CreateCondBr(u64ToI1(mConditional->toLLVM(pc), pc), whileLoop, merge);
    pc.blockStack.pop(pc.builder);

    pc.blockStack.push(merge, pc.builder);
    return nullptr;
}

hclang::LLV hclang::For::toLLVM(parserContext &pc) const {
    llvm::Function *curFn = pc.builder.GetInsertBlock()->getParent();
    auto forLoop = llvm::BasicBlock::Create(pc.context, "for_loop", curFn);
    auto merge = llvm::BasicBlock::Create(pc.context, "for_cont", curFn);

    for (auto &startExp : mStartExps) {
        startExp->toLLVM(pc);
    }

    pc.builder.CreateBr(forLoop);

    pc.blockStack.push(forLoop, pc.builder);
    mBody->toLLVM(pc);

    for (auto &endExp : mEndExps) {
        endExp->toLLVM(pc);
    }
    pc.builder.CreateCondBr(u64ToI1(mConditional->toLLVM(pc), pc), forLoop, merge);
    pc.blockStack.pop(pc.builder);

    pc.blockStack.push(merge, pc.builder);
    return nullptr;
}

hclang::LLV hclang::ElseIf::toLLVM(parserContext &pc) const {
    return nullptr;
}

hclang::LLV hclang::FunctionDefinition::toLLVM(parserContext &pc) const {
    mBody->toLLVM(pc);
    // TODO Insert implicit return statement.
    return nullptr;
}

hclang::LLV hclang::FunctionDeclaration::toLLVM(parserContext &pc) const {
    // Create argument list.
    std::vector<llvm::Type *> args(mArgs.size(), nullptr);
    auto mArgsIter = mArgs.begin();
    for (auto &arg : args) {
        arg = llvmTypeFrom((*(mArgsIter++))->getType(), pc);
    }
    // Create function.
    auto prototype = llvm::FunctionType::get(llvmTypeFrom(getType(), pc), args, false);
    auto func = llvm::Function::Create(prototype, llvm::GlobalValue::ExternalLinkage,
                                       llvm::Twine(getIdRef().getId()), pc.module);
    pc.symbolTable[getIdRef()] = llvmSymbol(prototype, func);
    if (mDefinition) {
        // Supply argument names and update the symbol table.
        SymTableCtx ctx(pc.symbolTable);
        mArgsIter = mArgs.begin();
        // Set up new block stack.
        BlockStack bs;
        bs.push("entry", pc.context, pc.builder, func);
        int argIndex = 0;
        for (auto &arg : func->args()) {
            argIndex++;
            auto hclangArg = (*(mArgsIter++));
            auto name = hclangArg->getIdRef();
            arg.setName(name.getId());
            if (hclangArg->getType().isPointer()) {
                func->addParamAttr(argIndex, llvm::Attribute::NoUndef);
            }
            // Make a tmp variable for loading and storing.
            auto newVar = generateEntryBlockAlloca(arg.getType(), name, pc);
            pc.symbolTable[name] = newVar;
            assignment(&arg, newVar, nullptr, hclang::Operator::Assignment, pc);
        }
        parserContext tmpPc{pc.symbolTable, bs, pc.context, pc.builder, pc.module};
        // Codegen the definition.
        mDefinition->toLLVM(tmpPc);
        // Restore "main"/global state.
        pc.blockStack.setInsertPoint(pc.builder);
    }

    return nullptr;
}

hclang::LLV hclang::CompoundStatement::toLLVM(parserContext &pc) const {
    for (const auto &statement : mStatementList) {
        statement->toLLVM(pc);
    }
    return nullptr;
}

hclang::LLV hclang::Return::toLLVM(parserContext &pc) const {
    if (mExp) {
        return pc.builder.CreateRet(mExp->toLLVM(pc));
    }

    return pc.builder.CreateRet(nullptr);
}

hclang::LLV hclang::LLVMCast::toLLVM(parserContext &pc) const {
    if (!mExpr) {
        throw std::invalid_argument("Cast: Child is null");
    }
    // TODO check expression's type.
    // If also an int, this is fine.
    // But if float, we need to convert differently.
    // Also true with pointer.
    switch (mType.type) {
    case hct::I8i:
    case hct::U8i:
        return pc.builder.CreateIntCast(mExpr, llvm::Type::getInt8Ty(pc.context), false, "casti8");
        break;
    case hct::I16i:
    case hct::U16i:
        return pc.builder.CreateIntCast(mExpr, llvm::Type::getInt16Ty(pc.context), false,
                                        "casti16");
        break;
    case hct::I32i:
    case hct::U32i:
        return pc.builder.CreateIntCast(mExpr, llvm::Type::getInt32Ty(pc.context), false,
                                        "casti32");
        break;
    case hct::I64i:
    case hct::U64i:
        return pc.builder.CreateIntCast(mExpr, llvm::Type::getInt64Ty(pc.context), false,
                                        "casti64");
        break;
    case hct::Pointer:
        return pc.builder.CreateIntToPtr(mExpr, llvmTypeFrom(mType, pc), "castitop");
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::BinaryAssignment::toLLVM(parserContext &pc) const {
    if (!mLhs || !mRhs) {
        return nullptr;
    }

    return assignment(mRhs->toLLVM(pc), mLhs->toLLVM(pc), LToRValue(mLhs).toLLVM(pc), mOp, pc);
}

hclang::LLV hclang::Goto::toLLVM(parserContext &pc) const {
    return nullptr;
}

hclang::LLV hclang::Label::toLLVM(parserContext &pc) const {
    return nullptr;
}

hclang::LLV hclang::FunctionCall::toLLVM(parserContext &pc) const {
    std::vector<llvm::Value *> args(mArgExps.size());
    std::size_t i = 0;
    for (auto &argExp : mArgExps) {
        if (argExp) {
            args[i] = argExp->toLLVM(pc);
        } else {
            args[i] = nullptr;  // TODO default args.
        }
        i++;
    }
    auto funcId = mFunc->getIdRef();
    auto [_, funcType, callee] = pc.symbolTable.find(funcId);
    // TODO callee can be NULL if its external.
    if (!funcType || !callee) {
        throw std::runtime_error(fmt::format("Function {} is not defined in symbol table", funcId));
    }

    return pc.builder.CreateCall(funcType, callee, args, mFunc->getType().isVoid() ? "" : "retval");
}
