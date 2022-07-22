#include "parser.hpp"
#include "ast.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>

#include <string_view>
#include <map>

// Aliases.

using O = hclang::Operator;

// Static globals.

static llvm::LLVMContext context;
static llvm::IRBuilder<> builder(context);
static llvm::Module *module = nullptr;
static llvm::Function *mainFunc = nullptr;
static std::map<std::string_view, llvm::Value*> symbols;

class BlockStack {
protected:
    // The entry block.
    llvm::BasicBlock *mEntryBlock = nullptr;
    // Stack of blocks.
    std::stack<llvm::BasicBlock*> mBlockStack;
    // Merge stack for breaks in inside while loops.
    std::stack<llvm::BasicBlock*> mMergeStack;

public:
    BlockStack() = default;

    void init() {
        // Make and push the entry block.
        mEntryBlock = llvm::BasicBlock::Create(context, "entry", mainFunc);
        mBlockStack.push(mEntryBlock);
        builder.SetInsertPoint(mEntryBlock);
    }

    void pop() {
        // Do not allow entry to popped and ignore if stack is empty.
        if(mBlockStack.size() <= 1)
            return;
        mBlockStack.pop();
        builder.SetInsertPoint(peek());
    }

    llvm::BasicBlock *peek() const {
        return mBlockStack.top();
    }

    llvm::BasicBlock *peekMerge() const {
        return mMergeStack.top();
    }

    void pushMerge(llvm::BasicBlock *merge) {
        mMergeStack.push(merge);
    }

    void clearMergeStack() {
        while(mMergeStack.size() != 0) {
            mMergeStack.pop();
        }
    }

    void popMergeStack() {
        if(mMergeStack.size() == 0)
            return;
        mMergeStack.pop();
    }

    llvm::BasicBlock *getEntry() const {
        return mEntryBlock;
    }

    void push(llvm::BasicBlock *block) {
        mBlockStack.push(block);
        builder.SetInsertPoint(block);
    }

    virtual ~BlockStack() = default;
};

static BlockStack blockStack;

// Static functions.

static llvm::Value *binaryOperation(llvm::Value *lhs, llvm::Value *rhs,
                                    hclang::Operator op) {
    if(!lhs || !rhs) {
        return nullptr;
    }

    switch(op) {
    case O::Multiply:
        return builder.CreateMul(lhs, rhs, "multmp");
        break;
    case O::Divide:
        return builder.CreateSDiv(lhs, rhs, "multmp");
        break;
    case O::Add:
        return builder.CreateAdd(lhs, rhs, "addtmp");
        break;
    case O::Subtract:
        return builder.CreateSub(lhs, rhs, "subtmp");
        break;
    default:
        break;
    }

    return nullptr;
}

static inline llvm::Value *f64Constant(double val) {
    return llvm::ConstantFP::get(context, llvm::APFloat(val));
}

static inline llvm::Value *integerConstant(std::uint8_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(8, val, false));
}

static inline llvm::Value *integerConstant(std::int8_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(8, val, true));
}

static inline llvm::Value *integerConstant(std::uint16_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(16, val, false));
}

static inline llvm::Value *integerConstant(std::int16_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(16, val, true));
}

static inline llvm::Value *integerConstant(std::uint32_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, val, false));
}

static inline llvm::Value *integerConstant(std::int32_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, val, true));
}

static inline llvm::Value *integerConstant(std::uint64_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(64, val, false));
}

static inline llvm::Value *integerConstant(std::int64_t val) {
    return llvm::ConstantInt::get(context, llvm::APInt(64, val, true));
}

static inline llvm::Value *makePlus(llvm::Value *lhs, llvm::Value *rhs) {
    return builder.CreateFAdd(lhs, rhs, "addtmp");
}

template<typename T>
static llvm::Value *generateEntryBlockAlloca(const hclang::Identifier &id) {
    return nullptr;
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int8_t>(const hclang::Identifier &id) {
    auto nId = id.getId();
    if(symbols.count(nId)) {
        return symbols[nId];
    }

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt8Ty(context), nullptr, name
        );
    symbols[nId] = alloca;
    return alloca;
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int16_t>(const hclang::Identifier &id) {
    auto nId = id.getId();
    if(symbols.count(nId)) {
        return symbols[nId];
    }

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt16Ty(context), nullptr, name
        );
    symbols[nId] = alloca;
    return alloca;
}


template<>
llvm::Value *generateEntryBlockAlloca<std::int32_t>(const hclang::Identifier &id) {
    auto nId = id.getId();
    if(symbols.count(nId)) {
        return symbols[nId];
    }

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt32Ty(context), nullptr, name
        );
    symbols[nId] = alloca;
    return alloca;
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int64_t>(const hclang::Identifier &id) {
    auto nId = id.getId();
    if(symbols.count(nId)) {
        return symbols[nId];
    }

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt64Ty(context), nullptr, name
        );
    symbols[nId] = alloca;
    return alloca;
}


static inline llvm::Value *variableValue(const std::string &name) {
    llvm::Value *ptr = nullptr;
    if(symbols.count(name)) {
        ptr = symbols[name];
    } else {
        ptr = integerConstant(0);
    }

    return builder.CreateLoad(
        llvm::Type::getInt32Ty(context),
        ptr,
        name.c_str()
        );
}

// ParseTree implementation.

void hclang::ParseTree::compile(const hclang::fs::path &path) const {
    module = new llvm::Module("squish", context);

    llvm::FunctionType *mainFuncPrototype = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(context), false
        );

    mainFunc = llvm::Function::Create(mainFuncPrototype, llvm::GlobalValue::ExternalLinkage,
                                      "hclang_main", module);
    blockStack.init();


    // AST->LLVM.
    mProgram.toLLVM();

    auto retBlock = llvm::BasicBlock::Create(context, "ret", mainFunc);
    blockStack.push(retBlock);
    if(symbols.count("result")) {
        builder.CreateRet(variableValue("result"));
    } else {
        builder.CreateRet(integerConstant(0));
    }
    blockStack.pop();
    builder.CreateBr(retBlock);

    // Verify.
    llvm::verifyModule(*module);

    std::string realPath = path.u8string();
    if(realPath.empty()) {
        realPath = "a.out";
    }

    if(mConfig.shouldEmitLLVM()) {
        module->print(llvm::outs(), nullptr);
        return;
    }

    if(mConfig.syntaxOnly()) {
        return;
    }

    // Compile to object code.
    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    auto targetReg = llvm::TargetRegistry::lookupTarget(targetTriple, error);

    if(!targetReg) {
        throw std::runtime_error(error);
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = targetReg->createTargetMachine(targetTriple, cpu, features,
                                                        opt, rm);


    module->setDataLayout(targetMachine->createDataLayout());
    module->setTargetTriple(targetTriple);

    std::error_code ec;
    llvm::raw_fd_ostream dest(realPath, ec, llvm::sys::fs::OF_None);

    if(ec) {
        throw std::runtime_error(fmt::format("Could not open output file: {}", ec.message()));
    }

    llvm::legacy::PassManager pass;
    auto fileType = llvm::CGFT_ObjectFile;

    if(targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
        throw std::runtime_error("TargetMachine cannot emit a file of this type.");
    }

    pass.run(*module);
    dest.flush();
}

hclang::LLV hclang::IntegerConstant::toLLVM() const {
    if(mIsSigned) {
        return integerConstant(static_cast<std::int64_t>(mValue));
    }
    return integerConstant(mValue);
}

hclang::LLV hclang::VariableDeclaration::toLLVM() const {
    using hct = hclang::HCType;
    switch(mType.type) {
    case hct::I8i:
    case hct::U8i:
        return generateEntryBlockAlloca<std::int8_t>(mId);
        break;
    case hct::I16i:
    case hct::U16i:
        return generateEntryBlockAlloca<std::int16_t>(mId);
        break;
    case hct::I32i:
    case hct::U32i:
        return generateEntryBlockAlloca<std::int32_t>(mId);
        break;
    case hct::I64i:
    case hct::U64i:
        return generateEntryBlockAlloca<std::int64_t>(mId);
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

hclang::LLV hclang::VariableInitialization::toLLVM() const {
    auto alloca = hclang::VariableDeclaration::toLLVM();
    auto rhs = mRhs->toLLVM();

    // TODo
    return builder.CreateStore(rhs, alloca);
}

hclang::LLV hclang::Program::toLLVM() const {
    for(const auto &pd : mStatements) {
        if(pd != nullptr) {
            pd->toLLVM();
        }
    }
    return nullptr;
}

hclang::LLV hclang::BinaryOperator::toLLVM() const {
    return binaryOperation(mLhs->toLLVM(), mRhs->toLLVM(), mOp);
}

hclang::LLV hclang::DeclarationStatement::toLLVM() const {
    for(const auto &dec : mDecls) {
        dec->toLLVM();
    }
    return nullptr;
}

hclang::LLV hclang::Cast::toLLVM() const {
    // TODO check expression's type.
    if(hclang::isInteger(mIntoType)) {
        return builder.CreateIntCast(mExpr->toLLVM(), llvm::Type::getInt32Ty(context), true);
    }
    return nullptr;
}
