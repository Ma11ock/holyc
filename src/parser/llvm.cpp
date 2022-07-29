#include "parser.hpp"
#include "ast.hpp"
#include "symbols.hpp"
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

// Aliases.

using O = hclang::Operator;
using hct = hclang::HCType;

// Static globals.

static llvm::LLVMContext context;
static llvm::IRBuilder<> builder(context);
static llvm::Module *module = nullptr;
static llvm::Function *mainFunc = nullptr;

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

static llvm::Type *llvmTypeFrom(const hclang::typeInfo &ti) {
    switch(ti.type) {
    case hct::U64i:
    case hct::I64i:
        return llvm::Type::getInt64Ty(context);
        break;
    case hct::U32i:
    case hct::I32i:
        return llvm::Type::getInt32Ty(context);
        break;
    case hct::U16i:
    case hct::I16i:
        return llvm::Type::getInt16Ty(context);
        break;
    case hct::U8i:
    case hct::I8i:
        return llvm::Type::getInt8Ty(context);
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
static llvm::Value *generateEntryBlockAlloca(const hclang::Identifier &id,
                                             hclang::SymbolTable<llvm::Value*> &symbols) {
    static_assert(true, "Cannot generate alloca for type");
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int8_t>(const hclang::Identifier &id,
                                                   hclang::SymbolTable<llvm::Value*> &symbols) {
    if(auto symbol = symbols.find(id);
       symbol) {
        return symbol;
    }

    auto nId = id.getId();
    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt8Ty(context), nullptr, name
        );
    symbols[id] = alloca;
    return alloca;
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int16_t>(const hclang::Identifier &id,
                                                    hclang::SymbolTable<llvm::Value*> &symbols) {
    if(auto symbol = symbols.find(id);
       symbol) {
        return symbol;
    }
    auto nId = id.getId();

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt16Ty(context), nullptr, name
        );
    symbols[id] = alloca;
    return alloca;
}


template<>
llvm::Value *generateEntryBlockAlloca<std::int32_t>(const hclang::Identifier &id,
                                                    hclang::SymbolTable<llvm::Value*> &symbols) {
    if(auto symbol = symbols.find(id);
       symbol) {
        return symbol;
    }
    auto nId = id.getId();

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt32Ty(context), nullptr, name
        );
    symbols[id] = alloca;
    return alloca;
}

template<>
llvm::Value *generateEntryBlockAlloca<std::int64_t>(const hclang::Identifier &id,
                                                    hclang::SymbolTable<llvm::Value*> &symbols) {
    if(auto symbol = symbols.find(id);
       symbol) {
        return symbol;
    }
    auto nId = id.getId();

    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> tmpBuilder(
        &curFn->getEntryBlock(),
        curFn->getEntryBlock().begin()
        );
    llvm::StringRef name(nId.data(), nId.size());
    auto alloca = tmpBuilder.CreateAlloca(
        llvm::Type::getInt64Ty(context), nullptr, name
        );
    symbols[id] = alloca;
    return alloca;
}

template<typename T>
static inline llvm::Value *generateEntryBlockAlloca(std::string_view id,
                                                    hclang::SymbolTable<llvm::Value*> &symbols) {
    return generateEntryBlockAlloca<T>(hclang::Identifier(id), symbols);
}

template<typename T>
static inline llvm::Value *readLvalue(std::string_view name,
                                      hclang::SymbolTable<llvm::Value*> &symbols) {
    static_assert(true, "Cannot read Lvalue for type");
}

template<>
llvm::Value *readLvalue<std::uint8_t>(std::string_view name,
                                      hclang::SymbolTable<llvm::Value*> &symbols) {
    llvm::Value *ptr = symbols.find(hclang::Identifier(name));
    if(!ptr) {
        ptr = integerConstant(UINT32_C(0));
    }
    return builder.CreateLoad(
        llvm::Type::getInt8Ty(context),
        ptr,
        name
        );
}

template<>
llvm::Value *readLvalue<std::uint16_t>(std::string_view name,
                                       hclang::SymbolTable<llvm::Value*> &symbols) {
    llvm::Value *ptr = symbols.find(hclang::Identifier(name));
    if(!ptr) {
        ptr = integerConstant(UINT32_C(0));
    }
    return builder.CreateLoad(
        llvm::Type::getInt16Ty(context),
        ptr,
        name
        );
}

template<>
llvm::Value *readLvalue<std::uint32_t>(std::string_view name,
                                       hclang::SymbolTable<llvm::Value*> &symbols) {
    llvm::Value *ptr = symbols.find(hclang::Identifier(name));
    if(!ptr) {
        ptr = integerConstant(UINT32_C(0));
    }
    return builder.CreateLoad(
        llvm::Type::getInt32Ty(context),
        ptr,
        name
        );
}

template<>
llvm::Value *readLvalue<std::uint64_t>(std::string_view name,
                                       hclang::SymbolTable<llvm::Value*> &symbols) {
    llvm::Value *ptr = symbols.find(hclang::Identifier(name));
    if(!ptr) {
        ptr = integerConstant(UINT32_C(0));
    }
    return builder.CreateLoad(
        llvm::Type::getInt64Ty(context),
        ptr,
        name
        );
}

template<typename T>
static inline llvm::Value *readLvalue(hclang::decl dec,
                                      hclang::SymbolTable<llvm::Value*> &symbols) {
    return readLvalue<T>(dec->getIdRef().getId(), symbols);
}


static llvm::Value *unaryOperation(llvm::Value *expr, hclang::Operator op) {
    if(!expr) {
        return nullptr;
    }

    switch(op) {
    case O::Negative:
        return builder.CreateNeg(expr, "negatetmp");
        break;
    case O::BitwiseNot:
        return builder.CreateNot(expr, "nottmp");
        break;
    case O::Positive:
        return expr;
        break;
    default:
        break;
    }

    return nullptr;
}

static llvm::Value *assignment(llvm::Value *expr, llvm::Value *variable,
                               hclang::Operator op, const hclang::typeInfo &ti = {}) {
    if(!expr || !variable) {
        return nullptr;
    }

    // Create a store, return it.
    if(op == O::Assignment) {
        return builder.CreateStore(expr, variable, false);
    }

    // All other assigns depend on the current value of variable.
    auto val = builder.CreateLoad(llvmTypeFrom(ti), variable, false, "assigntmp");
    llvm::Value *result = nullptr;
    switch(op) {
    case O::AddAssignment:
    {
        result = builder.CreateAdd(val, expr, "addeqtmp");
        builder.CreateStore(result, variable, false);
    }
        break;
    default:
        break;
    }

    return result;
}

static llvm::Value *assignment(llvm::Value *expr, const hclang::Identifier &id,
                               hclang::Operator op, hclang::SymbolTable<llvm::Value*> &symbols) {
    auto to = symbols.find(id);
    if(!to) {
        return nullptr;
    }
    return assignment(expr, to, op);
}

/**
 * Create a binary operation between two LLVM values.
 * @param lhs Left hand side of the operation.
 * @param rhs Right hand side of the operation.
 * @param op Operation to perform. Should be a binary operator.
 * @see hclang::Operator
 * @param nowrap True if wrapping should be disabled, false if not.
 * @param signedOp True if the operation is signed, false if unsigned.
 * @param exact True if operation is "exact" (no remainder).
 * @return LLVM value generated from the operation.
 */
static llvm::Value *binaryOperation(llvm::Value *lhs, llvm::Value *rhs,
                                    hclang::Operator op, bool nowrap = false,
                                    bool signedOp = false, bool exact = false) {
    if(!lhs || !rhs) {
        return nullptr;
    }

    bool hasNUW = nowrap && !signedOp;
    bool hasNSW = nowrap && signedOp;
    switch(op) {
    case O::Multiply:
        return builder.CreateMul(lhs, rhs, "multmp", hasNUW, hasNSW);
        break;
    case O::Divide:
        if(signedOp) {
            return builder.CreateSDiv(lhs, rhs, "sdivtmp", exact);
        }
        return builder.CreateUDiv(lhs, rhs, "udivtmp", exact);
        break;
    case O::Add:
        return builder.CreateAdd(lhs, rhs, "addtmp", hasNUW, hasNSW);
        break;
    case O::Subtract:
        return builder.CreateSub(lhs, rhs, "subtmp", hasNUW, hasNSW);
        break;
    case O::Modulo:
        if(signedOp) {
            return builder.CreateSRem(lhs, rhs, "smodtmp");
        }
        return builder.CreateURem(lhs, rhs, "umodtmp");
        break;
    case O::Leftshift:
        return builder.CreateShl(lhs, rhs, "shltmp", hasNUW, hasNSW);
        break;
    case O::Rightshift:
        if(signedOp) {
            return builder.CreateAShr(lhs, rhs, "ashrtmp", exact);
        }
        return builder.CreateLShr(lhs, rhs, "lshrtmp", exact);
        break;
    case O::BitwiseAnd:
        return builder.CreateAnd(lhs, rhs, "bitandtmp");
        break;
    case O::BitwiseOr:
        return builder.CreateOr(lhs, rhs, "bitortmp");
        break;
    case O::BitwiseXor:
        return builder.CreateXor(lhs, rhs, "bitxortmp");
        break;
    case O::NotEquals:
        return builder.CreateICmpNE(lhs, rhs, "neqtmp");
        break;
    default:
        break;
    }

    return nullptr;
}

static inline llvm::Value *noOp() {
    return binaryOperation(integerConstant(UINT32_C(0)), integerConstant(UINT32_C(0)),
                           hclang::Operator::Add);
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
    SymbolTable<llvm::Value*> symbolTable;
    symbolTable.pushTable();
    parserContext pc { symbolTable };
    mProgram.toLLVM(pc);

    // Insert implicit return 0 for main.
    builder.CreateRet(integerConstant(INT32_C(0)));

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

hclang::LLV hclang::IntegerConstant::toLLVM(parserContext &pc) const {
    switch(mType.type) {
    case hct::I8i:
        return integerConstant(static_cast<std::int8_t>(mValue));
        break;
    case hct::U8i:
        return integerConstant(static_cast<std::uint8_t>(mValue));
        break;
    case hct::I16i:
        return integerConstant(static_cast<std::int16_t>(mValue));
        break;
    case hct::U16i:
        return integerConstant(static_cast<std::uint16_t>(mValue));
        break;
    case hct::I32i:
        return integerConstant(static_cast<std::int32_t>(mValue));
        break;
    case hct::U32i:
        return integerConstant(static_cast<std::uint32_t>(mValue));
        break;
    case hct::I64i:
        return integerConstant(static_cast<std::int64_t>(mValue));
        break;
    case hct::U64i:
        return integerConstant(mValue);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::VariableDeclaration::toLLVM(parserContext &pc) const {
    using hct = hclang::HCType;
    switch(mType.type) {
    case hct::I8i:
    case hct::U8i:
        return generateEntryBlockAlloca<std::int8_t>(mId, pc.symbolTable);
        break;
    case hct::I16i:
    case hct::U16i:
        return generateEntryBlockAlloca<std::int16_t>(mId, pc.symbolTable);
        break;
    case hct::I32i:
    case hct::U32i:
        return generateEntryBlockAlloca<std::int32_t>(mId, pc.symbolTable);
        break;
    case hct::I64i:
    case hct::U64i:
        return generateEntryBlockAlloca<std::int64_t>(mId, pc.symbolTable);
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

    return assignment(rhs, alloca, hclang::Operator::Assignment);
}

hclang::LLV hclang::Program::toLLVM(parserContext &pc) const {
    for(const auto &pd : mStatements) {
        auto realPd = getPD(pd);
        if(realPd != nullptr) {
            realPd->toLLVM(pc);
        }
    }
    return nullptr;
}

hclang::LLV hclang::BinaryOperator::toLLVM(parserContext &pc) const {
    if(isAssignment()) {
        return assignment(mRhs->toLLVM(pc), mLhs->toLLVM(pc), mOp, mLhs->getType());
    }
    return binaryOperation(mLhs->toLLVM(pc), mRhs->toLLVM(pc), mOp);
}

hclang::LLV hclang::UnaryOperator::toLLVM(parserContext &pc) const {
    return unaryOperation(mExpr->toLLVM(pc), mOp);
}

hclang::LLV hclang::DeclarationStatement::toLLVM(parserContext &pc) const {
    for(const auto &dec : mDecls) {
        dec->toLLVM(pc);
    }
    return nullptr;
}

hclang::LLV hclang::Cast::toLLVM(parserContext &pc) const {
    // TODO check expression's type.
    if(!mType.isIntrinsic() || mType.isVoid()) {
        throw std::invalid_argument(fmt::format("Cast: Type to cast is of type {}, but it needs to be an intrinsic",
                                                mType));
    }
    if(auto fromType = mExpr->getType();
       !fromType.isIntrinsic() || fromType.isVoid()) {
        throw std::invalid_argument(fmt::format("Cast: Type to cast is of type {}, but it needs to be an intrinsic",
                                                fromType));
    }
    if(!mExpr) {
        throw std::invalid_argument("Cast: Child is null");
    }
    switch(mType.type) {
    case hct::I8i:
    case hct::U8i:
        return builder.CreateIntCast(mExpr->toLLVM(pc),
                                     llvm::Type::getInt8Ty(context), true);
    case hct::I16i:
    case hct::U16i:
        return builder.CreateIntCast(mExpr->toLLVM(pc),
                                     llvm::Type::getInt16Ty(context), true);
    case hct::I32i:
    case hct::U32i:
        return builder.CreateIntCast(mExpr->toLLVM(pc),
                                     llvm::Type::getInt32Ty(context), true);
    case hct::I64i:
    case hct::U64i:
        return builder.CreateIntCast(mExpr->toLLVM(pc),
                                     llvm::Type::getInt64Ty(context), true);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::DeclarationReference::toLLVM(parserContext &pc) const {
    return pc.symbolTable[getIdRef()];
}

hclang::LLV hclang::LToRValue::toLLVM(parserContext &pc) const {
    // TODO struct, enum, union, etc.
    auto underlyingDecl = mDeclRef->getDeclRef();
    switch(mDeclRef->getType().type) {
    case hct::U8i:
    case hct::I8i:
        return readLvalue<std::uint8_t>(underlyingDecl, pc.symbolTable);
        break;
    case hct::U16i:
    case hct::I16i:
        return readLvalue<std::uint16_t>(underlyingDecl, pc.symbolTable);
        break;
    case hct::U32i:
    case hct::I32i:
        return readLvalue<std::uint32_t>(underlyingDecl, pc.symbolTable);
        break;
    case hct::U64i:
    case hct::I64i:
        return readLvalue<std::uint64_t>(underlyingDecl, pc.symbolTable);
        break;
    default:
        break;
    }
    return nullptr;
}

hclang::LLV hclang::If::toLLVM(parserContext &pc) const {
    // Create the blocks that will house the if-else code.
    llvm::Function *curFn = builder.GetInsertBlock()->getParent();
    auto condTrue = llvm::BasicBlock::Create(context, "cond_true", curFn);
    auto condFalse = llvm::BasicBlock::Create(context, "cond_false", curFn);
    auto condElse = condFalse;
    auto merge = llvm::BasicBlock::Create(context, "ifcont", curFn);

    builder.CreateCondBr(mConditional->toLLVM(pc), condTrue, condFalse);

    blockStack.push(condTrue);
    mBody->toLLVM(pc);
    builder.CreateBr(merge);
    blockStack.pop();

    for(auto &elif : mElseIfs) {
        auto cond = elif->getConditional();
        auto body = elif->getBody();

        auto condElif = llvm::BasicBlock::Create(context, "cond_elif", curFn);
        auto condElifElse = llvm::BasicBlock::Create(context, "cond_elif_else", curFn);

        blockStack.push(condElse);
        builder.CreateCondBr(cond->toLLVM(pc), condElif, condElifElse);
        blockStack.pop();

        blockStack.push(condElif);
        body->toLLVM(pc);
        builder.CreateBr(merge);
        blockStack.pop();

        condElse = condElifElse;
    }

    // Else
    blockStack.push(condElse);
    if(mElseBody) {
        mElseBody->toLLVM(pc);
    }
    builder.CreateBr(merge);
    blockStack.pop();

    blockStack.push(merge);
    return nullptr;
}

hclang::LLV hclang::ElseIf::toLLVM(parserContext &pc) const {
    return nullptr;
}


hclang::LLV hclang::FunctionDefinition::toLLVM(parserContext &pc) const {
    return nullptr;
}

hclang::LLV hclang::FunctionDeclaration::toLLVM(parserContext &pc) const {
    return nullptr;
}

hclang::LLV hclang::CompoundStatement::toLLVM(parserContext &pc) const {
    pc.symbolTable.pushTable();
    for(const auto &statement : mStatementList) {
        statement->toLLVM(pc);
    }
    pc.symbolTable.popTable();
    return nullptr;
}

hclang::LLV hclang::Return::toLLVM(parserContext &pc) const {
    if(mExp) {
        return builder.CreateRet(mExp->toLLVM(pc));
    }

    return builder.CreateRet(nullptr);
}
