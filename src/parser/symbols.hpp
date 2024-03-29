#ifndef HCLANG_SYMBOLTABLE_HPP
#define HCLANG_SYMBOLTABLE_HPP

#include <cstdint>
#include <list>
#include <unordered_map>

#include "ast.hpp"
#include "lexer/lexer.hpp"

namespace hclang {

struct identifierHashFun {
    inline std::size_t operator()(const Identifier &id) const {
        return std::hash<std::string_view>()(id.getId());
    }
};

/**
 *
 * Note: T needs to have some sort of default constructor ({} initializable).
 */
template <typename T>
class SymbolTable {
    public:
    SymbolTable() : mTableStack({}) {
    }

    ~SymbolTable() = default;

    inline void popTable() {
        mTableStack.pop_back();
    }

    inline void pushTable() {
        mTableStack.emplace_back();
    }

    inline T &pushGlobal(const Identifier &id) {
        return mTableStack.front()[id];
    }

    inline void add(const Identifier &id, T t) {
        mTableStack.back()[id] = t;
    }

    inline bool contains(std::string_view id) const {
        return contains(Identifier(id));
    }

    inline bool contains(const Identifier &id) const {
        for (const auto &table : mTableStack) {
            if (table.count(id) != 0) {
                return true;
            }
        }
        return {};
    }

    inline T find(const Identifier &id) const {
        // Find most recent object.
        for (const auto &table : mTableStack) {
            auto i = table.find(id);
            if (i != table.end()) {
                return i->second;
            }
        }
        return {};
    }

    inline T &operator[](const std::string_view &str) {
        return (*this)[Identifier(str)];
    }

    inline T &operator[](const Identifier &id) {
        // Find most recent object.
        for (auto &table : mTableStack) {
            auto i = table.find(id);
            if (i != table.end()) {
                return i->second;
            }
        }
        // If not found, push id to the stack and return the pointer.
        return mTableStack.back()[id];
    }

    private:
    std::list<std::unordered_map<Identifier, T, identifierHashFun>> mTableStack;
};

/**
 * Class that manages the symbol table context. Uses RAII to ensure
 * that the symbol table gets popped when it goes out of scope.
 */
template <typename T>
class SymTableCtx {
    public:
    SymTableCtx(SymbolTable<T> &symbolTable) : mSymbolTable(symbolTable) {
        mSymbolTable.pushTable();
    }

    ~SymTableCtx() {
        mSymbolTable.popTable();
    }

    inline SymbolTable<T> &getSymbolTable() const {
        return mSymbolTable;
    }

    protected:
    SymbolTable<T> &mSymbolTable;
};

}  // namespace hclang

#endif /* HCLANG_SYMBOLTABLE_HPP */
