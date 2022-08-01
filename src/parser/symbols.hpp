#ifndef HCLANG_SYMBOLTABLE_HPP
#define HCLANG_SYMBOLTABLE_HPP

#include <unordered_map>
#include <list>
#include <cstdint>

#include "ast.hpp"

namespace hclang {

    struct identifierHashFun {

        inline std::size_t operator() (const Identifier &id) const {
            return std::hash<std::string_view>()(id.getId());
        }
    };

    /**
     *
     * Note: T needs to be a poniter (or at least nullable).
     */
    template<typename T>
    class SymbolTable {
    public:
        SymbolTable() : mTableStack({}) {  }
        ~SymbolTable() = default;

        inline void popTable() {
            mTableStack.pop_back();
        }
        inline void pushTable() {
            mTableStack.emplace_back();
        }

        inline void add(const Identifier &id, T t) {
            mTableStack.back()[id] = t;
        }

        inline bool contains(std::string_view id) const {
            return contains(Identifier(id));
        }

        inline bool contains(const Identifier &id) const {
            for(const auto &table : mTableStack) {
                if(table.count(id) != 0) {
                    return true;
                }
            }
            return false;
        }

        inline T find(const Identifier &id) const {
            // Find most recent object.
            for(const auto &table : mTableStack) {
                auto i = table.find(id);
                if(i != table.end()) {
                    return i->second;
                }
            }
            return nullptr;
        }

        inline T &operator[](const std::string_view &str) {
            return (*this)[Identifier(str)];
        }

        inline T &operator[](const Identifier &id) {
            // Find most recent object.
            for(auto &table : mTableStack) {
                auto i = table.find(id);
                if(i != table.end()) {
                    return i->second;
                }
            }
            // If not found, push id to the stack and return the pointer.
            return mTableStack.back()[id];
        }
    private:
        std::list<std::unordered_map<Identifier, T, identifierHashFun>> mTableStack;
    };

    template<typename T>
    class SymTableCtx {
    public:
        SymTableCtx(SymbolTable<T> &symbolTable)
            : mSymbolTable(symbolTable) {
            mSymbolTable.pushTable();
        }

        ~SymTableCtx() {
            mSymbolTable.popTable();
        }

        inline SymbolTable<T> &getSymbolTale() const {
            return mSymbolTable;
        }
    protected:
        SymbolTable<T> &mSymbolTable;
    };

}  // hclang

#endif /* HCLANG_SYMBOLTABLE_HPP */
