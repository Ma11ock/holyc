#ifndef HCLANG_SYMBOLTABLE_HPP
#define HCLANG_SYMBOLTABLE_HPP

#include <unordered_map>
#include <stack>
#include <cstdint>

#include "ast.hpp"

namespace hclang {

    struct identifierHashFun {

        inline std::size_t operator() (const Identifier &id) const {
            return std::hash<std::string_view>()(id.getId());
        }
    };

    template<typename T>
    class SymbolTable {
    public:
        SymbolTable() : mTableStack({{}}) {  }
        ~SymbolTable() = default;

        inline void popTable() { mTableStack.pop(); }
        inline void pushTable() { mTableStack.emplace(); }

        inline void add(const Identifier &id, T t) { mTableStack.top()[id] = t; }

        inline bool contains(std::string_view id) const {
            return contains(Identifier(id));
        }

        inline bool contains(const Identifier &id) const {
            return mTableStack.top().count(id) != 0;
        }

        inline T find(const Identifier &id) const {
            auto i = mTableStack.top().find(id);
            if(i == mTableStack.top().end()) {
                return nullptr;
            }
            return i->second;
        }

        inline T &operator[](const Identifier &id) {
            return mTableStack.top()[id];
        }
    private:
        std::stack<std::unordered_map<Identifier, T, identifierHashFun>> mTableStack;
    };

}  // hclang

#endif /* HCLANG_SYMBOLTABLE_HPP */
