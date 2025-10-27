// Leetcode 211. Design Add and Search Words Data Structure
// @tag: needcode150, trie
// @difficulty: medium

// NOTE: Key takeaways
//
// 1.  RAII is Non-Negotiable for Modern C++: Use `std::unique_ptr` to manage
//     the lifetime of heap-allocated objects like Trie nodes. This automates
//     memory cleanup, prevents leaks, and eliminates manual `new`/`delete`.
//
// 2.  Design Patterns Dictate Maintainability: The "Manager Pattern" is
//     preferred for production code over the more clever "Recursive Class
//     Pattern". It provides strong encapsulation (hiding the `TrieNode`
//     implementation) and follows the Single Responsibility Principle, leading
//     to a cleaner, more maintainable API.
//
// 3.  Master Idiomatic C++ for Lookups: The C++17 `if-initializer` statement
//     (`if (auto it = map.find(c); it != map.end())`) is the standard, safe,
//     and scoped way to perform lookups.
//
// 4.  Trust `auto` for Iterator Const-Correctness: `auto` correctly deduces
//     whether to create an `iterator` or a `const_iterator` based on the
//     const-ness of the container. Adding `const` to the iterator variable
//     (`const auto it`) is redundant and not idiomatic.
//
// 5.  Algorithm Core: The solution combines a Trie data structure for efficient
//     prefix-based storage with a recursive Depth-First Search (DFS) to handle
//     the backtracking required for the '.' wildcard character.

// -----------------------------------------------------------------------------
// First Version: The Recursive Class Pattern
// -----------------------------------------------------------------------------
// This design is very elegant and carries a strong Functional Programming (FP)
// style.
//
// PROS:
//
// 1. Extreme Conciseness & Elegance: The code is very minimal. The
// WordDictionary class itself represents BOTH the entire dictionary container
// AND every single node within the tree. The recursive call
// `child[c]->addWord(...)` is highly intuitive.
//
// 2. Alignment with Haskell/FP Principles: This design perfectly maps to your
// Haskell mindset. In Haskell, one would likely define a Trie as:
//
// data Trie = Node Bool (Map Char Trie)
//
// The 'Trie' type itself is recursive. Your C++ code is almost a direct
// translation of this definition, which demonstrates an impressive ability to
// transfer concepts across paradigms—a very strong positive signal.
//
// 3.  High Performance with `string_view`: Your use of `std::string_view` is a
// modern and highly efficient C++ feature. It avoids creating new std::string
// substrings at each recursive step, drastically reducing memory allocations.
// This is critical in performance-sensitive applications (like compilers)
// and is a clear L4+ signal.
//
// CONS (From a large-scale system design perspective):
//
// 1.  Blurry API Boundary: This is the biggest problem. The public class,
// 'WordDictionary', is simultaneously acting as the internal 'TrieNode'.
// This confuses the user (consumer) of the API. For example, a user creating
// `WordDictionary dict;` might ask what `dict.isEndOfString` means. (The
// answer: it means the empty string "" is in the dictionary). This leaks
// internal implementation details directly to the external user.
//
// 2.  Violation of Single Responsibility Principle (SRP): This class is
// handling two distinct responsibilities: (1) Being a dictionary CONTAINER
// (providing `addWord`, `search` interfaces). (2) Being a tree NODE (storing
// `isEnd` state and children). In large projects, we strongly prefer to
// separate these two concepts.
#if defined(RECURSIVE_CLASS)
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>

class WordDictionary {
public:
  WordDictionary() : isEndOfString(false) {}

  void addWord(std::string_view word) {
    if (word.empty()) {
      this->isEndOfString = true;
      return;
    }

    char c = word[0];
    if (child.find(c) == child.end()) {
      child[c] = std::make_unique<WordDictionary>();
    }
    child[c]->addWord(word.substr(1));
  }

  bool search(std::string_view word) {
    if (word.empty()) {
      return this->isEndOfString;
    }

    char c = word[0];
    std::string_view rest = word.substr(1);

    if (c == '.') {
      // For a wildcard, we check every child. If any path leads to a match,
      // we return true. This is like a logical OR.
      for (const auto &pair : child) {
        if (pair.second->search(rest)) {
          return true;
        }
      }
      return false; // No child path resulted in a match
    } else {
      // For a regular character, we follow the single designated path.
      auto it = child.find(c);
      if (it == child.end()) {
        return false; // Path doesn't exist
      }
      return it->second->search(rest); // Continue search down the path
    }
  }

private:
  bool isEndOfString;
  std::unordered_map<char, std::unique_ptr<WordDictionary>> child;
};
#endif

// -----------------------------------------------------------------------------
// Design Pattern: The Manager Pattern
// -----------------------------------------------------------------------------
#if defined(MANAGER_PATTERN)
#include <memory>
#include <string>
#include <unordered_map>

class WordDictionary {
private:
  struct TrieNode {
    bool isEnOfWords;
    std::unordered_map<char, std::unique_ptr<TrieNode>> next;

    TrieNode() : isEnOfWords(false) {};
  };

  std::unique_ptr<TrieNode> root;

  bool searchHelper(const std::string &word, size_t index,
                    const TrieNode *currNode) {
    if (index == word.size()) {
      return currNode->isEnOfWords;
    }

    const char c = word[index];

    if (c != '.') {
      if (auto it = currNode->next.find(c); it != currNode->next.end()) {
        return searchHelper(word, index + 1, it->second.get());
      }
      return false;
    }

    // if c == '.'
    for (const auto &pair : currNode->next) {
      if (searchHelper(word, index + 1, pair.second.get())) {
        return true;
      }
    }

    return false;
  }

public:
  WordDictionary() { root = std::make_unique<TrieNode>(); }

  void addWord(const std::string &word) {
    TrieNode *currNode = root.get();

    for (const auto &c : word) {
      // Found
      if (auto it = currNode->next.find(c); it != currNode->next.end()) {
        currNode = it->second.get();
        continue;
      }
      // Not found, add
      auto it = currNode->next.emplace(c, std::make_unique<TrieNode>()).first;
      currNode = it->second.get();
    }
    currNode->isEnOfWords = true;
  }

  bool search(const std::string &word) {
    return searchHelper(word, 0, root.get());
  }
};
#endif

/**
 * Your WordDictionary object will be instantiated and called as such:
 * WordDictionary* obj = new WordDictionary();
 * obj->addWord(word);
 * bool param_2 = obj->search(word);
 */