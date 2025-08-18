// Leetcode 211. Design Add and Search Words Data Structure
// @tag: trie
// @difficulty: medium

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

/**
 * Your WordDictionary object will be instantiated and called as such:
 * WordDictionary* obj = new WordDictionary();
 * obj->addWord(word);
 * bool param_2 = obj->search(word);
 */