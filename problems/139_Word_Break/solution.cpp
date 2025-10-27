// Leetcode 139. Word Break
// @tag: dp, neetcode150
// @difficulty: medium
//
// NOTE: This problem is very similar to (91. Decode Ways)
// The srategy is to use a recursive function with memoization to check
// if the string can be segmented into words from the dictionary.

#include <cstddef>
#include <string>
#include <vector>

class Solution {
public:
  bool wordBreak(std::string s, std::vector<std::string> &wordDict) {
    // Memoization array for optimization: once we determine that a substring
    // from index i to the end cannot be broken into valid words, we cache
    // this result to avoid redundant computation.
    // cannotSplit[i] = true means substring s[i..end] cannot be segmented
    // cannotSplit[i] = false means either it can be segmented or not yet
    // computed
    std::vector<bool> cannotSplit(s.size(), false);
    return canBreak(s, 0, wordDict, cannotSplit);
  }

private:
  bool canBreak(const std::string &s, std::size_t start,
                const std::vector<std::string> &wordDict,
                std::vector<bool> &cannotSplit) {
    if (start == s.size()) {
      return true; // Reached the end, successful split
    }
    if (cannotSplit[start]) {
      return false; // Already determined cannot split from this index
    }

    for (const auto &word : wordDict) {
      if (isPrefix(s.substr(start), word)) {
        if (canBreak(s, start + word.size(), wordDict, cannotSplit)) {
          return true; // Found a valid split
        }
      }
    }

    // No valid split found from this index
    cannotSplit[start] = true;
    return false;
  }

  bool isPrefix(const std::string &text, const std::string &prefix) {
    if (text.size() < prefix.size()) {
      return false;
    }
    // Can use test.compare or test.starts_with (C++20)
    return text.find(prefix) == 0;
  }
};
