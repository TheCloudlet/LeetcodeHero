// Leetcode 14. Longest Common Prefix
// @tag: array, need-review
// @difficulty: easy

#include <string>
#include <vector>

class Solution {
public:
  std::string longestCommonPrefix(const std::vector<std::string> &strs) {
    if (strs.empty() || strs[0].empty()) {
      return "";
    }

    const std::string &first = strs[0];
    for (size_t charPos = 0; charPos < first.size(); ++charPos) {
      for (size_t strIdx = 1; strIdx < strs.size(); ++strIdx) {
        // Check if we've reached the end of current string
        if (charPos >= strs[strIdx].size()) {
          return first.substr(0, charPos);
        }
        // Check if characters match at current position
        if (first[charPos] != strs[strIdx][charPos]) {
          return first.substr(0, charPos);
        }
      }
    }

    return first;
  }
};
