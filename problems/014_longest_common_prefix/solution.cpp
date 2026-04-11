// Leetcode 14. Longest Common Prefix
// @tag: array, need-review
// @difficulty: easy

#include <string>
#include <vector>

class Solution {
 public:
  std::string longestCommonPrefix(const std::vector<std::string>& strs) {
    if (strs.empty()) return "";

    std::string ans;
    ans.reserve(strs[0].size());

    for (std::size_t i = 0; i < strs[0].size(); ++i) {
      const char ch = strs[0][i];

      for (std::size_t j = 1; j < strs.size(); ++j) {
        if (i == strs[j].size() || strs[j][i] != ch) {
          return ans;
        }
      }
      ans.push_back(ch);
    }

    return ans;
  }
};
