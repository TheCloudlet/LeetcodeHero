// Leetcode 392. Is Subsequence
#include <cstddef>
#include <string>

class Solution {
 public:
  bool isSubsequence(std::string s, std::string t) {
    if (s.empty()) return true;
    if (t.empty()) return false;

    std::size_t idx_s = 0;
    std::size_t idx_t = 0;

    while (idx_s < s.size() && idx_t < t.size()) {
      if (t[idx_t] == s[idx_s]) {
        ++idx_s;
      }
      ++idx_t;
    }

    return idx_s == s.size();
  }
};
