// Leetcode 567. Permutation in String

#include <string>
#include <vector>

class Solution {
 public:
  bool checkInclusion(std::string s1, std::string s2) {
    // s1's permutations is the substring of s2 ?
    int s1_len = static_cast<int>(s1.size());
    int s2_len = static_cast<int>(s2.size());
    if (s1_len > s2_len) return false;

    std::vector<int> freq(26, 0);
    for (const char c : s1) {
      ++freq[c - 'a'];
    }

    int missing = s1.size();

    auto AddCharToWindow = [&](const char c) {
      if (freq[c - 'a']-- > 0) --missing;
    };

    auto RemoveCharFromWindow = [&](const char c) {
      if (++freq[c - 'a'] > 0) ++missing;
    };

    int left = 0;
    for (int right = 0; right < s2_len; ++right) {
      AddCharToWindow(s2[right]);
      while (right - left + 1 > s1_len) {
        RemoveCharFromWindow(s2[left]);
        ++left;
      }
      if (missing == 0) return true;
    }

    return false;
  }
};
