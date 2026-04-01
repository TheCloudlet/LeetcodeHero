// Leetcode 3. Longest Substring Without Repeating Characters
// @tag: hashtalble, string, sliding-window
// @difficulty: medium
//
// Solution
// Two pointers

#if defined(SLIDING_WINDOW)
#include <algorithm>
#include <string>
#include <vector>

class Solution {
 public:
  int lengthOfLongestSubstring(const std::string& s) {
    if (s.empty()) return 0;

    int max_len = 0;
    const int len = static_cast<int>(s.size());
    std::vector<int> freq(128, 0);

    int left = 0;
    for (int right = 0; right < len; ++right) {
      ++freq[s[right]];
      while (freq[s[right]] > 1) {
        --freq[s[left]];
        ++left;
      }
      max_len = std::max(max_len, right - left + 1);
    }

    return max_len;
  }
};

// Better solution
// Can jump left directly to the next posisiton
#if defined(DP)
#include <algorithm>  // for std::min
#include <vector>

class Solution {
 public:
  int lengthOfLongestSubstring(string s) {
    if (s.size() == 0 || s.size() == 1) {
      return s.size();
    }
    int left = 0;
    int max_len = 0;
    std::vector<int> last_seen(128, -1);

    for (int right = 0; right < static_cast<int>(s.size()); ++right) {
      const char c = s[right];
      if (last_seen[c] != -1 && last_seen[c] >= left) {
        left = last_seen[c] + 1;
      }
      last_seen[c] = right;
      max_len = std::max(max_len, right - left + 1);
    }
    return max_len;
  }
};
#endif
