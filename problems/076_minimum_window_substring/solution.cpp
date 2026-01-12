// Leetcode 76. Minimum Window Substring
// @tag: sliding-window, hash-table, two-pointers, neetcode150
// @difficulty: hard

#include <string>
#include <vector>

class Solution {
 public:
  std::string minWindow(const std::string& s, const std::string& t) {
    // Optimization: Use vector<int> instead of unordered_map for O(1) access.
    // Rename 'map' to 'dict' to avoid naming conflicts with std::map.
    // dict[c] > 0: We have a "DEBT" of character 'c' (need to find it).
    // dict[c] < 0: We have a "SURPLUS" of character 'c' (extra in window).
    std::vector<int> dict(128, 0);
    for (const char c : t) {
      ++dict[c];
    }

    int count = t.size();  // tracks the TOTAL debt remaining.

    int start = 0;
    int min_len = INT_MAX;

    int left = 0;
    int right = 0;

    while (right < static_cast<int>(s.size())) {
      const char r_ch = s[right];

      // 1. Expand Window (Right)
      // If dict[r_ch] is positive, it means this char is useful for paying off
      // debt.
      if (dict[r_ch] > 0) {
        --count;
      }

      // Always decrement frequency.
      // Positive -> Closer to 0 (Debt paid)
      // 0 or Negative -> Becomes Negative (Surplus created)
      --dict[r_ch];
      ++right;

      while (count == 0) {
        // Check if current window is the smallest so far
        if (right - left < min_len) {
          start = left;
          min_len = right - left;
        }

        const char l_ch = s[left];

        // We are removing l_ch. Check if this breaks the valid condition.
        // If dict[l_ch] == 0, it means we had EXACTLY enough.
        // Removing it puts us back in debt.
        if (dict[l_ch] == 0) {
          ++count;
        }
        ++dict[l_ch];
        ++left;
      }
    }

    return min_len == INT_MAX ? "" : s.substr(start, min_len);
  }
};
