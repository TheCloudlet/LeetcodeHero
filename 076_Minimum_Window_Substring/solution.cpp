// Leetcode 76. Minimum Window Substring
// @tag: sliding-window, hash-table, two-pointers, neetcode150
// @difficulty: hard

#include <cassert>
#include <climits>
#include <string>
#include <unordered_map>

class Solution {
 public:
  std::string minWindow(std::string s, std::string t) {
    if (t.empty() || s.length() < t.length()) {
      return "";
    }

    std::unordered_map<char, int> t_freq;
    for (const auto& ch : t) {
      ++t_freq[ch];
    }

    int need_unique = t_freq.size();
    int match_unique = 0;

    int min_start = 0;
    int min_length = INT_MAX;

    int left = 0;
    std::unordered_map<char, int> freq_window;

    for (int right = 0; right < s.size(); ++right) {
      const char r_char = s[right];
      ++freq_window[r_char];
      if (t_freq.count(r_char) && freq_window[r_char] == t_freq[r_char]) {
        match_unique++;
      }

      if (match_unique == need_unique) {
        // Try shrink left side
        while (!t_freq.count(s[left]) ||
               freq_window[s[left]] > t_freq[s[left]]) {
          --freq_window[s[left]];
          ++left;
        }

        // Valid state
        if (int curr_len = right - left + 1; curr_len < min_length) {
          min_start = left;
          min_length = curr_len;
        }
      }
    }

    return min_length == INT_MAX ? "" : s.substr(min_start, min_length);
  }
};