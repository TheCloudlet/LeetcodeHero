// Leetcode 3. Longest Substring Without Repeating Characters
// @tag: hashtalble, string, sliding-window
// @difficulty: medium
//
// Solution
// Two pointers

#include <string>
#include <unordered_map>

class Solution {
 public:
  int lengthOfLongestSubstring(const std::string& s) {
    // If we are sure is ASCII, we can use array
    // std::array<int, 128> freq{};  // zero-initialized
    std::unordered_map<char, int> freq;
    int left = 0, maxLength = 0;

    for (int right = 0; right < s.size(); ++right) {
      const char& ch = s[right];
      freq[ch]++;

      // Shrink window until no duplicates
      while (freq[ch] > 1) {
        freq[s[left]]--;
        left++;
      }

      maxLength = std::max(maxLength, right - left + 1);
    }

    return maxLength;
  }
};

// Better solution
// Can jump left directly to the next posisiton
#if defined(better)
#include <algorithm> // for std::min
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
