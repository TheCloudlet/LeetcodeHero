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
      const char &ch = s[right];
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
