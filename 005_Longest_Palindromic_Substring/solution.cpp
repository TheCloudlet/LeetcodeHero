// Leetcode 005 Longest Palindromic Substring
// @tag: iteration, dp, neetcode150
// @difficulty: medium
//
// There are typically three approaches to solve this problem:
// 1. Expand Around Center - O(n^2) time and O(1) space
// 2. Dynamic Programming (2D array) - O(n^2) time and O(n^2) space
// 3. Manacher's Algorithm - O(n) time and O(n) space (more complex to
// implement)
//
// NOTE: It's unclear why this is tagged as 1D DP in neetcode150.

// Solution 1: Expand Around Center
// For each character, treat it as the center of a palindrome.
// - Check both odd-length (single center) and even-length (double center).
// - Use a helper to expand around the center and find the longest palindrome.
// - Track the start position and length of the longest palindrome.
// - Return the corresponding substring.
#if defined(SOLUTION_1)
#include <cstddef>
#include <string>

class Solution {
public:
  std::string longestPalindrome(std::string s) {
    std::size_t longestLen = 0;
    std::size_t longestStartPos = 0;
    for (std::size_t pos = 0; pos < s.size(); ++pos) {
      // odd
      auto [oddStartPos, oddLen] = expandAroundCenter(s, pos, pos);
      if (oddLen > longestLen) {
        longestLen = oddLen;
        longestStartPos = oddStartPos;
      }
      // even
      auto [evenStartPos, evenLen] = expandAroundCenter(s, pos, pos + 1);
      if (evenLen > longestLen) {
        longestLen = evenLen;
        longestStartPos = evenStartPos;
      }
    }
    return std::string(s, longestStartPos, longestLen);
  }

private:
  std::pair<std::size_t, std::size_t> expandAroundCenter(const std::string &s,
                                                         int left, int right) {
    while (left >= 0 && right < s.size() && s[left] == s[right]) {
      --left;
      ++right;
    }
    std::size_t leftPos = left + 1;
    std::size_t rightPos = right - 1;
    return {leftPos, rightPos - leftPos + 1};
  }
};
#endif

// Solution 2: Dynamic Programming (2D array)
#if defined(SOLUTION_2)
#endif

// Solution 3: Manacher's Algorithm
#if defined(SOLUTION_3)
#endif