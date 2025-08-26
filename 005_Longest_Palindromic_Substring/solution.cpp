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
      const auto [oddStartPos, oddLen] = expandAroundCenter(s, pos, pos);
      if (oddLen > longestLen) {
        longestLen = oddLen;
        longestStartPos = oddStartPos;
      }
      // even
      const auto [evenStartPos, evenLen] = expandAroundCenter(s, pos, pos + 1);
      if (evenLen > longestLen) {
        longestLen = evenLen;
        longestStartPos = evenStartPos;
      }
    }
    return std::string(s, longestStartPos, longestLen);
  }

private:
  std::pair<std::size_t, std::size_t> expandAroundCenter(const std::string &s,
                                                         std::size_t left,
                                                         std::size_t right) {
    int l = static_cast<int>(left);
    int r = static_cast<int>(right);
    while (l >= 0 && r < s.size() && s[l] == s[r]) {
      --l;
      ++r;
    }
    const std::size_t leftPos = l + 1;
    const std::size_t rightPos = r - 1;
    return {leftPos, rightPos - leftPos + 1};
  }
};
#endif

// Solution 2: Dynamic Programming (2D array)
//
// Example:
// index: 0 1 2 3 4 5
//      " a a a b b a "
//
// Step 1. Create a table
// Step 2. Diagnal self is palindromic string
//
//   | 0 1 2 3 4 5 <-- ending position
// --+---------------
// 0 | 1 2
// 1 |   1 2
// 2 |     1 0
// 3 |       1 2
// 4 |         1 0
// 5 |           1
// ^
// | starting position
//
//
// Step 3: starting looping
//    [start, end] is palindromic if (1) s[start] == s[end]
//                                   (2) [start + 1, end - 1] is palindromic
#if defined(SOLUTION_2)

#include <cstddef>
#include <vector>

class Solution {
public:
  std::string longestPalindrome(std::string s) {
    std::size_t n = s.size();
    if (n < 2) {
      return s;
    }
    std::size_t maxStartPos = 0;
    std::size_t maxLen = 1; // Initialize to 1 for single char

    // dp[i][j] stores whether s[i..j] is a palindrome
    std::vector<std::vector<bool>> dp(n, std::vector<bool>(n, false));

    for (std::size_t offset = 0; offset < n; ++offset) {
      for (std::size_t startPos = 0; startPos < n; ++startPos) {
        // Calculate current lenth
        std::size_t endPos = startPos + offset;
        if (endPos >= n) {
          continue;
        }

        if (offset == 0) {
          dp[startPos][endPos] = true;
        } else if (offset == 1) {
          dp[startPos][endPos] = (s[startPos] == s[endPos]);
        } else {
          dp[startPos][endPos] = (s[startPos] == s[endPos]) &&
                                 dp[startPos + 1][endPos - 1] == true;
        }

        if (dp[startPos][endPos] && offset + 1 > maxLen) {
          maxLen = currLen;
          maxStartPos = startPos;
        }
      }
    }
    return std::string(s, maxStartPos, maxLen);
  }
};
#endif

// Solution 3: Manacher's Algorithm
#if defined(SOLUTION_3)
#endif