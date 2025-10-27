// Leetcode 91. Decode Ways
// @tag: dp, string, neetcode150
// @difficulty: medium

#include <string>
#include <vector>

class Solution {
public:
  int numDecodings(const std::string &s) {
    if (s.empty()) {
      return 0;
    }
    std::vector<int> memo(s.size(), -1);
    return possibleDecodings(s, 0, memo);
  }

private:
  int possibleDecodings(const std::string &s, const std::size_t index,
                        std::vector<int> &memo) {
    if (index >= s.size()) {
      return 1;
    }

    int possibleCount = 0;
    int singleDigit = s[index] - '0';

    // If current digit is 0, no valid single digit decoding
    if (singleDigit == 0) {
      memo[index] = 0;
      return 0;
    }

    if (int savedCount = memo[index]; savedCount != -1) {
      return savedCount;
    }

    // Try single digit decoding (1-9)
    possibleCount += possibleDecodings(s, index + 1, memo);

    // Try two digit decoding (10-26)
    if (index + 1 < s.size()) {
      int twoDigit = 10 * singleDigit + (s[index + 1] - '0');
      if (twoDigit <= 26) {
        possibleCount += possibleDecodings(s, index + 2, memo);
      }
    }

    memo[index] = possibleCount;
    return possibleCount;
  }
};