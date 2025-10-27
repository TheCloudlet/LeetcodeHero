// Leetcode 3713. Longest Balanced Substring I
// @tag: string, brute-force, leetcode-contest
// @difficulty: medium

// Brute force solution
// I did not even think of the solution during the contest

#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

class Solution {
public:
  int longestBalancedSubstring(const std::string &s) {
    auto pireltonak = s;
    int n = pireltonak.length();
    int maxLength = 0;

    // Brute force
    for (int start = 0; start < n; ++start) {
      std::unordered_map<char, int> counts;
      for (int end = start; end < n; ++end) {
        counts[pireltonak[end]]++;
        if (isBalanced(counts)) {
          maxLength = std::max(maxLength, end - start + 1);
        }
      }
    }
    return maxLength;
  }

private:
  bool isBalanced(const std::unordered_map<char, int> &counts) {
    if (counts.empty()) {
      return true;
    }

    int targetCount = counts.begin()->second;

    // Check if all other characters have the same count
    for (const auto &[_, count] : counts) {
      if (count != targetCount) {
        return false;
      }
    }
    return true;
  }
};