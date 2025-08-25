// Leetcode 198. House Robber
// @tag: dp, neetcode150
// @difficulty: medium

#include <vector>

class Solution {
public:
  int rob(std::vector<int> &nums) {
    int prev2 = 0; // dp[i-2]
    int prev1 = 0; // dp[i-1]
    for (int num : nums) {
      int temp = prev1;
      prev1 = std::max(prev2 + num, prev1);
      prev2 = temp;
    }
    return prev1;
  }
};