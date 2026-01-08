// Leetcode 746: Min Cost Climbing Stairs
// @tag: dp, neetcode150
// @difficulty: easy


#include <vector>

class Solution {
public:
  // Space complexity O(1)
  // Time complexity O(n)
  //
  // Same idea as using foldl' in Haskell: we only need to keep track of the
  // last two states. No need to keep the whole array.
  int minCostClimbingStairs(std::vector<int> &cost) {
    int prev1 = 0;
    int prev2 = 0;
    for (size_t idx = 0; idx < cost.size(); ++idx) {
      int newPrev2 = cost[idx] + std::min(prev1, prev2);
      prev1 = prev2;
      prev2 = newPrev2;
    }
    return std::min(prev1, prev2);
  }
};
