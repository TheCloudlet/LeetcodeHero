// Leetcode 213 House Robber II
// @tag: dp, neetcode150
// @difficulty: medium

#include <algorithm> // For std::max
#include <vector>

class Solution {
public:
  int rob(std::vector<int> &nums) {
    int n = nums.size();
    if (n == 0) {
      return 0;
    }
    if (n == 1) {
      return nums[0];
    }

    int skipFirst = robLinear(nums, 1, n - 1);
    int skipLast = robLinear(nums, 0, n - 2);
    return std::max(skipFirst, skipLast);
  }

private:
  int robLinear(std::vector<int> &nums, int start, int end) {
    int prevTwo = 0; // Max money two positions back
    int prevOne = 0; // Max money one position back

    for (int i = start; i <= end; i++) {
      int current = std::max(prevTwo + nums[i], prevOne);
      prevTwo = prevOne;
      prevOne = current;
    }

    return prevOne;
  }
};