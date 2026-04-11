// Leetcode 53. Maximum Subarray
// @tag: dp, divide-and-conquer, array, neetcode150, kadane-algorithm
// @difficulty: medium

// NOTE:
// The solution is called Kadane's Algorithm, which efficiently finds the
// maximum sum of any contiguous subarray in O(n) time and O(1) space.
//
// Algorithm explanation:
// - We maintain two variables: localMax (max sum ending at current position)
//   and globalMax (maximum sum seen so far across all positions)
// - At each position, we decide whether to:
//   1. Start a new subarray from current element (if current > localMax +
//   current)
//   2. Extend the existing subarray (if localMax + current > current)
// - This is equivalent to: localMax = max(current, localMax + current)
// - We update globalMax with the maximum value seen so far
//
// Example: [-2, 1, -3, 4, -1, 2, 1, -5, 4]
// The algorithm finds subarray [4, -1, 2, 1] with sum = 6
//
// Related problems (variants of Kadane's algorithm):
// - LeetCode #53. Maximum Subarray (this problem)
// - LeetCode #152. Maximum Product Subarray
// - LeetCode #918. Maximum Sum Circular Subarray
// - LeetCode #363. Max Sum of Rectangle No Larger Than K

#if defined(LEFT_FOLD_ACCUMULATOR)
#include <algorithm>
#include <cassert>
#include <vector>

class Solution {
 public:
  int maxSubArray(const std::vector<int>& nums) {
    assert(!nums.empty());

    int global_max = INT_MIN;  // IMPORTANT
    int local_max = 0;         // IMPORTANT

    for (const int num : nums) {
      local_max = std::max(local_max + num, num);
      global_max = std::max(global_max, local_max);
    }

    return global_max;
  }
};
#endif

#if defined(DP_BOTTOM_UP)
#include <algorithm>
#include <climits>
#include <vector>

class Solution {
 public:
  int maxSubArray(const std::vector<int>& nums) {
    const int n = static_cast<int>(nums.size());

    int global_max = INT_MIN;       // IMPORTANT
    std::vector<int> dp(n + 1, 0);  // IMPORTANT

    for (int i = 1; i < n + 1; ++i) {
      dp[i] = std::max(nums[i - 1], dp[i - 1] + nums[i - 1]);
      global_max = std::max(global_max, dp[i]);
    }

    return global_max;
  }
};
#endif
