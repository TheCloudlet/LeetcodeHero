// Leetcode 53. Maximum Subarray
// @tag: dp, divide-and-conquer, array, neetcode150, kadane-algorithm
// @difficulty: medium

// NOTE:
// The solution is called Kadane's Algorithm, which efficiently finds the maximum
// sum of any contiguous subarray in O(n) time and O(1) space.
//
// Algorithm explanation:
// - We maintain two variables: localMax (max sum ending at current position)
//   and globalMax (maximum sum seen so far across all positions)
// - At each position, we decide whether to:
//   1. Start a new subarray from current element (if current > localMax + current)
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

#include <algorithm>
#include <vector>

class Solution {
public:
  int maxSubArray(vector<int> &nums) {
    assert(!nums.empty());

    int globalMax = nums[0];
    int localMax = nums[0];

    for (int idx = 1; idx < nums.size(); ++idx) {
      int num = nums[idx];
      localMax = std::max(num, localMax + num);
      globalMax = std::max(globalMax, localMax);
    }

    return globalMax;
  }
};
