// Leetcode 300. Longest Increasing Subsequence
// @tag: dp, binary-search, greedy, neetcode150
// @difficulty: medium
//
// Thinking process:
//
// 1. Brute Force Approach
// The trivial approach is to try all possible subsequences using recursion
// (pick or skip each element). This generates 2^n possible combinations.
// Time complexity: O(2^n) - exponential and unacceptable for large inputs.
//
// 2. Transform to Graph Problem
// In previous practice, we learned monotonic stack techniques for problems
// like "next greater element". We could first find next greater elements in
// O(n), then model this as a graph problem in O(n^2). This transforms the
// problem into finding the longest path in a directed acyclic graph (DAG).
// We can use topological sort to find the longest path in O(V+E).
//
// 3. Traditional DP Solution
// Use a DP array where dp[i] represents the length of the longest increasing
// subsequence ending at index i. Time complexity: O(n^2).
//
// 4. DP with binary search
// FIXME: TO BE COMPLETED...

#if defined(DP_SOLUTION)
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <limits>
#include <vector>

class Solution {
public:
  int lengthOfLIS(const std::vector<int> &nums) {
    std::size_t n = nums.size();
    assert(n >= 1 && n <= 2500);
    if (n == 1) {
      return 1;
    }

    // dp[i] indicates the length of LIS ending at index i
    // dp[i] is initialized with 1 because every element forms a LIS of length 1
    // by itself
    std::vector<int> dp(n, 1);
    int globalMaxLIS = 1; // Minimum LIS length is 1

    for (std::size_t end = 0; end < n; ++end) {
      for (std::size_t pos = 0; pos < end; ++pos) {
        if (nums[end] > nums[pos]) {
          dp[end] = std::max(dp[end], dp[pos] + 1);
        }
      }
      globalMaxLIS = std::max(globalMaxLIS, dp[end]);
    }

    // NOTE: I made an error by returning dp.back()
    return globalMaxLIS;
  }
};
#endif