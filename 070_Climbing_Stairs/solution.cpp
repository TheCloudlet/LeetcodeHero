// Leetcode 70. Climbing Stairs
// @tag: dp
// @difficulty: easy

// NOTE: There should be multiple ways to solve this problem
// 1. Top-down DP with memoization
// 2. Bottom-up DP with array
// 3. Fibonacci formula

#include <vector>

class Solution {
public:
  int climbStairs(int n) {
    if (n < 2) {
      return n;
    }
    // bottom-up DP
    std::vector<int> fib(n + 1, 0);
    fib[0] = 1;
    fib[1] = 1;
    for (int idx = 2; idx <= n; ++idx) {
      fib[idx] = fib[idx - 1] + fib[idx - 2];
    }
    return fib[n];
  }
};