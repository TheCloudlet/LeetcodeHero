// Leetcode 191. Number of 1 Bits

class Solution {
 public:
  int hammingWeight(int n) { return __builtin_popcount(n); }

  int hammingWeight2(int n) {
    // Brian Kernighan Algorithm
    int cnt = 0;
    for (; n; ++cnt) {
      n &= (n - 1);  // Clear the lowest bit
    }
    return cnt;
  }
};
