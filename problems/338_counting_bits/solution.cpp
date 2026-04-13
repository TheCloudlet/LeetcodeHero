// Leetcode 338. Counting Bits

#if defined(BIT_SCAN_N_LOG_N)
// Time Complexity: O(N log N)
// This is because the outer loop (bit = 1 to n, doubling) runs O(log N) times,
// and the inner loop (num = 0 to n) runs O(N) times.
#include <vector>

class Solution {
 public:
  std::vector<int> countBits(int n) {
    std::vector<int> ans(n + 1, 0);
    // For each bit position (1,2,4,...), count that bit for every number.
    for (int bit = 1; bit <= n; bit <<= 1) {
      for (int num = 0; num <= n; ++num) {
        if (num & bit) ++ans[num];
      }
    }
    return ans;
  }
};
#endif

#if defined(DP_RIGHT_SHIFT)
// Time Complexity: O(N)
class Solution {
 public:
  // Counts the number of 1 bits for all numbers from 0 to n.
  // Uses dynamic programming: for num, ans[num] = ans[num >> 1] + (num & 1).
  // This works because num >> 1 removes the least significant bit,
  // and (num & 1) adds it back if it was 1.
  std::vector<int> countBits(int n) {
    std::vector<int> ans(n + 1, 0);
    // ans[0] = 0;  // Base case: 0 has no 1 bits

    for (int num = 1; num <= n; ++num) {
      // ans[num >> 1] gives the count for the number without the LSB,
      // and (num & 1) adds 1 if the LSB of num is 1.
      ans[num] = ans[num >> 1] + (num & 1);
    }
    return ans;
  }
};
#endif

#if defined(KERNIGHAN_WHILE)
#include <vector>

class Solution {
 public:
  std::vector<int> countBits(int n) {
    std::vector<int> ans(n + 1, 0);

    for (int i = 0; i < n + 1; ++i) {
      int num = i;
      while (num) {
        ++ans[i];
        num = num & (num - 1);
      }
    }

    return ans;
  }
};
#endif

#if defined(SOLUTION_DP_KERNIGHAN)
#include <vector>

class Solution {
 public:
  std::vector<int> countBits(int n) {
    std::vector<int> ans(n + 1, 0);

    // DP state transition:
    // The number of 1s in 'i' is exactly one more than the number of 1s
    // in 'i & (i - 1)', because 'i & (i - 1)' drops the lowest set bit.
    for (int i = 1; i <= n; ++i) {
      ans[i] = ans[i & (i - 1)] + 1;
    }

    return ans;
  }
};
#endif
