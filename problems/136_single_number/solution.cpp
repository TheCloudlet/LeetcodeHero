// Leetcode 136. Single Number

// It can be a simple for-loop with a accumulator, or just use std::accumulate

#include <numeric>
#include <vector>

class Solution {
 public:
  int singleNumber(const std::vector<int>& nums) {
    return std::accumulate(nums.begin(), nums.end(), 0,
                           [](int acc, int num) { return acc ^ num; });
  }
};
