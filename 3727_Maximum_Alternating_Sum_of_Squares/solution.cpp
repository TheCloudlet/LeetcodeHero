// Leetcode 3727. Maximum Alternating Sum of Squares
// @tag: greedy, sorting, weekly-contest
// @difficulty: medium
// @contest-spent-time: 16 mins 37 secs

// Thinking:
// This is a greedy problem
// Sort by abs(nums) in descending order, then get first
// `nums.size() - nums.size() / 2` elements to add, others to subtract

#include <algorithm>
#include <cmath>
#include <vector>

class Solution {
 public:
  long long maxAlternatingSum(vector<int>& nums) {
    std::vector<int> sorted_by_abs_value = nums;

    auto compare_by_abs_value = [](int a, int b) {
      return std::abs(a) > std::abs(b);
    };
    std::sort(sorted_by_abs_value.begin(), sorted_by_abs_value.end(),
              compare_by_abs_value);

    long long total_sum = 0;
    int half_size = nums.size() / 2;
    int add_threshold = nums.size() - half_size;

    for (int idx = 0; idx < nums.size(); idx++) {
      int squared_value = sorted_by_abs_value[idx] * sorted_by_abs_value[idx];
      if (idx < add_threshold) {
        total_sum += squared_value;
      } else {
        total_sum -= squared_value;
      }
    }

    return total_sum;
  }
};