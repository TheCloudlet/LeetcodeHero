// Leetcode 209. Minimum Size Subarray Sum

#include <algorithm>  // for std::min
#include <climits>    // for INT_MAX
#include <vector>

class Solution {
 public:
  int minSubArrayLen(int target, vector<int>& nums) {
    int left = 0;
    int sum = 0;
    int min_len = INT_MAX;

    for (int right = 0; right < static_cast<int>(nums.size()); ++right) {
      sum += nums[right];
      while (sum >= target) {
        min_len = std::min(min_len, right - left + 1);
        sum -= nums[left];
        ++left;
      }
    }

    return (min_len == INT_MAX) ? 0 : min_len;
  }
};
