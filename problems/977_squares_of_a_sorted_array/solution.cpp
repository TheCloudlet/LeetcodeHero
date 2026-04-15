// Leetcode 977. Squares of a Sorted Array

#if defined(INSIDE_OUT)
// Steps:
// 1. Find zero
// 2. Two pointers insert
//
// But we could out-side in

#include <algorithm>
#include <iterator>
#include <vector>

class Solution {
 public:
  std::vector<int> sortedSquares(const std::vector<int>& nums) {
    const int n = static_cast<int>(nums.size());

    std::vector<int> ans;
    ans.reserve(n);

    auto it = std::lower_bound(nums.begin(), nums.end(), 0);

    int right = std::distance(nums.begin(), it);
    int left = right - 1;

    while (left >= 0 && right < n) {
      // CAUTION: -10^4 <= nums[i] <= 10^4
      // If not, INT_MIX * (-1) will underflow. We need to cast to int64_t
      const int abs_l = -1 * nums[left];
      const int abs_r = nums[right];
      if (abs_l < abs_r) {
        ans.push_back(nums[left] * nums[left]);
        --left;
      } else {
        ans.push_back(nums[right] * nums[right]);
        ++right;
      }
    }

    while (left >= 0) {
      ans.push_back(nums[left] * nums[left]);
      --left;
    }

    while (right < n) {
      ans.push_back(nums[right] * nums[right]);
      ++right;
    }

    return ans;
  }
};
#endif
