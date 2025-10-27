// Leetcode 15. 3Sum
// @tag: two-pointer, sort
// @difficulty: medium

#include <algorithm>
#include <vector>

class Solution {
public:
  std::vector<std::vector<int>> threeSum(std::vector<int> &nums) {
    if (nums.size() < 3) {
      return {};
    }
    std::vector<std::vector<int>> result;

    std::sort(nums.begin(), nums.end());
    for (size_t pivot = 0; pivot < nums.size() - 2; ++pivot) {
      // Skip duplicate pivot values
      if (pivot > 0 && nums[pivot] == nums[pivot - 1])
        continue;

      int target = -nums[pivot];
      int left = pivot + 1;
      int right = nums.size() - 1;
      while (left < right) {
        int sum = nums[left] + nums[right];
        if (sum == target) {
          result.push_back({nums[pivot], nums[left], nums[right]});

          // NOTE: Skip duplicates for left and right pointers
          while (left < right && nums[left] == nums[left + 1]) {
            ++left;
          }
          while (left < right && nums[right] == nums[right - 1]) {
            --right;
          }
          ++left;
          --right;
        } else if (sum < target) {
          ++left;
        } else {
          --right;
        }
      }
    }
    return result;
  }
};