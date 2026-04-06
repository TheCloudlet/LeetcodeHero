// Leetcode 81. Search in Rotated Sorted Array II
#include <vector>

class Solution {
 public:
  bool search(const std::vector<int>& nums, int target) {
    if (nums.empty()) return false;

    int left = 0;
    int right = static_cast<int>(nums.size()) - 1;

    while (left <= right) {
      const int mid = left + (right - left) / 2;

      if (nums[mid] == target) return true;

      if (nums[mid] < nums[right]) {
        // right sorted
        if (nums[mid] < target && target <= nums[right]) {
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      } else if (nums[mid] > nums[right]) {
        // left sorted
        if (nums[right] <= target && target < nums[mid]) {
          right = mid - 1;
        } else {
          left = mid + 1;
        }
      } else {
        // nums[mid] == nums[right]
        --right;
      }
    }

    return false;
  }
};
