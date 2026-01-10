// Leetcode 33. Search in Rotated Sorted Array
//
// Keys:
//   1. Check left or right sorted
//   2. Check in sorted range or not

#include <vector>

class Solution {
 public:
  int search(const std::vector<int>& nums, int target) {
    if (nums.empty()) {
      return -1;
    }

    int left = 0;
    int right = nums.size() - 1;
    while (left <= right) {
      const int mid = left + (right - left) / 2;

      if (target == nums[mid]) {
        return mid;
      }

      // [BUG Missing Equality Check]
      // YOUR CODE: if (nums[mid] > nums[left])
      //
      // ANALYSIS:
      // When the window shrinks to 2 elements (e.g., [3, 1]), 'left' is at index 0
      // and 'mid' is also at index 0 (integer division).
      // In this case, nums[mid] == nums[left] (3 == 3).
      // * Your condition (3 > 3) evaluates to FALSE, incorrectly forcing execution
      // into the 'else' block (logic for right-sorted), even though the left side
      // is technically valid/sorted.
      //
      // FIX: Use '>=' to handle the case where mid and left overlap.
      if (nums[mid] > nums[left]) {
        // left is sorted
        if (target >= nums[left] && target < nums[mid]) {
          right = mid - 1;
        } else {
          left = mid + 1;
        }
      } else {
        // right is sorted
        if (target > nums[mid] && target <= nums[left]) {
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      }
    }
    return -1;
  }
};
