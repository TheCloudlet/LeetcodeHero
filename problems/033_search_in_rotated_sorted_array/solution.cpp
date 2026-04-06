// Leetcode 33. Search in Rotated Sorted Array
//
// Keys:
//   1. Check whether the left or right half is sorted.
//   2. Check whether `target` falls within the sorted range.
//
// Q: Why check if the right half is sorted using `nums[mid] < nums[right]`,
//    instead of the left half?
// A: Because `mid = left + (right - left) / 2` rounds down, `mid` can equal
//    `left` but never `right`. Anchoring comparisons to `right` guarantees
//    distinct indices and cleanly avoids overlap.

#include <vector>

class Solution {
 public:
  int search(const std::vector<int>& nums, int target) {
    int left = 0;
    int right = static_cast<int>(nums.size()) - 1;

    while (left <= right) {  // Note: Finding a single elem, not range
      const int mid = left + (right - left) / 2;
      if (nums[mid] == target) return mid;

      if (nums[mid] < nums[right]) {
        if (nums[mid] < target && target <= nums[right]) {  // CAUTION: `<=`
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      } else {
        if (nums[left] <= target && target < nums[mid]) {  // CAUTION: `<=`
          right = mid - 1;
        } else {
          left = mid + 1;
        }
      }
    }

    return -1;
  }
};
