// Leetcode 35. Search Insert Position
// Binary Search for Lower Bound
//
// Goal:
//   Find the index where `target` should be inserted to maintain the sorted
//   order.
//   If `target` is found in the array, return its index.
//   Otherwise, return the index where it can be inserted.
//
// Strategy:
//   - Use binary search with a left-closed, right-open interval [left, right)
//   - In each step, compare nums[mid] with target:
//       * If nums[mid] < target: the target must be on the right
//         side (exclusive), so move left = mid + 1
//       * Else: the target is on the left side (inclusive), so move right = mid
//   - Loop ends when left == right, and left is the insertion index.
//
// Time complexity: O(log n)

#include <vector>
class Solution {
public:
  int searchInsert(const std::vector<int> &nums, int target) {
    // `low` and `high` to indicate this is boundary not index
    int low = 0;
    int high = nums.size();
    int mid;
    while (low < high) {
      mid = low + (high - low) / 2;
      if (nums[mid] < target) {
        low = mid + 1;
      } else {
        high = mid;
      }
    }
    return low;
  }
};
