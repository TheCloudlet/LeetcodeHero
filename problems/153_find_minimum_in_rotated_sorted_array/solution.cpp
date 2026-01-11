// Leetcode 153. Find Minimum in Rotated Sorted Array

// First attempt:
// Although this solution is correct, but it will create a lot of confusion
// during implementation. Also, it is error prone.
//
// Think why it is error prone?

#if defined(try1)
#include <vector>

class Solution {
 public:
  int findMin(std::vector<int>& nums) {
    if (nums.empty()) {
      return -1;
    }
    int left = 0;
    int right = nums.size() - 1;

    while (left <= right) {
      const int mid = left + (right - left) / 2;

      if (nums[mid] >= nums[left] && nums[mid] <= nums[right]) {
        return nums[left];
      } else if (nums[mid] >= nums[left]) {
        left = mid + 1;
      } else {
        right = mid;
      }
    }
    return -1;
  }
};
#endif

// The better way to think this question is:
//
// 1. Q: Are we able to use mid to get directly to the answer?
//    - Yes. Find target. `while (left <= right)`
//    - No. Find boundary. `while (left < right)`
//
// 2. How do we update left and right?
//    To prevent "infinte-loop" and "missing answer"
//    Q: If mid might be anwser?
//    - No. left = mid + 1, right = mid - 1
//    - Yes. Normally, one side might be answer and the other side will not be
//      right = mid, left = mid + 1;

#if defined(suggested)
#include <vector>

class Solution {
 public:
  int findMin(std::vector<int>& nums) {
    if (nums.empty()) {
      return -1;
    }

    int left = 0;
    int right = nums.size() - 1;

    while (left < right) {
      const int mid = left + (right - left) / 2;
      if (nums[mid] < nums[right]) {
        // Right sorted, so the minimal will be on the left (including mid)
        right = mid;
      } else {
        left = mid + 1;
      }
    }

    return nums[left];
  }
};
#endif
