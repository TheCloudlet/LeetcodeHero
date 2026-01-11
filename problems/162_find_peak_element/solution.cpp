// Leetcode 162. Find Peak Element
//
// There are two ways of solviing this
// 1. while (left <= right)
// 2. while (left < right)

#include <vector>

class Solution {
 public:
  int findPeakElement(const std::vector<int>& nums) {
    int left = 0;
    int right = nums.size() - 1;

    while (left < right) {
      const int mid = left + (right - left) / 2;
      if (nums[mid] < nums[mid + 1]) {
        // If right is going uphill
        left = mid + 1;
      } else {
        right = mid;
      }
    }
    return left;
  }
};

// More risky way: Check neighbor
#if defined(not_recommended)
class Solution {
 public:
  int findPeakElement(vector<int>& nums) {
    if (nums.empty()) {
      return -1;
    }
    if (nums.size() == 1) {
      return 0;
    }
    int left = 0;
    int right = nums.size() - 1;

    while (left <= right) {
      const int mid = left + (right - left) / 2;
      const int prev = (mid - 1 >= 0) ? nums[mid - 1] : INT_MIN;
      const int next = (mid + 1 < nums.size()) ? nums[mid + 1] : INT_MIN;
      if (nums[mid] > prev && nums[mid] > next) {
        return mid;
      } else if (nums[mid] < prev) {
        right = mid - 1;
      } else {
        left = mid + 1;
      }
    }
    return -1;
  }
};
#endif
