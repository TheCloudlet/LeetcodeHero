// Leetcode 34. Find First and Last Position of Element in Sorted Array
// Binary Search for First and Last Occurrence
//
// Goal:
//   Given a sorted array of integers and a target value,
//   find the starting and ending position of the target.
//
// Strategy:
//   Use two binary search passes:
//     1. Lower bound: find the first index where nums[i] == target
//     2. Upper bound: find the last index where nums[i] == target
//
//   If target is not found, return [-1, -1].
//
// Time Complexity:
//   O(log n) for each binary search, total O(log n)

#include <vector>

class Solution {
 public:
  std::vector<int> searchRange(const std::vector<int>& nums, int target) {
    const int n = static_cast<int>(nums.size());

    int left = 0;
    int right = n;

    // Pass 1: find lower bound
    while (left < right) {
      const int mid = left + (right - left) / 2;
      if (nums[mid] < target) {
        left = mid + 1;
      } else {
        right = mid;
      }
    }

    // Check if target exists
    if (left >= n || nums[left] != target) {
      return {-1, -1};
    }

    const int lower = left;

    // Pass 2: find upper bound
    // Optimization: start from `lower` since target is at or after `lower`
    left = lower;
    right = n;
    while (left < right) {
      const int mid = left + (right - left) / 2;
      if (nums[mid] <= target) {
        left = mid + 1;
      } else {
        right = mid;
      }
    }

    // The upper bound points to the first element > target.
    // Since target existed, the index before upper bound must be target.
    const int upper = left - 1;

    return {lower, upper};
  }
};

#if defined(STL)
#include <vector>
#include <algorithm>
#include <iterator>

class Solution {
 public:
  std::vector<int> searchRange(const std::vector<int>& nums, int target) {
    const auto it1 = std::lower_bound(nums.begin(), nums.end(), target);
    if (it1 == nums.end() || *it1 != target) {
        return {-1, -1};
    }

    // Optimiztion: starting from it1
    const auto it2 = std::upper_bound(it1, nums.end(), target);

    const int first = std::distance(nums.begin(), it1);
    const int second = std::distance(nums.begin(), std::prev(it2));

    return {first, second};
  }
};
#endif
