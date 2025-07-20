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
        int lower = -1, upper = -1;
        int left = 0, right = nums.size();
        int mid;

        // Pass 1: find lower
        while (left < right) {
            mid = left + (right - left) / 2;
            if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        if (left >= nums.size() || nums[left] != target) {
            return {-1, -1}; // target does not exsist in nums
        }
        lower = left;

        // Pass 2: find upper
        left = lower; // don't need to find from index 0
        right = nums.size();
        while (left < right) {
            mid = left + (right - left) / 2;
            if (nums[mid] <= target) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        upper = left - 1;

        return {lower, upper};
    }
};
