// Leetcode 704. Binary SGoal:
//
// Goal:
// - Search for a target value in a sorted array.
// - Return the index if found; otherwise return -1.
//
// Binary Search Pattern:
// - Standard search using closed interval [left, right].
// - while (left <= right)
// - mid = left + (right - left) / 2
//
// Key Insight:
// - Compare target with nums[mid] to shrink the range.
// - This is the simplest binary search use case.earch

#include <vector>

class Solution {
public:
    int search(std::vector<int>& nums, int target) {
        int left = 0;
        int right = nums.size() - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            if (target == nums[mid]) {
                return mid;
            } else if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return -1;
    }
};
