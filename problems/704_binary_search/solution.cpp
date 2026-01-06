// Leetcode 704. Binary SGoal:
//
// Goal:
// - Search for a target value in a sorted array.
// - Return the index if found; otherwise return -1.
//
// Binary Search Pattern:
// - Standard search using closed interval [lo, hi].
// - while (lo <= hi)
// - mid = lo + (hi - lo) / 2
//
// Key Insight:
// - Compare target with nums[mid] to shrink the range.
// - This is the simplest binary search use case.earch

#include <vector>

class Solution {
public:
    int search(const std::vector<int>& nums, int target) {
        int lo = 0;
        int hi = nums.size() - 1;
        while (lo <= hi) {
            const int mid = lo + (hi - lo) / 2;
            if (target == nums[mid]) {
                return mid;
            } else if (nums[mid] < target) {
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }
        return -1;
    }
};
