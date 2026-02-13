// LeetCode 3634. Minimum Removals to Balance Array
//
// NOTE:
// Initially attempted a greedy approach with left and right pointers, but the
// problem is actually a sliding window problem.
//
// Time Complexity: O(N)
// Space Complextiy: O(1)

#include <algorithm>
#include <vector>

class Solution {
 public:
  int minRemoval(std::vector<int>& nums, int k) {
    std::sort(nums.begin(), nums.end());

    int max_window = 0;
    size_t left = 0;

    for (size_t right = 0; right < nums.size(); ++right) {
      // Use int64_t to prevent integer overflow during multiplication.
      // float/double is avoided to preserve precision.
      while (left < right &&
             nums[right] > static_cast<int64_t>(nums[left]) * k) {
        left++;
      }
      max_window = std::max(max_window, right - left + 1);
    }
    return nums.size() - max_window;
  }
};
