// Leetcode 3719. Longest Balanced Subarray I
// @tags: array, hash-set, two-pointer, sliding-window, enumeration
// @difficulty: medium
// @contest-spent-time: 1 hr 19 mins
//
// Time Complexity: O(N^2)
// Space Complexity: O(N)

#include <algorithm>
#include <unordered_set>
#include <vector>

class Solution {
 public:
  int longestBalanced(std::vector<int>& nums) {
    int max_len = 0;
    for (int start = 0; start < nums.size(); ++start) {
      std::unordered_set<int> distinct_odd, distinct_even;
      for (int end = start; end < nums.size(); ++end) {
        if (nums[end] % 2 == 0) {
          distinct_even.insert(nums[end]);
        } else {
          distinct_odd.insert(nums[end]);
        }

        if (distinct_odd.size() == distinct_even.size()) {
          max_len = std::max(max_len, end - start + 1);
        }
      }
    }
    return max_len;
  }
};