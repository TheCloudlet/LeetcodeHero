// Leetcode 3719. Longest Balanced Subarray I
// @tags: array, hash-set, two-pointer, sliding-window, enumeration
// @difficulty: medium
// @contest-spent-time: 1 hr 19 mins
//
// Time Complexity: O(N^2)
// Space Complexity: O(N)

// Why this is brute-force?
//
// In general, we have 3 tools to solve array subarray problems:
// 1. Sliding-window: When we have a monotonic condition to expand/shrink the
//    window (e.g., adding elements never violates the condition).
// 2. Dynamic-Programming: When the problem has optimal substructure and
//    overlapping subproblems.
// 3. Brute-force: When neither of the above apply.
//
// Here, we don't have a monotonic condition to use sliding-window (adding
// an element can break the balance).
// Also, we don't have optimal substructure to use DP (knowing the answer for
// shorter subarrays doesn't help compute the answer for longer ones).
// So we use brute-force to enumerate all subarrays.


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