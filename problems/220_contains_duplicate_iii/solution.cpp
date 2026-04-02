// Leetcode 220. Contains Duplicate III
//
// The initial approach used a `std::map` to track the sliding window, following
// the standard template (add, adjust, validate). Validation checked three cases:
//   1. Exact duplicates.
//   2. Difference with `std::prev`.
//   3. Difference with `std::next`.
//
// Key lessons learned:
// 1. Integer overflow: Subtracting two `int` values can overflow. We must use
//    `long long` for container values and arithmetic operations.
// 2. Stale elements: We must systematically erase nodes falling out of the
//    window to ensure we don't compare against data outside the `indexDiff`
//    range.
//
// Optimization:
// Since we return immediately upon finding a valid pair, all elements in the
// window are guaranteed to be unique. Thus, a `std::set` suffices over a `map`.

#include <set>
#include <vector>

class Solution {
 public:
  bool containsNearbyAlmostDuplicate(std::vector<int>& nums, int indexDiff,
                                     int valueDiff) {
    // Use long long as the container type to perfectly avoid Integer Overflow
    std::set<long long> window;

    for (int i = 0; i < static_cast<int>(nums.size()); ++i) {
      // 1. Maintain window size: cleanly sever the past, leaving no Ghost Nodes
      if (i > indexDiff) {
        window.erase(nums[i - indexDiff - 1]);
      }

      // 2. Find if there exists a lower bound satisfying the condition
      // We want to find an x such that: nums[i] - valueDiff <= x <= nums[i] +
      // valueDiff
      auto it = window.lower_bound(static_cast<long long>(nums[i]) - valueDiff);

      // 3. Check if the found number is within the allowed upper bound
      if (it != window.end() && *it - nums[i] <= valueDiff) {
        return true;
      }

      // 4. Insert the current number into the state machine only after checking
      window.insert(nums[i]);
    }

    return false;
  }
};
