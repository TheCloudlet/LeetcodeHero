// Leetcode 220. Contains Duplicate III
//
// The initial approach used a `std::map` to track the sliding window, following
// the standard template (add, adjust, validate). Validation checked three
// cases:
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
  bool containsNearbyAlmostDuplicate(const std::vector<int>& nums,
                                     int indexDiff, int valueDiff) {
    // Use long long to avoid integer overflow
    std::set<long long> window;

    for (int i = 0; i < static_cast<int>(nums.size()); ++i) {
      // 1. Maintain window size: remove the element that is out of range
      if (i > indexDiff) {
        window.erase(static_cast<long long>(nums[i - indexDiff - 1]));
      }

      // 2. Find the smallest number >= nums[i] - valueDiff
      const auto curr = static_cast<long long>(nums[i]);
      auto it = window.lower_bound(curr - valueDiff);

      // 3. Check if that number is within nums[i] + valueDiff
      // (if it is smaller than curr, will be negative still works)
      if (it != window.end() && *it - curr <= valueDiff) return true;

      // 4. Insert current number after checking
      window.insert(curr);
    }
    return false;
  }
};
