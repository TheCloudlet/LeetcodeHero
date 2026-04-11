// Leetcode 152. Maximum Product Subarray
// @tag: dp, array, neetcode150, need-review, kadane-algorithm
// @difficulty: medium

// Let's think:
//
// (1) Brute-Force
// For a brute-force solution, we loop over all possible (start, end) pairs
// to find every subarray, calculate its product, and find the maximum.
//
// (2) The Correct DP Insight
// We must track **both the maximum and minimum product** ending at the previous
// position. To handle the choice between extending an old subarray or starting
// a new one, we must always consider `nums[i]` itself as a candidate.
//
// This leads to the correct logic: at each step, the new max is the largest of
// three values:
//   a. `nums[i]` itself (i.e., starting a new subarray).
//   b. `nums[i] * previous_max` (extending the previous max).
//   c. `nums[i] * previous_min` (extending the previous min, in case of a sign
//      flip).
//
// NOTE:
// This final approach also correctly handles tricky edge cases. For example, a
// zero in the array will naturally reset the `max` and `min` products because
// `nums[i]` (which would be 0) will be chosen as the new max/min, effectively
// starting a new subarray calculation from that point.

#if defined(FOLD_WITH_EXPLICIT_BASE)
#include <algorithm>
#include <cassert>
#include <climits>
#include <vector>

class Solution {
 public:
  int maxProduct(const std::vector<int>& nums) {
    int n = nums.size();
    assert(n != 0);

    int globalMax = nums[0];
    int localMax = nums[0];
    int localMin = nums[0];
    for (int idx = 1; idx < n; idx++) {  // Start with idx 1
      // Three conditions: the current is `-`, 0, `+`
      int num = nums[idx];
      int prevMax = localMax;
      int prevMin = localMin;
      localMax = std::max({prevMax * num, prevMin * num, num});
      localMin = std::min({prevMax * num, prevMin * num, num});
      globalMax = std::max(globalMax, localMax);
    }

    return globalMax;
  }
};
#endif

#if defined(FOLD_WITH_MONOID_IDENTITY)
#include <algorithm>
#include <cassert>
#include <climits>
#include <vector>

class Solution {
 public:
  int maxProduct(const std::vector<int>& nums) {
    assert(!nums.empty());

    int global_max = INT_MIN;
    int local_max = 1;  // IMPORTANT
    int local_min = 1;  // IMPORTANT

    for (const int num : nums) {
      const int prev_max = local_max;
      local_max = std::max({local_min * num, prev_max * num, num});
      local_min = std::min({local_min * num, prev_max * num, num});
      global_max = std::max({global_max, local_max, local_min});
    }

    return global_max;
  }
};
#endif
