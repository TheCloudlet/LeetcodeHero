// Leetcode 152. Maximum Product Subarray
// @tag: dp, array, neetcode150, need-review
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

#include <algorithm>
#include <cassert>
#include <vector>

class Solution {
public:
  int maxProduct(std::vector<int> &nums) {
    assert(!nums.empty());

    // Initialize with the first element
    int globalMaxProduct = nums[0];
    int minProductEndingHere = nums[0];
    int maxProductEndingHere = nums[0];

    for (size_t i = 1; i < nums.size(); ++i) {
      int n = nums[i];

      // We need a temp variable because we're about to overwrite
      // maxProductEndingHere, but we still need its old value to calculate
      // the new minProductEndingHere.
      int oldMax = maxProductEndingHere;

      // The new max is the largest of n, n * old_max, or n * old_min
      maxProductEndingHere =
          std::max({n, n * oldMax, n * minProductEndingHere});

      // The new min is the smallest of n, n * old_max, or n * old_min
      minProductEndingHere =
          std::min({n, n * oldMax, n * minProductEndingHere});

      // Update the overall global maximum
      globalMaxProduct = std::max(globalMaxProduct, maxProductEndingHere);
    }

    return globalMaxProduct;
  }
};