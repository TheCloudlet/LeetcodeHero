// Leetcode 16. 3Sum Closest
// @tag: array, two-pointer, sorting, need-review
// @difficulty: medium

// --- Intuition & Strategy ---
// The goal is to find three numbers whose sum is as close as possible
// to a target value.

// ---
// ## 1. Brute-Force Approach: O(n^3)
// The most straightforward solution is to check every possible unique
// combination of three numbers. We can use three nested loops to
// iterate through all triplets (i, j, k). For each triplet, we
// calculate the sum and see if it's closer to the target than what
// we've seen so far. While simple, this O(n^3) approach is too
// slow for typical constraints.

// ---
// ## 2. Optimized Approach: Sort + Two Pointers: O(n^2)
// A much better approach is to first sort the array. Sorting allows
// us to intelligently search for the remaining two numbers. The core
// idea is to reduce the problem's complexity:
//
//   1. Fix one number: We iterate through the array with a `for`
//      loop, picking `nums[i]` as the first number of our triplet.
//
//   2. Solve a 2Sum Closest problem: For each `nums[i]`, we need
//      to find two other numbers in the rest of the array (from
//      index `i+1` to the end) whose sum is closest to
//      `target - nums[i]`.
//
//   3. Use Two Pointers: Because the array is sorted, this "2Sum
//      Closest" sub-problem can be solved in O(n) time. We set a
//      `left` pointer at `i+1` and a `right` pointer at the end.
//      - If the sum is less than the target, we need a larger sum,
//        so we increment `left`.
//      - If the sum is greater than the target, we need a smaller
//        sum, so we decrement `right`.
//
// This method avoids redundant calculations. The outer loop is O(n)
// and the inner two-pointer scan is also O(n), giving a total time
// complexity of O(n*n) = O(n^2).

// ---
// ## 3. Why Binary Search Isn't Optimal Here
// One might think of fixing two numbers (`nums[i]` and `nums[j]`)
// and then using binary search to find the third number that's
// closest to `target - nums[i] - nums[j]`.
//
//   - Complexity: This would involve two nested `for` loops (O(n^2))
//     and a binary search (O(log n)) inside, leading to a total
//     time complexity of O(n^2 * log n).
//
//   - Conclusion: The two-pointer approach (O(n^2)) is more
//     efficient. The two-pointer scan is an amortized O(n)
//     operation for the sub-problem. Performing a separate binary
//     search for every pair of `(i, j)` is more work. The two-
//     pointer method better utilizes the sorted nature of the array
//     to discard search space linearly.

#include <algorithm>
#include <cmath> // Required for std::abs
#include <limits>
#include <vector>

class Solution {
public:
  int threeSumClosest(std::vector<int> &nums, int target) {
    std::sort(nums.begin(), nums.end());

    // Initial with a first guess
    int closestSum = nums[0] + nums[1] + nums[2];

    for (int first = 0; first < nums.size() - 2; ++first) {
      int left = first + 1;
      int right = nums.size() - 1;

      while (left < right) {
        int currentSum = nums[first] + nums[left] + nums[right];
        if (std::abs(currentSum - target) < std::abs(closestSum - target)) {
          closestSum = currentSum;
        }

        if (currentSum < target) {
          ++left;
        } else if (currentSum > target) {
          --right;
        } else {
          return currentSum;
        }
      }
    }
    return closestSum;
  }
};