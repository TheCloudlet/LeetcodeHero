// Leetcode 643. Maximum Average Subarray I

#if defined(PATTERN)
#include <algorithm>
#include <vector>

class Solution {
 public:
  double findMaxAverage(const std::vector<int>& nums, int k) {
    const int n = static_cast<int>(nums.size());
    if (n < k) return 0.0;

    constexpr int NINF = -1e9;
    double max_sum = NINF;
    double window_sum = 0;

    // PERFORMANCE BOTTLENECK 1: Loop Overhead
    // By merging the "initial window buildup" and "sliding" phases, we force
    // the CPU to constantly calculate 'left' and evaluate internal branches for
    // every single element in the array.
    for (int right = 0; right < n; ++right) {
      int left = right - k + 1;

      // PERFORMANCE BOTTLENECK 2: Implicit Type Conversion & FPU Cost
      // nums[right] is an 'int', but window_sum is a 'double'.
      // The CPU must execute an implicit Int-to-Float conversion instruction
      // here, and then dispatch the addition to the slower Floating-Point
      // Unit (FPU, ~4 cycles) instead of the fast integer ALU (~1 cycle).
      window_sum += nums[right];

      // PERFORMANCE BOTTLENECK 3: Branch Misprediction Penalty
      // For the first k-1 iterations, this condition evaluates to false.
      // On the k-th iteration, it flips to true and stays true until the end.
      // This state change causes a branch misprediction in the CPU pipeline,
      // flushing the instruction queue momentarily.
      if (left >= 0) {
        max_sum = std::max(window_sum, max_sum);
        window_sum -= nums[left];
      }
    }

    return max_sum / static_cast<double>(k);
  }
};
#endif

#if defined(BETTER_SOLUTION)
#include <algorithm>
#include <vector>
#include <numeric>

class Solution {
 public:
  double findMaxAverage(const std::vector<int>& nums, int k) {
    int n = static_cast<int>(nums.size());
    if (n < k) return 0.0;

    int sum = std::accumulate(nums.begin(), nums.begin() + k, 0.0);
    int max_sum = sum;

    for (int pos = k; pos < n; ++pos) {
      sum = sum + nums[pos] - nums[pos - k];
      max_sum = std::max(max_sum, sum);
    }

    return static_cast<double>(max_sum) / k;
  }
};
#endif
