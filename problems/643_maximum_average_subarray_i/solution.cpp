// Leetcode 643. Maximum Average Subarray I

#if defined(PATTERN)
#include <vector>

class Solution {
 public:
  double findMaxAverage(const std::vector<int>& nums, int k) {
    int n = static_cast<int>(nums.size());
    if (n < k) return 0.0;

    double max_sum = INT_MIN;
    double sum = 0;
    int left = 0;

    for (int right = 0; right < n; ++right) {
      sum += nums[right];
      if (right - left + 1 == k) {
        max_sum = std::max(max_sum, sum);
        sum -= nums[left];
        ++left;
      }
    }

    return max_sum / k;
  }
};
#endif

#if defined(SOLUTION_2)
#include <algorithm>
#include <vector>
#include <numeric>

class Solution {
 public:
  double findMaxAverage(const std::vector<int>& nums, int k) {
    int n = static_cast<int>(nums.size());
    if (n < k) return 0.0;

    double sum = std::accumulate(nums.begin(), nums.begin() + k, 0.0);
    double max_sum = sum;

    for (int pos = k; pos < n; ++pos) {
      sum = sum + nums[pos] - nums[pos - k];
      max_sum = std::max(max_sum, sum);
    }

    return max_sum / k;
  }
};
#endif
