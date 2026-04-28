// Leetcode 1011. Capacity To Ship Packages Within D Days

#include <algorithm>
#include <cassert>
#include <numeric>
#include <vector>

class Solution {
 public:
  int shipWithinDays(const std::vector<int>& weights, int max_days) {
    assert(!weights.empty() && "Expected non-empty input.");

    auto CalcDays = [&](const int weight_per_day) -> int {
      int num_days = 1;
      int daily_weight = 0;
      for (const int weight : weights) {
        assert(weight <= weight_per_day &&
               "Expect size always smaller than weight_per_day");

        if (weight + daily_weight > weight_per_day) {
          ++num_days;
          daily_weight = 0;
        }
        daily_weight += weight;
      }
      return num_days;
    };

    int lo = *std::max_element(weights.begin(), weights.end());
    int hi = std::accumulate(weights.begin(), weights.end(), 0,
                             [](int acc, int elem) { return acc + elem; });

    while (lo < hi) {
      const int mid = lo + (hi - lo) / 2;
      const int d = CalcDays(mid);

      if (d > max_days) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }

    return lo;
  }
};
