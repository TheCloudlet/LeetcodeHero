// Leetcode 410. Split Array Largest Sum

#include <algorithm>
#include <cassert>
#include <numeric>
#include <vector>
#include <cstdint>

class Solution {
 public:
  [[nodiscard]] int splitArray(const std::vector<int>& nums, int k) {
    assert(!nums.empty() && "Expected non-empty vector input.");
    assert(k > 0 && "Expected strictly positive split count.");

    // To int64_prevent overflow
    int64_t lo = *std::max_element(nums.begin(), nums.end());
    int64_t hi = std::accumulate(nums.begin(), nums.end(), 0LL);

    auto IsValid = [&nums, k](const int64_t largest_sum) noexcept -> bool {
      int64_t acc = 0;
      int count = 1;

      for (const int num : nums) {
        if (acc + num > largest_sum) {
          ++count;
          acc = 0;
        }
        acc += num;
      }
      return count <= k;
    };

    while (lo < hi) {
      const int64_t mid = lo + (hi - lo) / 2;
      if (!IsValid(mid)) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }

    return static_cast<int>(lo);
  }
};
