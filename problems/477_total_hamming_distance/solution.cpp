// Leetcode 477. Total Hamming Distance

#if defined(TRIVIAL)
#include <bit>
#include <vector>

class Solution {
 public:
  int totalHammingDistance(const std::vector<int>& nums) {
    int count = 0;
    int n = static_cast<int>(nums.size());

    for (int i = 0; i < n; ++i) {
      for (int j = i + 1; j < n; ++j) {
        count += std::popcount(static_cast<unsigned int>(nums[i] ^ nums[j]));
      }
    }

    return count;
  }
};
#endif

#if defined(LEARN)
#include <vector>

class Solution {
 public:
  int totalHammingDistance(const std::vector<int>& nums) {
    int total_distance = 0;
    int n = static_cast<int>(nums.size());

    for (int i = 0; i < 32; ++i) {
      int count_ones = 0;

      // Count bit 1 Nums
      for (int num : nums) {
        if ((static_cast<unsigned int>(num) >> i) & 1) {
          ++count_ones;
        }
      }

      const int count_zeros = n - count_ones;

      total_distance += count_ones * count_zeros;
    }

    return total_distance;
  }
};
#endif
