// Leetcode 260. Single Number III

// Algorithm:
//   1. XOR all numbers -> xor_sum = a ^ b (the XOR of the two unique numbers)
//   2. Find the lowest set bit in xor_sum. This bit is 1 in either a or b, but
//      not both.
//   3. Partition numbers into two groups: those with that bit set, and those
//      without. We are certain that two numbers are in different group
//   4. XOR within each group to isolate a and b separately.
#include <numeric>
#include <vector>

class Solution {
 public:
  std::vector<int> singleNumber(const std::vector<int>& nums) {
    unsigned int xor_sum = 0;
    for (const int num : nums) {
      xor_sum ^= num;
    }

    const unsigned int lowest_set_bit = xor_sum & ~(xor_sum - 1);

    int first_single = 0;
    int second_single = 0;

    for (const int num : nums) {
      if ((num & lowest_set_bit) != 0) {
        first_single ^= num;
      } else {
        second_single ^= num;
      }
    }

    return {first_single, second_single};
  }
};
