// Leetcode 137. Single Number II

#if defined(BIT_MODULO)
// This solution uses bitwise accumulation and modulo to separate the number
// that appears once from those that appear three times.
//
// For each bit position (0 to 31), we count how many numbers have that bit set.
// Since every number except the target appears three times, the bit count
// modulo 3 will be 1 if the target has that bit set, otherwise 0.
#include <numeric>
#include <vector>

class Solution {
 public:
  int singleNumber(const std::vector<int>& nums) {
    int unique_num = 0;
    for (int bit = 0; bit < 32; ++bit) {
      int bit_sum = 0;
      for (const int num : nums) {
        bit_sum += ((num >> bit) & 1);
      }

      if (bit_sum % 3 == 1) unique_num |= (1u << bit);
    }
    return unique_num;
  }
};
#endif

#if defined(FSM)
// Finite State Machine (FSM) approach: track bits modulo 3 using two integers.
//
// Each bit can be in one of three states:
//   - 0: bit has appeared 0 times (mod 3)
//   - 1: bit has appeared 1 time  (mod 3)
//   - 2: bit has appeared 2 times (mod 3)
//
// We encode the state using two variables:
//   ones: bits that are in state 1 (count ≡ 1)
//   twos: bits that are in state 2 (count ≡ 2)
//   (state 0 is when both are 0)
///
// Transition table for each incoming bit x (0 or 1):
// +-----------+-----------+---+-----------+-----------+
// | ones (in) | twos (in) | x | ones (out)| twos (out)|
// +-----------+-----------+---+-----------+-----------+
// |     0     |     0     | 0 |     0     |     0     |
// |     0     |     0     | 1 |     1     |     0     |
// |     1     |     0     | 0 |     1     |     0     |
// |     1     |     0     | 1 |     0     |     1     |
// |     0     |     1     | 0 |     0     |     1     |
// |     0     |     1     | 1 |     0     |     0     |
// +-----------+-----------+---+-----------+-----------+
//
// Derived logic:
//   new_ones = (ones ^ x) & ~twos
//   new_twos = (twos ^ x) & ~new_ones
//
// Applied bitwise to all 32 bits in parallel.
#include <vector>

class Solution {
 public:
  int singleNumber(const std::vector<int>& nums) {
    int ones = 0;
    int twos = 0;
    for (int num : nums) {
      ones = (ones ^ num) & ~twos;
      twos = (twos ^ num) & ~ones;
    }
    return ones;
  }
};
#endif
