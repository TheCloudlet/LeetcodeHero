// Leetcode 190. Reverse Bits

#if defined(SEQUENTIAL)
class Solution {
 public:
  int reverseBits(int n) {
    uint32_t x = static_cast<uint32_t>(n);
    uint32_t ans = 0;
    for (int i = 0; i < 32; ++i) {
      ans <<= 1;
      ans |= (x & 1);
      x >>= 1;

      // Or prefer the following. The assembly is exact the same as the previous
      // but by not modifing `x`. Code quality is better.
      // ans = (ans << 1) | ((x >> i) & 1);
    }
    return static_cast<int>(ans);
  }
};
#endif

#if defined(DIVIDE_AND_CONQUER)
// The core idea is to split into two halves and swap. Take 8 bit for example.
//
// Original: 1 0 1 1 | 0 1 0 0
//   Pass 1: 0 1 0 0 | 1 0 1 1    <= Mask (0b11110000, 0b00001111)
//   Pass 2: 0 0 0 1 | 1 1 1 0    <= Mask (0b11001100, 0b00110011)
//   Pass 3: 0 0 1 0 | 1 1 0 1    <= Mask (0b10101010, 0b10101010)
class Solution {
 public:
  uint32_t reverseBits(uint32_t n) {
    n = ((n & 0xffff0000) >> 16) | ((n & 0x0000ffff) << 16);
    n = ((n & 0xff00ff00) >> 8) | ((n & 0x00ff00ff) << 8);
    n = ((n & 0xf0f0f0f0) >> 4) | ((n & 0x0f0f0f0f) << 4);
    n = ((n & 0xcccccccc) >> 2) | ((n & 0x33333333) << 2);
    n = ((n & 0xaaaaaaaa) >> 1) | ((n & 0x55555555) << 1);
    return n;
  }
};
#endif

#if defined(TABLE_LOOKUP)
// The fastest solution.
#include <array>
#include <cstdint>

class Solution {
 public:
  uint32_t reverseBits(uint32_t n) {
    // Using IIFE with constexpr
    static constexpr std::array<uint8_t, 256> table = []() constexpr {
      std::array<uint8_t, 256> tbl{};
      for (int i = 0; i < 256; ++i) {
        uint8_t x = i;
        uint8_t rev = 0;
        for (int j = 0; j < 8; ++j) {
          rev = (rev << 1) | (x & 1);
          x >>= 1;
        }
        tbl[i] = rev;
      }
      return tbl;
    }();

    return (table[n & 0xFF] << 24) | (table[(n >> 8) & 0xFF] << 16) |
           (table[(n >> 16) & 0xFF] << 8) | (table[(n >> 24) & 0xFF]);
  }
};
#endif
