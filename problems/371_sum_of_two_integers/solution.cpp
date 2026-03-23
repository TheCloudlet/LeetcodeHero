// Leetcode 371. Sum of Two Integers

// The problem describes that the range of the number is [-1000, 1000] so we
// guarenty the value will not overflow int32_t.

#if defined(RCA)
// Ripple-Carry Adder (RCA)
class Solution {
 public:
  int getSum(int a, int b) {
    unsigned int sum = 0;
    unsigned int c = 0;

    for (int i = 0; i < 32; ++i) {
      unsigned int ai = (static_cast<unsigned int>(a) >> i) & 1;
      unsigned int bi = (static_cast<unsigned int>(b) >> i) & 1;
      sum |= (ai ^ bi ^ c) << i;
      c = (ai & bi) | (ai & c) | (bi & c);
    }

    return static_cast<int>(sum);
  }
};
#endif

#if defined(PARALLEL_RCA)
// FIXME: Explain how this works
//
// Dry run: a = 0101, b = 0011
// | round |    a |    b | carry | new_a | new_b |
// |-------|------|------|-------|-------|-------|
// |     1 | 0101 | 0011 |  0010 |  0110 |  0010 |
// |     2 | 0110 | 0010 |  0100 |  0100 |  0100 |
// |     3 | 0100 | 0100 |  1000 |  1000 |  0000 |
class Solution {
 public:
  int getSum(int a, int b) {
    while (b != 0) {
      unsigned int carry = static_cast<unsigned int>(a & b) << 1;

      a = a ^ b;
      b = carry;
    }

    return a;
  }
};
#endif
