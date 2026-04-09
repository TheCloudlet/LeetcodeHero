// Leetcode 461. Hamming Distance

#if defined(CPU)
class Solution {
 public:
  int hammingDistance(int x, int y) {
    int count = 0;
    auto diff = static_cast<unsigned int>(x ^ y);

    while (diff != 0) {
      diff = diff & (diff - 1);
      ++count;
    }

    return count;
  }
};
#endif

#if defined(STL)
#include <bit>

class Solution {
 public:
  int hammingDistance(int x, int y) {
    return std::popcount(static_cast<unsigned int>(x ^ y));
  }
};
#endif
