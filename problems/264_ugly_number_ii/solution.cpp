// Leetcode 264 Ugly Number II

#if defined(TLE)
#include <unordered_set>

class Solution {
 public:
  int nthUglyNumber(int n) {
    std::unordered_set<int> uglies{1};
    int ugly_count = 1;
    int i = 1;
    while (ugly_count < n) {
      ++i;
      if ((i % 2 == 0 && uglies.count(i / 2)) ||
          (i % 3 == 0 && uglies.count(i / 3)) ||
          (i % 5 == 0 && uglies.count(i / 5))) {
        uglies.insert(i);
        ++ugly_count;
      }
    }
    return i;
  }
};
#endif  // TLE
