// Leetcode 875. Koko Eating Bananas

#include <cassert>
#include <vector>

class Solution {
 public:
  int minEatingSpeed(const std::vector<int>& piles, int h) {
    assert(!piles.empty());

    auto CalcTotalHours = [&piles](int speed) -> int {
      int time = 0;
      for (const int pile : piles) {
        time += (pile + speed - 1) / speed;  // HERE!
      }
      return time;
    };

    int lo = 1;
    int hi = *std::max_element(piles.begin(), piles.end());

    while (lo < hi) {
      const int mid = lo + (hi - lo) / 2;
      const int time = CalcTotalHours(mid);
      if (time > h) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }

    return lo;
  }
};
