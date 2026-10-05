// Leetcode 263. Ugly Number

#include <initializer_list>

class Solution {
 public:
  bool isUgly(int n) {
    if (n <= 0) return false;

    for (auto i : std::initializer_list<int>{2, 3, 5})
      while (n % i == 0) n /= i;

    return n == 1;
  }
};
