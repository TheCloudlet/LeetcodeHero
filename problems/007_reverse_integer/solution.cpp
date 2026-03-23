// Leetcode 7. Reverse Integer

#include <climits>

#if defined(ITERATIVE)
class Solution {
 public:
  int reverse(int x) {
    int result = 0;
    int digit = 0;
    while (0 != x) {
      digit = x % 10;
      if (result > INT_MAX / 10 || result < INT_MIN / 10) {
        // key is here
        return 0;
      }
      result = 10 * result + digit;
      x /= 10;
    }
    return result;
  }
};
#endif

#if defined(RECURSIVE)
#include <climits>

class Solution {
 public:
  int reverse(int x) {
    return reverseHelper(x, 0);
  }

 private:
  int reverseHelper(int x, int rev) {
    if (x == 0) return rev;
    int pop = x % 10;
    if (rev > INT_MAX / 10 || (rev == INT_MAX / 10 && pop > 7)) return 0;
    if (rev < INT_MIN / 10 || (rev == INT_MIN / 10 && pop < -8)) return 0;
    rev = rev * 10 + pop;
    return reverseHelper(x / 10, rev);
  }
};
#endif
