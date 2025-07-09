#include <climits>
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
