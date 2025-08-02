// Leetcode 9. Palindrome Number

#include <string>
class Solution {
public:
  bool isPalindrome(int x) {
    if (x < 0) {
      return false;
    }
    std::string s = std::to_string(x);
    for (int l = 0, r = s.length() - 1; l < r; ++l, --r) {
      if (s[l] != s[r]) {
        return false;
      }
    }
    return true;
  }
};
