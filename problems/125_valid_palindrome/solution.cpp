// Leetcode 125. Valid Palindrome
//
// Note: for isalnum and tolower the input should be unsigned char

#include <cctype>  // isalnum, tolower
#include <string>

class Solution {
 public:
  bool isPalindrome(const std::string& s) {
    if (s.empty()) return false;

    int left = 0;
    int right = static_cast<int>(s.size()) - 1;  // -1 should be in the back to
                                                 // prevent overflow

    while (left < right) {
      while (left < right &&
             !std::isalnum(static_cast<unsigned char>(s[left]))) {
        ++left;
      }
      while (left < right &&
             !std::isalnum(static_cast<unsigned char>(s[right]))) {
        --right;
      }

      if (std::tolower(static_cast<unsigned char>(s[left])) !=
          std::tolower(static_cast<unsigned char>(s[right]))) {
        return false;
      }

      ++left;
      --right;
    }

    return true;
  }
};
