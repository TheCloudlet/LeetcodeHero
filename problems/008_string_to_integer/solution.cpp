// Leetcode 8. String to Integer (atoi)

// FIXME: still not fully understood
// REVIEW: integer boundary

#include <cctype>
#include <climits>
#include <string>

class Solution {
public:
  int myAtoi(std::string s) {
    int i = 0;
    int n = s.length();

    // Skip whitespace
    while (i < n && std::isspace(s[i])) ++i;

    // Sign
    int sign = 1;
    if (i < n && (s[i] == '+' || s[i] == '-')) {
      if (s[i] == '-') sign = -1;
      ++i;
    }

    // Use long long to accumulate
    long long result = 0; // int64_t
    while (i < n && std::isdigit(s[i])) {
      int digit = s[i] - '0';
      result = result * 10 + digit;

      // Check overflow against 32-bit int
      if (sign == 1 && result > INT_MAX) return INT_MAX;
      if (sign == -1 && -result < INT_MIN) return INT_MIN;

      ++i;
    }

    return static_cast<int>(sign * result);
  }
};
