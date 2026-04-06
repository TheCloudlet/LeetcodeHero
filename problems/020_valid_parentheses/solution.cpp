// Leetcode 20. Valid Parentheses

#include <stack>
#include <string>

class Solution {
 public:
  bool isValid(const std::string& s) {
    // Use bitwise AND for a strictly safe parity check, bypassing the signed
    // modulo issue entirely. (e.g., -3 % 2 == -1)
    if (s.length() & 1) return false;

    std::stack<char> expected;

    for (const char c : s) {
      switch (c) {
        case '(':
          expected.push(')');
          break;
        case '[':
          expected.push(']');
          break;
        case '{':
          expected.push('}');
          break;

        default:
          if (expected.empty() || expected.top() != c) {
            return false;
          }
          expected.pop();
      }
    }

    return expected.empty();
  }
};
