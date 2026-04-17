// Leetcode 844. Backspace String Compare

#include <cassert>
#include <string>

class Solution {
 public:
  bool backspaceCompare(const std::string& s, const std::string& t) {
    std::string s1;
    std::string s2;

    auto BuildString = [](const std::string& in, std::string& out) -> void {
      for (const char ch : in) {
        if (ch == '#') {
          if (out.length() >= 1) out.pop_back();
        } else if (std::islower(ch)) {
          out += ch;
        } else {
          assert(false && "Invalid input: Not a lowercase letter or '#'");
        }
      }
    };

    BuildString(s, s1);
    BuildString(t, s2);

    return s1 == s2;
  }
};
