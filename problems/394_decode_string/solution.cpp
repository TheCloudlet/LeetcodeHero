// Leetcode 394 Decode String

#include <cassert>
#include <cctype>
#include <string>

class Solution {
 public:
  std::string decodeString(std::string& s) {
    size_t pos = 0;
    return decode(s, pos);
  }

 private:
  std::string decode(std::string& s, size_t& pos) {
    std::string out;

    while (pos < s.size() && s[pos] != ']') {
      if (std::isalpha(s[pos])) {
        out += s[pos];
        ++pos;
      } else if (std::isdigit(s[pos])) {
        int repeat_count = 0;
        while (pos < s.size() && s[pos] != '[') {
          repeat_count = repeat_count * 10 + s[pos] - '0';
          ++pos;
        }

        ++pos;  // skip '['
        const std::string decoded = decode(s, pos);
        while (repeat_count > 0) {
          out += decoded;
          --repeat_count;
        }
        ++pos;  // skip ']'
      } else {
        assert(false && "unexpected input");
      }
    }

    return out;
  }
};
