// Leetcode 13. Roman to Integer

#include <assert.h>
#include <string>
#include <unordered_map>

class Solution {
public:
  int romanToInt(const std::string &s) {
    assert(!s.empty());
    std::unordered_map<char, int> charToValue = {
        {'I', 1},   {'V', 5},   {'X', 10},  {'L', 50},
        {'C', 100}, {'D', 500}, {'M', 1000}};

    int result = 0;
    int idx = 0;
    while (idx < s.length()) {
      int curr = charToValue[s[idx]];
      int next = (idx + 1 < s.length()) ? charToValue[s[idx + 1]] : 0;
      if (curr < next) {
        result += next - curr;
        idx = idx + 2;
        continue;
      }
      result += curr;
      idx += 1;
    }

    return result;
  }
};
