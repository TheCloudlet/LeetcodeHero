// LeetCode 12. Integer to Roman
//
// NOTE: Use greedy algorithm
//
// Constrains 1 <= n <= 3999
//
// I	1
// V	5
// X	10
// L	50
// C	100
// D	500
// M	1000
#include <array>
#include <string>

class Solution {
public:
  std::string intToRoman(int num) {
    const std::array<std::string, 13> romanChar = {
        "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
    const std::array<int, 13> val = {1000, 900, 500, 400, 100, 90, 50,
                                     40,   10,  9,   5,   4,   1};

    std::string romanStr;
    for (int i = 0; i < 13; ++i) {
      while (num >= val[i]) {
        romanStr += romanChar[i];
        num -= val[i];
      }
    }

    return romanStr;
  }
};
