// 6. Zigzag Conversion
//
// Thinking
//
// (n = 3)
// 0   4   8   (3+3-2) = 4
// 1 3 5 7 9
// 2   6   10
//
// => 0,4,8,1,3,5,7,9,2,6,10
//
// (n = 4)
// 0    6       12 (4+4-2)
// 1  5 7    11 13
// 2 4  8 10    14
// 3    9       15
//
// => 0,6,12,1,5,7,11,13,2,4,8,10,14
#include <cassert>
#include <string>

class Solution {
public:
  std::string convert(std::string s, int numRows) {
    assert(numRows > 0);
    if (1 == numRows) {
      return s;
    }
    int numChar = s.size();
    std::string result;
    // NOTE: please change `i` to `row` for clarity
    for (int i = 0; i < numRows; ++i) { // outer iter
      const int last = numRows - 1;
      if (0 == i) { // first row
        for (int j = i; j < numChar; j += 2 * numRows - 2) {
          result.push_back(s[j]);
        }
      } else if (numRows - 1 == i) { // last row
        for (int j = i; j < numChar; j += 2 * numRows - 2) {
          result.push_back(s[j]);
        }
      } else {                 // other row
        bool direction = true; // true: up, false: down
        int j = i;
        while (j < numChar) {
          result.push_back(s[j]);
          if (direction) {
            j += 2 * (numRows - i - 1); // NOTE: forget to -1 here
            direction = false;
          } else {
            j += 2 * i;
            direction = true;
          }
        }
      }
    }
    assert(result.size() == numChar);
    return result;
  }
};

// 但其實我們可以直接用 std::vector<string>
// 每一個 row 做一個 string。Iterate 完以後再 Concat 就可以了
