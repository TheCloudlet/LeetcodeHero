// Leetcode 28 Find the Index of the First Occurrence in a String
//
// Time Complexity: O(M+N)
// Space Complexity: O(1)
//   where
//     M is size of haystack
//     N is size of needle

#include <string>

class Solution {
 public:
  int strStr(std::string& haystack, std::string& needle) {
    const size_t pos = haystack.find(needle);
    if (pos == std::string::npos) {
      return -1;
    }

    return static_cast<int>(pos);
  }
};
