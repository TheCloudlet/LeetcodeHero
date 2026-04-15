// 744. Find Smallest Letter Greater Than Target

#include <algorithm>
#include <cassert>
#include <vector>

class Solution {
 public:
  char nextGreatestLetter(const std::vector<char>& letters, char target) {
    const int n = static_cast<int>(letters.size());
    assert(n >= 2);

    const auto it = std::upper_bound(letters.begin(), letters.end(), target);

    return (it == letters.end()) ? letters[0] : *it;
  }
};
