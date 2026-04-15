// Leetcode 242. Valid Anagram

#if defined(IN_PLACE_SORT)
#include <algorithm>
#include <string>

class Solution {
 public:
  bool isAnagram(std::string& s, std::string& t) {
    if (s.length() != t.length()) return false;

    std::sort(s.begin(), s.end());
    std::sort(t.begin(), t.end());
    return s == t;
  }
};
#endif

#if defined(FREQ_COUNT)
#include <algorithm>
#include <array>
#include <string>

class Solution {
 public:
  bool isAnagram(const std::string& s, const std::string& t) {
    if (s.length() != t.length()) return false;  // CAUTION: forget to add

    std::array<int, 26> freq_count = {0};
    for (const char ch : s) {
      assert(ch >= 'a' && ch <= 'z');
      ++freq_count[ch - 'a'];
    }

    for (const char ch : t) {
      assert(ch >= 'a' && ch <= 'z');
      if (--freq_count[ch - 'a'] < 0) return false;
    }

    // This is not needed if we add the length check
    const int count_0 = std::count_if(freq_count.begin(), freq_count.end(),
                                      [](const int elem) { return elem == 0; });

    return (count_0 == 26) ? true : false;
  }
};
#endif
