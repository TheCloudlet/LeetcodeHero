// 389. Find the Difference

#if defined(FREQ_COUNT)
#include <array>
#include <cassert>
#include <string>

class Solution {
 public:
  char findTheDifference(const std::string& s, const std::string& t) {
    if (s.empty()) return t[0];

    std::array<int, 26> freq = {0};

    for (const char ch : s) {
      ++freq[ch - 'a'];
    }

    for (const char ch : t) {
      if (freq[ch - 'a'] <= 0) return ch;

      --freq[ch - 'a'];
    }

    assert(false && "Should not reach here");
    return '0';
  }
};
#endif

#if defined(XOR)
#include <cassert>
#include <functional>  // std::bit_xor
#include <numeric>

class Solution {
 public:
  char findTheDifference(const std::string& s, const std::string& t) {
    char acc = 0;

    acc = std::accumulate(s.begin(), s.end(), 0, std::bit_xor<>());
    acc = std::accumulate(t.begin(), t.end(), acc, std::bit_xor<>());

    return static_cast<char>(acc);
  }
};
#endif
