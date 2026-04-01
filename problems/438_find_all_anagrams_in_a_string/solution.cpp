// 438. Find All Anagrams in a String

#if defined(HASHMAP)
#include <string>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<int> findAnagrams(const std::string& s, const std::string& p) {
    if (s.size() < p.size()) return {};

    std::unordered_map<char, int> freq;
    for (char c : p) {
      ++freq[c];
    }

    int missing = freq.size();
    int p_len = p.size();
    int s_len = s.size();
    std::vector<int> result;

    auto AddCharToWindow = [&](char c) {
      auto it = freq.find(c);
      if (it != freq.end() && --it->second == 0) --missing;
    };

    auto RemoveCharFromWindow = [&](char c) {
      auto it = freq.find(c);
      if (it != freq.end() && it->second++ == 0) ++missing;
    };

    // CAUTION: Made mistake for the range
    for (int i = 0; i < p_len - 1; ++i) {  // Ran
      AddCharToWindow(s[i]);
    }

    // CAUTION: Made mistake for the range
    for (int i = p_len - 1; i < s_len; ++i) {
      AddCharToWindow(s[i]);  // Expand right, window size: p_len
      if (missing == 0) result.push_back(i - p_len + 1);
      RemoveCharFromWindow(s[i - p_len + 1]);  // Shrink, Window size: p_len - 1
    }

    return result;
  }
};
#endif

#if defined(VECTOR)
#include <string>
#include <vector>

class Solution {
 public:
  std::vector<int> findAnagrams(const std::string& s, const std::string& p) {
    int p_len = p.size();
    int s_len = s.size();
    if (s_len < p_len) return {};

    std::vector<int> result;
    std::vector<int> freq(26, 0);
    for (char c : p) {
      ++freq[c - 'a'];
    }

    int missing = p_len;  // count of chars still needed

    auto AddCharToWindow = [&](char c) {
      if (freq[c - 'a']-- > 0) --missing;
    };

    auto RemoveCharFromWindow = [&](char c) {
      if (++freq[c - 'a'] > 0) ++missing;
    };

    int left = 0;
    for (int right = 0; right < s_len; ++right) {
      AddCharToWindow(s[right]);

      while (right - left + 1 > p_len) RemoveCharFromWindow(s[left++]);

      if (missing == 0) result.push_back(left);
    }

    return result;
  }
};
#endif
