// Leetcode 3720. Lexicographically Smallest Permutation Greater Than Target
// @tags: TODO
// @difficulty: medium

#include <string>

class Solution {
 public:
  std::string lexGreaterPermutation(std::string s, std::string target) {
    if (target.size() > s.size()) {
      return "";
    }

    int char_count[26] = {0};
    for (const char& c : s) {
      char_count[c - 'a']++;
    }

    std::string candidate;
    candidate.reserve(s.size());

    bool is_greater = false;
    for (int idx = 0; !is_greater && idx < target.size(); ++idx) {
      char c = target[idx];
      if (char_count[c - 'a'] > 0) {
        candidate.push_back(c);
        printf("A>> push %c\n", c);
        char_count[c - 'a']--;
      } else {
        // find the next greater char
        for (char next_c = c + 1; next_c <= 'z'; ++next_c) {
          if (char_count[next_c - 'a'] > 0) {
            candidate.push_back(next_c);
            printf("B>> push %c <- Break\n", next_c);
            char_count[next_c - 'a']--;
            is_greater = true;
            break;
          }
        }
      }
    }

    if (is_greater) {
      // fill the rest with smallest chars
      for (int i = 0; i < 26; ++i) {
        while (char_count[i] > 0) {
          candidate.push_back('a' + i);
          printf("C>> push %c\n", 'a' + i);
          char_count[i]--;
        }
      }
      assert(candidate.size() == s.size());
      return candidate;
    }

    return "";
  }
};