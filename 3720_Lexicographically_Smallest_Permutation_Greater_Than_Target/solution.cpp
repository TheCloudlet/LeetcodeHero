// Leetcode 3720. Lexicographically Smallest Permutation Greater Than Target
// @tags: string, greedy, sorting, counting, hash-table
// @difficulty: medium

#include <algorithm>
#include <string>
#include <vector>

// TODO: Need to review the greedy algorithm carefully.
// I don't understand why this works yet.

class Solution {
 public:
  std::string lexGreaterPermutation(std::string s, std::string target) {
    // 1. Count characters in s
    std::vector<int> s_counts(26, 0);
    for (char c : s) {
      s_counts[c - 'a']++;
    }

    std::vector<std::string> candidates;
    int n = s.length();

    // This copy tracks available characters as we build a matching prefix
    std::vector<int> current_s_counts = s_counts;

    // 2. Iterate through each possible breakpoint index i
    for (int i = 0; i < n; ++i) {
      // 3. For the current prefix match, find all possible "next greater"
      // characters
      for (char c_val = target[i] + 1; c_val <= 'z'; ++c_val) {
        int char_idx = c_val - 'a';
        if (current_s_counts[char_idx] > 0) {
          // Found a valid character to create a candidate solution
          std::string prefix = target.substr(0, i);

          // Create the suffix from the remaining available characters
          std::vector<int> remaining_counts = current_s_counts;
          remaining_counts[char_idx]--;

          std::string suffix = "";
          for (int j = 0; j < 26; ++j) {
            suffix += std::string(remaining_counts[j], 'a' + j);
          }

          // Add the valid candidate to our list
          candidates.push_back(prefix + c_val + suffix);
        }
      }

      // 4. To check the next breakpoint (i+1), we must assume we matched
      // target[i]. So, consume that character from our available set.
      int target_char_idx = target[i] - 'a';
      if (current_s_counts[target_char_idx] > 0) {
        current_s_counts[target_char_idx]--;
      } else {
        // If we can't even match target's prefix, no further
        // candidates with a longer matching prefix can be found.
        break;
      }
    }

    if (candidates.empty()) {
      return "";
    } else {
      // 5. From all the valid candidates we found, return the smallest one.
      return *std::min_element(candidates.begin(), candidates.end());
    }
  }
};