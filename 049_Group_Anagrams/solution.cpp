// Leetcode 49. Group Anagrams
// @tag: hash, string, sort
// @difficulty: medium

#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

class Solution {
public:
  std::vector<std::vector<std::string>>
  groupAnagrams(const std::vector<std::string> &strs) {
    std::vector<std::vector<std::string>> result;
    std::unordered_map<std::string, std::vector<std::string>> anagramMap;

    for (const auto &str : strs) {
      std::string sortedStr = str;
      std::sort(sortedStr.begin(), sortedStr.end());
      anagramMap[sortedStr].push_back(str);
    }

    for (auto &entry : anagramMap) {
      result.emplace_back(std::move(entry.second)); // Avoid unnecessary copies
    }

    return result;
  }
};