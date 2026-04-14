// Leetcode 49. Group Anagrams
// @tag: hash, string, sort
// @difficulty: medium

#if defined(USE_SORTED_STRING_KEY)
#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<std::vector<std::string>> groupAnagrams(
      const std::vector<std::string>& strs) {
    std::unordered_map<std::string, std::vector<std::string>> groups;

    for (const auto& str : strs) {
      std::string key = str;
      std::sort(key.begin(), key.end());
      groups[key].push_back(str);
    }

    std::vector<std::vector<std::string>> ans;
    ans.reserve(groups.size());  // Missed

    for (auto& [key, str_list] : groups) {
      ans.emplace_back(std::move(str_list));
    }

    groups.clear();
    return ans;
  }
};
#endif

#if defined(USE_FREQUENCY_HASH_KEY)
#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<std::vector<std::string>> groupAnagrams(
      const std::vector<std::string>& strs) {
    std::unordered_map<std::string, std::vector<std::string>> groups;

    // This will be slower if you understand the std::string SSO and heap
    // realloc
    auto GetKey = [](const std::string& s) {
      vector<int> count(26, 0);
      for (char c : s) count[c - 'a']++;
      string key = "";
      for (int c : count) key += to_string(c) + "#";  // Split '1' and '11'
      return key;
    };

    for (const auto& str : strs) {
      groups[GetKey(str)].push_back(str);
    }

    std::vector<std::vector<std::string>> ans;
    ans.reserve(groups.size());  // Missed

    for (auto& [key, str_list] : groups) {
      ans.emplace_back(std::move(str_list));
    }

    groups.clear();
    return ans;
  }
};
#endif
