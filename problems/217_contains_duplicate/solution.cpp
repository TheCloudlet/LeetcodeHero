// Leetcode 217. Contains Duplicate
// @tag: neetcode150, hashmap
// @difficulty: easy

#include <unordered_set>
#include <vector>

class Solution {
public:
  bool containsDuplicate(std::vector<int> &nums) {
    std::unordered_set<int> exsisted;
    for (const auto &num : nums) {
      if (exsisted.find(num) != exsisted.end()) {
        return true;
      }
      exsisted.insert(num);
    }
    return false;
  }
};