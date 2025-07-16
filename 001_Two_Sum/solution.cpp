// Leetcode 1. Two Sum
//
// Soultions
// 1. Brute force
// 2. Two pointers
// 3. Map

// NOTE:
// Originally I use two pass.
// 1. Add all to std::unordered_map<std::vector<int>>
// 2. itreate and find complemnt
//
// But this uses redundant spaces

#include <unordered_map>
#include <vector>

class Solution {
public:
  std::vector<int> twoSum(std::vector<int> &nums, int target) {
    std::unordered_map<int, int> value_to_index;

    for (int i = 0; i < nums.size(); ++i) {
      int complement = target - nums[i];
      if (value_to_index.count(complement)) {
        return {value_to_index[complement], i};
      }
      value_to_index[nums[i]] = i;
    }

    return {};
  }
};
