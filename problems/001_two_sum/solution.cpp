// Leetcode 1. Two Sum
// @tag: hashtable, array
// @difficulty: easy
//
// Solutions:
// 1. Brute force - O(n²) time, O(1) space
// 2. Two pointers - O(n log n) time (due to sorting), O(1) space
// 3. Hash map - O(n) time, O(n) space (implemented below)

// NOTE:
// Originally I considered a two-pass approach:
// 1. Add all elements to std::unordered_map<int, int> (value -> index)
// 2. Iterate and find complement for each element
//
// However, the single-pass approach (implemented below) is more efficient
// as it reduces redundant operations and finds the solution as soon as possible.

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

// Sorting solution
#if defined (SORTING)
#include <algorithm>
#include <cassert>
#include <vector>

class Solution {
public:
    std::vector<int> twoSum(std::vector<int>& nums, int target) {
        std::vector<std::pair<int, int>> pairVec;
        for (int idx = 0; idx < nums.size(); ++idx) {
            pairVec.push_back({nums[idx], idx});
        }
        std::sort(pairVec.begin(), pairVec.end());

        int left = 0;
        int right = nums.size() - 1;
        while (left < right) {
            int sum = pairVec[left].first + pairVec[right].first;
            if (sum == target) {
                return {pairVec[left].second, pairVec[right].second};
            } else if (sum < target) {
                ++left;
            } else {  // sum > target
                --right;
            }
        }
        return {};
    }
};
#endif
