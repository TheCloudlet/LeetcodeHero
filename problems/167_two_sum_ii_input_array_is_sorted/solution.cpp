// Leetcode 167 Two Sum Ii Input Array Is Sorted

#include <vector>

class Solution {
 public:
  std::vector<int> twoSum(const std::vector<int>& numbers, int target) {
    int l = 0, r = numbers.size() - 1;
    while (l < r) {
      int sum = numbers[l] + numbers[r];
      if (sum == target) {
        return {l + 1, r + 1};  // 1-based index
      } else if (sum < target) {
        ++l;
      } else {
        --r;
      }
    }
    return {};
  }
};
