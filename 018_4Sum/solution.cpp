// Leetcode 18. 4Sum
// @tag: array, two-pointer, sorting, skip-duplicate
// @difficulty: medium
// @take-aways: duplicate handling using pointer-skipping

#include <algorithm>
#include <vector>

class Solution {
public:
  std::vector<std::vector<int>> fourSum(std::vector<int> &nums, int target) {
    std::vector<std::vector<int>> result;
    if (nums.size() < 4) {
      return result;
    }
    std::sort(nums.begin(), nums.end());

    for (int i = 0; i < nums.size() - 3; ++i) {
      // Skip duplicates for the first number
      if (i > 0 && nums[i] == nums[i - 1]) {
        continue;
      }

      for (int j = i + 1; j < nums.size() - 2; ++j) {
        // Skip duplicates for the second number
        if (j > i + 1 && nums[j] == nums[j - 1]) {
          continue;
        }

        long long newTarget = (long long)target - nums[i] - nums[j];
        int l = j + 1;
        int r = nums.size() - 1;

        while (l < r) {
          long long sum = (long long)nums[l] + nums[r];
          if (sum < newTarget) {
            l++;
          } else if (sum > newTarget) {
            r--;
          } else {
            result.push_back({nums[i], nums[j], nums[l], nums[r]});

            // Skip duplicates for the third and fourth numbers
            int lastL = nums[l];
            while (l < r && nums[l] == lastL) {
              l++
            };

            int lastR = nums[r];
            while (l < r && nums[r] == lastR) {
              r--
            };
          }
        }
      }
    }
    return result;
  }
};
