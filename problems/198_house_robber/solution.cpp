// Leetcode 198. House Robber
// @tag: dp, neetcode150
// @difficulty: medium

#if defined(TOP_DOWN)
// Define DP
// dp[i] = maximum money I can have, when I reach here. (robed i or not)
#include <algorithm>
#include <vector>

class Solution {
 public:
  int rob(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());
    if (n == 0) return 0;

    auto max_money_up_to = [&](auto& self, const int idx) {
      if (idx == 0) return nums[0];
      if (idx == 1) return max(nums[0], nums[1]);
      return std::max(self(self, idx - 1), nums[idx] + self(self, idx - 2));
    };

    return max_money_up_to(max_money_up_to, n - 1);
  }
};
#endif

#if defined(TOP_DOWN_MEMO)
// Define DP
// dp[i] = maximum money I can have, when I reach here. (robed i or not)
#include <algorithm>
#include <vector>

class Solution {
 public:
  int rob(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());
    if (n == 0) return 0;

    std::vector<int> memo(n, -1);

    auto max_money_up_to = [&](auto& self, const int idx) {
      if (idx == 0) return nums[0];
      if (idx == 1) return max(nums[0], nums[1]);

      if (memo[idx] != -1) return memo[idx];

      return memo[idx] =
                 std::max(self(self, idx - 1), nums[idx] + self(self, idx - 2));
    };

    return max_money_up_to(max_money_up_to, n - 1);
  }
};
#endif

#if defined(BOTTOM_UP)
#include <algorithm>
#include <vector>

class Solution {
 public:
  int rob(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());
    if (n == 0) return 0;
    if (n == 1) return nums[0];

    std::vector<int> dp(n, 0);
    dp[0] = nums[0];
    dp[1] = std::max(nums[0], nums[1]);

    for (int i = 2; i < n; ++i) {
      dp[i] = std::max(dp[i - 1], nums[i] + dp[i - 2]);
    }

    return dp.back();
  }
};
#endif

#if defined(FOLD)
#include <vector>

class Solution {
 public:
  int rob(const std::vector<int>& nums) {
    int prev2 = 0;  // dp[i-2]
    int prev1 = 0;  // dp[i-1]
    for (int num : nums) {
      int temp = prev1;
      prev1 = std::max(prev2 + num, prev1);
      prev2 = temp;
    }
    return prev1;
  }
};
#endif
