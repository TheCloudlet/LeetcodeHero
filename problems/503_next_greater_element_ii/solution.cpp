// Leetcode 503. Next Greater Element II
//
// NOTE:
// Compared to LC496, this problem is circular.
// Very similar pattern, except we must iterate the array twice.

#include <stack>
#include <vector>

class Solution {
 public:
  std::vector<int> nextGreaterElements(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());
    std::vector<int> ans(nums.size(), -1);
    std::stack<int> pending;

    for (int i = 0; i < n * 2; ++i) {
      int idx = i % n;

      while (!pending.empty() && nums[pending.top()] < nums[idx]) {
        ans[pending.top()] = nums[idx];
        pending.pop();
      }

      if (ans[idx] == -1) pending.push(idx);
    }

    return ans;
  }
};
