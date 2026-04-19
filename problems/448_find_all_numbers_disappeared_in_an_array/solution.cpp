// Leetcode 448. Find All Numbers Disappeared in an Array

#if defined(MYSOLUTION)
#include <vector>

class Solution {
 public:
  std::vector<int> findDisappearedNumbers(const std::vector<int>& nums) {
    const int n = static_cast<int>(nums.size());

    // The more extreme solution is to use `nums` sign as vistited. If negative
    // means we have alrealy visisted. So this is inplace
    std::vector<bool> existed(n + 1, false);
    existed[0] = true;

    for (const int num : nums) {
      if (num <= n) existed[num] = true;
    }

    std::vector<int> ans;
    ans.reserve(n);

    for (int i = 1; i <= n; ++i ) {
      if (existed[i] == false) ans.push_back(i);
    }

    return ans;
  }
};
#endif
