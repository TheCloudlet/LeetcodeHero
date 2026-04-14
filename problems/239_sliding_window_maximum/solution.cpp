// Leetcode 239. Sliding Window Maximum
// @tag: sliding-window
// @tag: monotonic-queue
// @difficulty: hard

#include <cassert>
#include <deque>
#include <vector>

class Solution {
 public:
  std::vector<int> maxSlidingWindow(const std::vector<int>& nums, int k) {
    assert(k >= 0);

    const int n = static_cast<int>(nums.size());
    if (n < k) return {};

    constexpr int NINF = -1e9;

    std::vector<int> ans(n - k + 1, NINF);
    std::deque<int> dq;

    for (int right = 0; right < n; ++right) {
      const int left = right - k + 1;

      // Insert right
      // Remove elements from the back that are smaller than the current
      // element. Since the current element is both larger and enters the window
      // later, the older, smaller elements can never be the maximum. We can
      // safely evict them.
      while (!dq.empty() && nums[dq.back()] < nums[right]) {
        dq.pop_back();
      }
      dq.push_back(right);

      // Remove outdated
      if (dq.front() < left) dq.pop_front();

      if (left >= 0) ans[left] = nums[dq.front()];  // CAUTION! We want value.
    }

    return ans;
  }
};
