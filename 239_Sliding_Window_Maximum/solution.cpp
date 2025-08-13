// Leetcode 239. Sliding Window Maximum
// @tag: sliding-window, monotonic-queue
// @difficulty: hard

#include <cassert>
#include <deque>
#include <vector>

class Solution {
public:
  std::vector<int> maxSlidingWindow(std::vector<int> &nums, int k) {
    assert(k <= nums.size());
    std::deque<int> monoQueue; // decreasing order (saving index)
    std::vector<int> result;

    for (int idx = 0; idx < nums.size(); ++idx) {
      // Pop the outdated
      if (!monoQueue.empty() && monoQueue.front() <= idx - k) {
        monoQueue.pop_front();
      }
      // Remove smaller element from behind
      while (!monoQueue.empty() && nums[monoQueue.back()] < nums[idx]) {
        monoQueue.pop_back();
      }
      monoQueue.push_back(idx);
      if (idx >= k - 1) {
        result.push_back(nums[monoQueue.front()]);
      }
    }

    return result;
  }
};