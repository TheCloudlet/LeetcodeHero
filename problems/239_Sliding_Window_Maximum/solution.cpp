// Leetcode 239. Sliding Window Maximum
// @tag: sliding-window
// @tag: monotonic-queue
// @difficulty: hard

#include <cassert>
#include <deque>
#include <vector>

class Solution {
public:
  std::vector<int> maxSlidingWindow(std::vector<int> &nums, int k) {
    assert(k <= nums.size());
    std::deque<std::size_t> monoQueue; // Non-increasing monotonic queue
    std::vector<int> result;
    result.reserve(nums.size() - k + 1);

    for (std::size_t idx = 0; idx < nums.size(); ++idx) {
      // Pop the outdated
      while (!monoQueue.empty() && idx >= k && monoQueue.front() <= idx - k) {
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