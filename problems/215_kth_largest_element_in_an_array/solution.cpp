// Leetcode 215 Kth Largest Element in an Arrar
//
// Note:
// The first comming way is to use a heap. It is trivial, but not fast enough.
// The fastest way is to use std::nth_element (quick select)

#if defined(queue)
// Time Complexity: O(N log N)
// Space Complexity: O(N)
//   where N is the size of nums array

#include <queue>
#include <vector>

class Solution {
 public:
  int findKthLargest(const std::vector<int>& nums, int k) {
    if (k <= 0 || nums.size() < k) {
      return -1;
    }

    std::priority_queue<int> pq(nums.begin(), nums.end());

    while (k > 1) {
      pq.pop();
      --k;
    }

    return pq.top();
  }
};
#endif

#if defined(stl)
// Time Complexity: O(N)
// Space Complexity: O(N)
//   where N is the size of nums array

#include <algorithm>
#include <vector>

class Solution {
 public:
  // remember to remove const
  int findKthLargest(std::vector<int>& nums, int k) {
    if (k <= 0 || nums.size() < k) {
      return -1;
    }

    std::nth_element(nums.begin(), nums.begin() + k - 1, nums.end(),
                     std::greater<int>());

    return nums[k - 1];
  }
};

#endif
