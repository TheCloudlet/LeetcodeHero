// Leetcode 27 Remove Elements
//
// Time Complexity: O(N)
// Space Complexity: O(1)
//   where N is the size of nums

#if defined(typical)
#include <vector>

class Solution {
 public:
  int removeElement(std::vector<int>& nums, int val) {
    if (nums.empty()) {
      return 0;
    }

    size_t slow = 0;
    for (size_t fast = 0; fast < nums.size(); ++fast) {
      if (nums[fast] != val) {
        nums[slow] = nums[fast];
        ++slow;
      }
    }

    return static_cast<int>(slow);  // Remember to explicitly cast
  }
};
#endif

// In fact, we should learn how to use stl algorithms.

#if defined(algorithm)
#include <algorithm>
#include <vector>

class Solution {
 public:
  int removeElement(std::vector<int>& nums, int val) {
    const auto it = std::remove(nums.begin(), nums.end(), val);
    return std::distance(nums.begin(), it);
  }
};
#endif
