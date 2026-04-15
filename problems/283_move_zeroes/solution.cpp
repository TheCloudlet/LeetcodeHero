// 283. Move Zeroes

#include <algorithm>  // std::fill
#include <vector>

class Solution {
 public:
  void moveZeroes(std::vector<int>& nums) {
    if (nums.empty()) return;

    const int n = static_cast<int>(nums.size());

    int slow = 0;
    for (int fast = 0; fast < n; ++fast) {
      if (nums[fast] != 0) {
        nums[slow] = nums[fast];
        ++slow;
      }
    }

    std::fill(nums.begin() + slow, nums.end(), 0);
  }
};
