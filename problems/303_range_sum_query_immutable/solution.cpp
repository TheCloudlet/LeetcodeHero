// Leetcode 303. Range Sum Query - Immutable

#include <vector>

class NumArray {
 private:
  std::vector<int> prefix_sum_;

 public:
  explicit NumArray(const std::vector<int>& nums) {
    const int n = static_cast<int>(nums.size());
    prefix_sum_.resize(n + 1);  // CAUTION: resize, not reserve
    prefix_sum_[0] = 0;

    for (int i = 0; i < n; ++i) {
      prefix_sum_[i + 1] = prefix_sum_[i] + nums[i];
    }
  }

  int sumRange(int left, int right) {
    // Problem guarantees [left, right] is valid
    return prefix_sum_[right + 1] - prefix_sum_[left];
  }
};

/**
 * Your NumArray object will be instantiated and called as such:
 * NumArray* obj = new NumArray(nums);
 * int param_1 = obj->sumRange(left,right);
 */
