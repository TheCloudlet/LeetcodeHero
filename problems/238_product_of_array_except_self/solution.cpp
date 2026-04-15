// Leetcode 238. Product of Array Except Self

#if defined(EXPLICIT_PREFIX_ARRAYS)
#include <vector>

class Solution {
 public:
  std::vector<int> productExceptSelf(const std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());

    std::vector<int> scanl(n, 1);
    int acc = 1;
    for (int i = 0; i < n; ++i) {
      acc *= nums[i];
      scanl[i] = acc;
    }

    std::vector<int> scanr(n, 1);
    acc = 1;
    for (int i = n - 1; i >= 0; --i) {
      acc *= nums[i];
      scanr[i] = acc;
    }

    auto safe_get = [&n](const std::vector<int>& v, const int idx) -> int {
      if (idx < 0 || idx >= n) return 1;
      return v[idx];
    };

    std::vector<int> ans(n, 1);
    for (int i = 0; i < n; i++) {
      ans[i] = safe_get(scanl, i - 1) * safe_get(scanr, i + 1);
    }

    return ans;
  }
};
#endif

#if defined(INPLACE_STATE_COMPRESSION)
#include <vector>

class Solution {
 public:
  std::vector<int> productExceptSelf(std::vector<int>& nums) {
    int n = static_cast<int>(nums.size());

    std::vector<int> ans(n, 1);

    int acc = 1;
    for (int i = 0; i < n; ++i) {
      ans[i] = acc;
      acc *= nums[i];
    }

    acc = 1;
    for (int i = n - 1; i >= 0; --i) {
      ans[i] *= acc;
      acc *= nums[i];
    }

    return ans;
  }
};

// [1,2,3,4]
//
// acc: 2
// [1,2,6,1]
#endif
