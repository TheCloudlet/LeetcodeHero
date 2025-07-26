// Leetcode 496. Next Greater Element I
//
// Thinking:
// - Do two pass
//   1. monotonic stack and save nums2 and it's max into a map
//   2. query and return

#include <assert.h>
#include <stack>
#include <unordered_map>
#include <vector>

class Solution {
public:
  std::vector<int> nextGreaterElement(std::vector<int> &nums1,
                                      std::vector<int> &nums2) {
    assert(nums1.size() < nums2.size());
    std::stack<int> monoStack;
    std::unordered_map<int, int> nextGreater;
    for (const auto &num : nums2) { // NOTE: we don't need index
      while (!monoStack.empty() && monoStack.top() < num) {
        nextGreater[monoStack.top()] = num;
        monoStack.pop();
      }
      monoStack.push(num);
    }

    std::vector<int> res;
    res.reserve(nums1.size());
    for (const auto &num : nums1) {
      auto it = nextGreater.find(num);
      if (it != nextGreater.end()) {
        res.emplace_back(it->second);
      } else {
        res.emplace_back(-1);
      }
    }

    return res;
  }
};
