// Leetcode 496. Next Greater Element I
//
// Thinking:
// - Do two pass
//   1. monotonic stack and save nums2 and it's max into a map
//   2. query and return

#include <stack>
#include <unordered_map>
#include <vector>

class Solution {
 public:
  std::vector<int> nextGreaterElement(const std::vector<int>& query,
                                      const std::vector<int>& nums) {
    std::unordered_map<int, int> next_greater;
    std::stack<int> pending;

    for (const int val : nums) {
      while (!pending.empty() && pending.top() < val) {
        next_greater[pending.top()] = val;
        pending.pop();
      }
      pending.push(val);
    }

    std::vector<int> ans;
    ans.reserve(query.size());

    for (const int q_val : query) {
      const auto it = next_greater.find(q_val);
      ans.push_back(it != next_greater.end() ? it->second : -1);
    }

    return ans;
  }
};
