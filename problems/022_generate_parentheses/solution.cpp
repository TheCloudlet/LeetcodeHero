// Leetcode 22. Generate Parentheses
// @tag: string, backtracking, dp
// @difficulty: medium
#include <cassert>
#include <stack>
#include <string>
#include <vector>

class Solution {
 public:
  std::vector<std::string> generateParenthesis(int n) {
    assert(n >= 0);
    std::vector<std::string> ans;
    std::string curr;
    curr.reserve(2 * n);

    auto backtrack = [&](auto& self, int open_count, int close_count) {
      assert(open_count >= close_count);

      if (open_count == n && close_count == n) {
        ans.push_back(curr);
        return;
      }

      if (open_count < n) {
        curr.push_back('(');
        self(self, open_count + 1, close_count);
        curr.pop_back();
      }

      if (open_count > close_count) {
        curr.push_back(')');
        self(self, open_count, close_count + 1);
        curr.pop_back();
      }
    };

    backtrack(backtrack, 0, 0);

    return ans;
  }
};
