// Leetcode 3703. Remove K-Balanced Substrings
// @tag: stack, string, weekly-contest470
// @difficulty: medium

#if defined(SOLUTION_1)
#include <string>
#include <vector>

class Solution {
public:
  std::string removeSubstring(std::string s, int k) {
    std::vector<std::pair<char, int>> intermidiate;
    intermidiate.reserve(s.length());

    for (const auto &ch : s) {
      if (intermidiate.empty() && ch == '(') {
        intermidiate.push_back({ch, 1});
        continue;
      } else if (intermidiate.empty() && ch == ')') {
        intermidiate.push_back({ch, 0});
        continue;
      }

      auto [prevCh, cnt] = intermidiate.back();

      if (ch == '(') {
        if (prevCh == '(') {
          intermidiate.push_back({ch, cnt + 1});
        } else {
          intermidiate.push_back({ch, 1});
        }
      } else { // ch == ')'
        if (prevCh == '(' && cnt >= k) {
          intermidiate.push_back({ch, 1});
        } else if (prevCh == '(' && cnt < k) {
          intermidiate.push_back({ch, 0});
        } else if (prevCh == ')' && cnt != 0) {
          intermidiate.push_back({ch, cnt + 1});
        } else { // prevCh == ')' && cnt == -1
          intermidiate.push_back({ch, 0});
        }
      }

      if (intermidiate.back().first == ')' && intermidiate.back().second == k) {
        for (int idx = 0; idx < k * 2; ++idx) {
          intermidiate.pop_back();
        }
      }
    }

    std::string result;
    result.reserve(intermidiate.size());
    for (const auto &[ch, _] : intermidiate) {
      result.push_back(ch);
    }

    return result;
  }
};
#endif

// NOTE: There are also some fun solutions
// 1. Directly use string as stack
// 2. Use two vectors to record "(" and ")" consecutive counts respectively
// ...