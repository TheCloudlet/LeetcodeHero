// Leetcode 22. Generate Parentheses
// @tag: string, backtracking, dp
// @difficulty: medium
#include <cassert>
#include <string>
#include <vector>

class Solution {
 public:
  std::vector<std::string> generateParenthesis(int n) {
    assert(n >= 0);
    std::vector<std::string> candidates;
    std::string currentStr;
    backtrack(candidates, currentStr, 0, 0, n);
    return candidates;
  }

 private:
  // NOTE: Normally the arguments should be in this converntion order:
  // [Output] -> [States] -> [Constants/Limits]
  void backtrack(std::vector<std::string>& candidates, std::string& currentStr,
                 int openCount, int closeCount, int maxPairs) {
    assert(openCount >= closeCount);
    if (currentStr.length() == maxPairs * 2) {
      candidates.push_back(currentStr);
      return;
    }

    if (openCount < maxPairs) {  // <-- WARNING
      currentStr.push_back('(');
      backtrack(candidates, currentStr, openCount + 1, closeCount, maxPairs);
      currentStr.pop_back('(');
    }

    if (closeCount < openCount) {
      currentStr.push_back(')');
      backtrack(candidates, currentStr, openCount, closeCount + 1, maxPairs);
      currentStr.pop_back(')');
    }
  }
};