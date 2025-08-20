// Leetcode: 17. Letter Combinations of a Phone Number
// @tag: hash-table, string, backtracking
// @difficulty: medium

#include <string>
#include <unordered_map>
#include <vector>

class Solution {
private:
  const std::string digitToLetterMap[8] = {"abc", "def",  "ghi", "jkl",
                                           "mno", "pqrs", "tuv", "wxyz"};

public:
  std::vector<std::string> letterCombinations(std::string &digits) {
    if (digits.empty()) {
      return {};
    }
    std::string first = digitToLetterMap[digits[0] - '0' - 2];
    std::vector<std::string> result = {""};
    for (int i = 0; i < digits.size(); ++i) {
      result = stringOperator(result, digitToLetterMap[digits[i] - '0' - 2]);
    }
    return result;
  }
  std::vector<std::string>
  stringOperator(const std::vector<std::string> &stringVec,
                 const std::string &newLetters) {
    if (newLetters.empty()) {
      return stringVec;
    }
    std::vector<std::string> result;
    for (const auto &prefix : stringVec) {
      for (const auto &newChar : newLetters) {
        result.emplace_back(prefix + newChar);
      }
    }
    return result;
  }
};