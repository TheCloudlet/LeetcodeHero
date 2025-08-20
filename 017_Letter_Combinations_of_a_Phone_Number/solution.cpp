// Leetcode: 17. Letter Combinations of a Phone Number
// @tag: hash-table, string, backtracking
// @difficulty: medium
//
// NOTE:
//
// This iterative solution is essentially a left fold (`foldl` in Haskell).
// It starts with an initial value (a list containing an empty string) and
// repeatedly applies a combination function for each digit.
//
// The canonical solution, however, uses backtracking. This is more akin to
// the way a recursive list comprehension or a monadic `do` block in Haskell
// would solve it, by defining how to combine one character with the result
// of the recursive call on the rest of the digits.

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