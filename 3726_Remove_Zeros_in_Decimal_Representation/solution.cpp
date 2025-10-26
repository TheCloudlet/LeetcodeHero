// Leetcode 3726. Remove Zeros in Decimal Representation
// @tag: string, weekly-contest
// @difficulty: easy

#include <string>

class Solution {
 public:
  long long removeZeros(long long n) {
    std::string num_str = std::to_string(n);
    std::string result_without_zeros;
    result_without_zeros.reserve(num_str.length());

    for (const auto& digit : num_str) {
      if (digit != '0') {
        result_without_zeros.push_back(digit);
      }
    }

    return std::stoll(result_without_zeros);
  }
};