// Leetcode 56. Merge Intervals

#include <vector>

class Solution {
 public:
  std::vector<std::vector<int>> merge(
      std::vector<std::vector<int>>& intervals) {
    if (intervals.empty()) {
      return {};
    }

    // sort interval
    std::sort(intervals.begin(), intervals.end(),
              [](const std::vector<int>& a, const std::vector<int>& b) {
                return a.at(0) < b.at(0);
              });  // NOTE: don't forget const and & here

    std::vector<std::vector<int>> result;
    result.reserve(intervals.size());  // Reserve space for performance

    for (const auto& interval : intervals) {
      if (result.empty() || result.back()[1] < interval[0]) {
        result.emplace_back(interval);
      } else {
        // merge
        // NOTE: Using [] instead of .at() for performance, since bounds are
        // guaranteed safe here. .at() provides runtime checks and is better
        // during debugging or with dynamic input.
        result.back()[1] = std::max(result.back()[1], interval[1]);
      }
    }

    return result;
  }
};
