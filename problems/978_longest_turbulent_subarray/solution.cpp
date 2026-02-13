// LeetCode 978. Longest Turbulent Subarray
//
// Sliding window requires maintaining a valid window. Initial attempt
// used enum class for state tracking, which was verbose; switching to a
// single char ('>', '<', '=') for the previous comparison simplifies the
// implementation.
//
// Alternative approach: DP using two variables (inc, dec) is more
// interview-friendly; see editorial. (Kaden's Algorithm)
//
// Analysis:
//   - Time:  O(N)
//   - Space: O(1)
// where N is the size of the array

#include <algorithm>
#include <vector>

class Solution {
 public:
  int maxTurbulenceSize(const std::vector<int>& arr) {
    if (arr.empty()) {
      return 0;
    }

    int max_window = 1;

    // '>' means the [right - 2] > [right - 1]
    // '=' means the [right - 2] = [right - 1]
    // '<' means the [right - 2] < [right - 1]
    char prev = '=';

    int left = 0;
    for (int right = 1; right < static_cast<int>(arr.size()); ++right) {
      if (arr[right - 1] == arr[right]) {
        // Invalid window: equal elements
        left = right;
        prev = '=';
        continue;
      }
      if (arr[right - 1] > arr[right] && prev == '>') {
        // Invalid window: decreasing order
        left = right - 1;
        prev = '>';
        continue;
      }
      if (arr[right - 1] < arr[right] && prev == '<') {
        // Invalid window: increasing order
        left = right - 1;
        prev = '<';
        continue;
      }

      // Valid window: update max window size and record current direction
      max_window = std::max(max_window, right - left + 1);
      prev = (arr[right - 1] > arr[right]) ? '>' : '<';  // Remember!
    }

    return max_window;
  }
};
