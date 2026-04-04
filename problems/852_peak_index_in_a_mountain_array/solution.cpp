// Leetcode 852. Peak Index in a Mountain Array
// Binary search for peak in strictly increasing then decreasing array

#include <vector>

class Solution {
 public:
  int peakIndexInMountainArray(const std::vector<int>& arr) {
    if (arr.empty()) return -1;

    int left = 0;
    int right = static_cast<int>(arr.size() - 1);  // guarantee peak exsists

    while (left < right) {
      const int mid = left + (right - left) / 2;
      if (arr[mid] < arr[mid + 1])
        left = mid + 1;  // peak is to the right
      else
        right = mid;  // peak is at mid or to the left
    }

    return right;
  }
};
