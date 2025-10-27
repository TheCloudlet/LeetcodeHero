// Leetcode 852. Peak Index in a Mountain Array
// Binary search for peak in strictly increasing then decreasing array
//
// 我們的目標是：
//   找到第一個 index，使得 arr[i] > arr[i + 1]，也就是「轉折點」。
//   So while (left < right)

#include <vector>

int peakIndexInMountainArray(std::vector<int>& arr) {
  int left = 0, right = arr.size() - 1;
  while (left < right) {
    int mid = left + (right - left) / 2;
    if (arr[mid] < arr[mid + 1]) {
      left = mid + 1; // peak is to the right
    } else {
      right = mid;    // peak is at mid or to the left
    }
  }
  return left; // left == right == peak index
}
