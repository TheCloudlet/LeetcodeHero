// Leetcode 647. Palindromic Substrings
// @tag: dp, string, neetcode150
// @difficulty: medium

#include <string>
#include <utility> // For std::pair and std::move
#include <vector>

class Solution {
public:
  int countSubstrings(const std::string &s) {
    int n = s.size();
    if (n <= 1) {
      return n;
    }

    // Layer 1: All single characters are palindromes.
    int palindromeCount = n;
    // layerKMinus2 holds the results for substrings of length (k-2).
    // Initially, k=3, so this holds results for length 1.
    std::vector<bool> layerKMinus2(n, true);

    // Layer 2: Check all adjacent character pairs.
    // layerKMinus1 holds the results for substrings of length (k-1).
    // Initially, k=3, so this holds results for length 2.
    std::vector<bool> layerKMinus1(n - 1, false);
    int prevLayerCount = 0; // For the early exit optimization
    for (int pos = 0; pos < n - 1; ++pos) {
      if (s[pos] == s[pos + 1]) {
        layerKMinus1[pos] = true;
        ++palindromeCount;
        ++prevLayerCount;
      }
    }

    // Layers >= 3: The main DP loop.
    for (int k = 3; k <= n; ++k) {
      auto [currentLayer, currentLayerCount] = buildLayer(s, k, layerKMinus2);

      if (currentLayerCount == 0 && prevLayerCount == 0) {
        break;
      }

      palindromeCount += currentLayerCount;
      prevLayerCount = currentLayerCount;

      layerKMinus2 = std::move(layerKMinus1);
      layerKMinus1 = std::move(currentLayer);
    }

    return palindromeCount;
  }

private:
  // This is now a pure function with no side effects.
  // It is marked const because it doesn't modify the object's state.
  std::pair<std::vector<bool>, int>
  buildLayer(const std::string &s, int k,
             const std::vector<bool> &layerKMinus2) const {
    int n = s.size();
    std::vector<bool> currentLayer(n - k + 1, false);
    int count = 0;

    for (int startPos = 0; startPos <= n - k; ++startPos) {
      // The core DP logic: check outer chars and the inner substring's status.
      if (s[startPos] == s[startPos + k - 1] && layerKMinus2[startPos + 1]) {
        currentLayer[startPos] = true;
        ++count;
      }
    }

    return {std::move(currentLayer), count};
  }
};