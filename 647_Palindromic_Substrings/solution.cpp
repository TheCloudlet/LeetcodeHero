// Leetcode 647. Palindromic Substrings
// @tag: dynamic-programming, string, neetcode
// @difficulty: medium

class Solution {
public:
  int countSubstrings(const std::string &s) {
    if (s.empty()) {
      return 0;
    }
    int n = s.size();
    if (n == 1) {
      return n;
    }

    std::vector<bool> layerKMinus2(n, true);
    int palindromeCount = n;

    std::vector<bool> layerKMinus1(n - 1, false);
    for (int pos = 0; pos < n - 1; ++pos) {
      if (s[pos] == s[pos + 1]) {
        layerKMinus1[pos] = true;
        ++palindromeCount;
      }
    }

    for (int k = 3; k <= n; ++k) {
      int currentLayerCount = buildLayer(s, k, layerKMinus2, layerKMinus1);
      palindromeCount += currentLayerCount;
    }
    return palindromeCount;
  }

private:
  int buildLayer(const std::string &s, int k, std::vector<bool> &layerKMinus2,
                 std::vector<bool> &layerKMinus1) {
    int n = s.size();
    assert(k >= 3);
    std::vector<bool> currentLayer(n - k + 1, false);

    int layerPalindromeCount = 0;
    for (int startPos = 0; startPos <= n - k; ++startPos) {
      if (s[startPos] == s[startPos + k - 1] && layerKMinus2[startPos + 1]) {
        currentLayer[startPos] = true;
        ++layerPalindromeCount;
      }
    }

    layerKMinus2 = std::move(layerKMinus1);
    layerKMinus1 = std::move(currentLayer);
    return layerPalindromeCount;
  }
};
