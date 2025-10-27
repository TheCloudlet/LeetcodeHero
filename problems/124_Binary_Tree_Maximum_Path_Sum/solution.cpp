// Leetcode 124. Binary Tree Maximum Path Sum
// @tag: tree, depth-first search, recursion, neetcode150
// @difficulty: hard

// Thinking
// Sub problem:
// max(left-weight-to-leaf + curr + right-weight-to-leaf, always max)

#include <algorithm>
#include <limits>
#include <utility>

class Solution {
public:
  int maxPathSum(TreeNode *root) {
    auto [maxPath, _] = getPathInfo(root);
    return maxPath;
  }

private:
  // @return: {maxPath, maxPathToLeaf}
  std::pair<int, int> getPathInfo(TreeNode *node) {
    if (!node) {
      // Base case: Smallest possible max, 0 gain for parent.
      return {std::numeric_limits<int>::min(), 0};
    }

    auto [lMax, lPathToLeaf] = getPathInfo(node->left);
    auto [rMax, rPathToLeaf] = getPathInfo(node->right);

    // NOTE: Discard negative downward paths. This is the key insight.
    //
    // For example
    //         A (val: 10)
    //        /  \
    //    -100    5
    //
    // => If we don't discard -100, the max in subtree A will be (-85).
    //    But the correct anser will be 15
    int leftGain = std::max(0, lPathToLeaf);
    int rightGain = std::max(0, rPathToLeaf);

    int pathToLeaf = node->val + std::max(leftGain, rightGain);
    int max = std::max({lMax, rMax, leftGain + rightGain + node->val});

    return {max, pathToLeaf};
  }
};