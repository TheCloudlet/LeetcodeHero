// Leetcode 110. Balanced Binary Tree
// @tag: tree, binary-tree, dfs, neetcode150
// @difficulty: easy

// Definition of "balanced":
// A binary tree in which the left and right subtrees of every node differ in
// height by no more than 1.

#include <utility>

class Solution {
public:
  bool isBalanced(TreeNode *root) {
    auto [_, _isBalanced] = getHeightAndBalanced(root);
    return _isBalanced;
  }

private:
  std::pair<int, bool> getHeightAndBalanced(TreeNode *root) {
    if (!root) {
      return {0, true};
    }

    auto [heightL, isBalancedL] = getHeightAndBalanced(root->left);
    if (!isBalancedL) {
      return {-1, false};
    }
    auto [heightR, isBalancedR] = getHeightAndBalanced(root->right);
    if (!isBalancedR) {
      return {-1, false};
    }

    if (std::abs(heightL - heightR) > 1) {
      return {-1, false};
    }

    return {1 + std::max(heightL, heightR), true};
  }
};
