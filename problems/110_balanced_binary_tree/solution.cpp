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

// Gemini's comment: We can just return -1 when unbalanced, and height
// otherwise. So we only need one return value, not a pair.
#if defined(GEMINI)
class Solution {
public:
  bool isBalanced(TreeNode *root) { return checkHeight(root) != -1; }

private:
  // Returns the height of the tree if it's balanced.
  // Otherwise, returns -1.
  int checkHeight(TreeNode *root) {
    if (!root) {
      return 0; // Height of a null tree is 0
    }

    // Recursively check left subtree
    int heightL = checkHeight(root->left);
    if (heightL == -1) {
      return -1; // Left is unbalanced, propagate failure up
    }

    // Recursively check right subtree
    int heightR = checkHeight(root->right);
    if (heightR == -1) {
      return -1; // Right is unbalanced, propagate failure up
    }

    // Check current node's balance
    if (std::abs(heightL - heightR) > 1) {
      return -1; // Current node is unbalanced
    }

    // Return height if balanced
    return 1 + std::max(heightL, heightR);
  }
};
#endif