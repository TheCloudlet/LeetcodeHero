// Leetcode 226. Invert Binary Tree
// @tag: tree, dfs, bfs, neetcode150
// @difficulty: easy

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left),
 * right(right) {}
 * };
 */

#if defined(SWAP)
#include <algorithm>  // for std::swap

class Solution {
 public:
  TreeNode* invertTree(TreeNode* root) {
    if (!root) {
      return nullptr;
    }
    std::swap(root->left, root->right);
    invertTree(root->left);
    invertTree(root->right);
    return root;
  }
};
#endif

#if defined(DIRECT)
#include <algorithm>  // for std::swap

class Solution {
 public:
  TreeNode* invertTree(TreeNode* root) {
    if (!root) return nullptr;

    TreeNode* tmp = root->left;
    root->left = invertTree(root->right);
    root->right = invertTree(tmp);

    return root;
  }
};
#endif
