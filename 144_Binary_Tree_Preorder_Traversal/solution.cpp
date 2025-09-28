// Leetcode 144. Binary Tree Preorder Traversal
// @tag: tree, dfs, traversal
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

#include <stack>
#include <vector>

class Solution {
public:
  std::vector<int> preorderTraversal(TreeNode *root) {
    if (!root) {
      return {};
    }

    std.vector<int> result;
    std.stack<TreeNode *> traversalStack;
    traversalStack.push(root);

    while (!traversalStack.empty()) {
      TreeNode *curr = traversalStack.top();
      traversalStack.pop();

      result.push_back(curr->val); // Process (Root)

      // Push right child first, so left is processed first
      if (curr->right) {
        traversalStack.push(curr->right); // (Right)
      }
      if (curr->left) {
        traversalStack.push(curr->left); // (Left)
      }
    }
    return result;
  }
};