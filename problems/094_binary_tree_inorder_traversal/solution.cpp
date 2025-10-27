// Leetcode 94. Binary Tree Inorder Traversal
// @tag: tree, dfs, stack
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

#include <vector>
#include <stack>

class Solution {
public:
  std::vector<int> inorderTraversal(TreeNode *root) {
    if (!root) {
      return {};
    }
    std::vector<int> result;

    TreeNode *curr = root;
    std::stack<TreeNode *> traversalStack;

    while (curr || !traversalStack.empty()) {
      while (curr) {
        traversalStack.push(curr);
        curr = curr->left;
      }

      curr = traversalStack.top();
      traversalStack.pop();
      result.push_back(curr->val);

      curr = curr->right;
    }

    return result;
  }
};