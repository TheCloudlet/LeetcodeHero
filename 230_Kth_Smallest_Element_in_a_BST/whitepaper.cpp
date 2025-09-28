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

class Solution {
public:
  int kthSmallest(TreeNode *root, int k) {
    if (!root) {
      return -1;
    }

    std::stack<TreeNode *> nodeStack;
    TreeNode *curr = root;

    while (curr || !nodeStack.empty()) {
      while (curr) {
        nodeStack.push(curr);
        curr = curr->left;
      }

      curr = nodeStack.top();
      nodeStack.pop();

      --k;
      if (k <= 0) {
        return curr->val;
      }

      curr = curr->right;
    }

    return -1;
  }
};