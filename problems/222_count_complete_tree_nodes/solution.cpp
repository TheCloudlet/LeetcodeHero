// 222. Count Complete Tree Nodes

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
#if defined(WRONG_ANSWER)
// The time complexity is O(N), but the problem needs < N
class Solution {
 public:
  int countNodes(TreeNode* root) {
    if (!root) return 0;

    return 1 + countNodes(root->left) + countNodes(root->right);
  }
};
#endif

#if defined(DFS)
class Solution {
 public:
  int countNodes(TreeNode* root) {
    if (!root) return 0;

    int left_depth = 0;  // CAUTION
    const TreeNode* left_ptr = root;
    while (left_ptr) {
      left_ptr = left_ptr->left;
      ++left_depth;
    }

    int right_depth = 0;
    const TreeNode* right_ptr = root;
    while (right_ptr) {
      right_ptr = right_ptr->right;
      ++right_depth;
    }

    if (left_depth == right_depth) {
      // Return (2^n - 1)
      return (1 << left_depth) - 1;  // CAUTION
    }

    return 1 + countNodes(root->left) + countNodes(root->right);
  }
};
#endif
