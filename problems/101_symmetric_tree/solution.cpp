// Leetcode 101. Symmetric Tree

class Solution {
 public:
  bool isSymmetric(TreeNode* root) { return is_mirror(root, root); }

 private:
  bool is_mirror(TreeNode* a, TreeNode* b) {
    if (!a && !b) return true;

    if (!a || !b) return false;  // if (!a || !b) return (!a && !b);

    // CAUTION: Arm's-length recursion
    // Don't write `a->left->val != b->right->val`.
    if (a->val != b->val) return false;

    return is_mirror(a->left, b->right) && is_mirror(a->right, b->left);
  }
};
