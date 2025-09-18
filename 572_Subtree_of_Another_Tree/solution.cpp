// Leetcode 572. Subtree of Another Tree
// @tag: tree, dfs, binary-tree, neetcode150
// @difficulty: easy

class Solution {
public:
  bool isSubtree(TreeNode *root, TreeNode *subRoot) {
    if (!subRoot) {
      return true;
    }
    if (!root && subRoot) {
      return false;
    }
    // root and subRoot
    if (isSameTree(root, subRoot)) {
      return true;
    }
    return isSubtree(root->left, subRoot) || isSubtree(root->right, subRoot);
  }

private:
  bool isSameTree(TreeNode *s, TreeNode *t) {
    if (!s && !t) {
      return true;
    }
    if (s && !t || !s && t) {
      return false;
    }
    if (s->val != t->val) {
      return false;
    }
    return isSameTree(s->left, t->left) && isSameTree(s->right, t->right);
  }
};