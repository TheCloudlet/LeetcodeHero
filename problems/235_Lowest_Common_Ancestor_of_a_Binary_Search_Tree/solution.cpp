// Leetcode 235. Lowest Common Ancestor of a Binary Search Tree
// @tag: tree, binary-search-tree, dfs, neetcode150
// @difficulty: medium

// Reframe the quetion. How can we find a node that splits p->val and q->val
#if defined(RECURSION)
class Solution {
public:
  TreeNode *lowestCommonAncestor(TreeNode *root, TreeNode *p, TreeNode *q) {
    if (!root || !p || !q) {
      return nullptr;
    }
    if (root == p || root == q) { // NOTE: this is redundant
      return root;
    }
    // If both nodes are in the right subtree
    if (root->val < p->val && root->val < q->val) {
      return lowestCommonAncestor(root->right, p, q);
    }
    // If both nodes are in the left subtree
    if (root->val > p->val && root->val > q->val) {
      return lowestCommonAncestor(root->left, p, q);
    }
    return root;
  }
};
#endif

#if defined(ITERATION)
class Solution {
public:
  TreeNode *lowestCommonAncestor(TreeNode *root, TreeNode *p, TreeNode *q) {
    if (!root || !p || !q) {
      return nullptr;
    }
    TreeNode *curr = root;
    while (curr) {
      if (curr == p || curr == q) {
        return curr; // NOTE: this is redundant
      }
      if (curr->val < p->val && curr->val < q->val) {
        curr = curr->right;
      } else if (curr->val > p->val && curr->val > q->val) {
        curr = curr->left;
      } else {
        return curr;
      }
    }
    return nullptr;
  }
};
#endif
