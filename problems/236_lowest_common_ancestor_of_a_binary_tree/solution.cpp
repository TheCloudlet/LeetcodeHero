// Leetcode 236. Lowest Common Ancestor of a Binary Tree
//
// Two cases:
// (1)   p    (2)  x
//     /          / \
//    q          p   q
//
// Key insight: p and q are guaranteed to exist in the tree.
// If we hit p or q first, it must be the LCA (Case 1).
class Solution {
 public:
  TreeNode* lowestCommonAncestor(TreeNode* root, TreeNode* p, TreeNode* q) {
    if (!root || root == p || root == q) {
      return root;
    }

    TreeNode* left = lowestCommonAncestor(root->left, p, q);
    TreeNode* right = lowestCommonAncestor(root->right, p, q);

    if (left && right) {  // p and q found in different subtrees
      return root;
    }

    return left ? left : right;
  }
};
