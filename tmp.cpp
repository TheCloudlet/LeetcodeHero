
// 226 Invert Binary Tree

TreeNode* invert(TreeNode* root) {
  if (!root) return nullptr;

  TreeNode* tmp = root->left;
  root->left = invert(root->right);
  root->right = invert(tmp);

  return root;
}
