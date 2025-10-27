// Leetcode 572. Subtree of Another Tree
// @tag: tree, dfs, binary-tree, neetcode150
// @difficulty: easy

// Serilize and KMP
#if defined(SERIALIZE_AND_KMP)
class Solution {
public:
  bool isSubtree(TreeNode *root, TreeNode *subRoot) {
    std::string s1, s2;
    serialize(root, s1);
    serialize(subRoot, s2);
    return s1.find(s2) != std::string::npos;
  }

private:
  void serialize(TreeNode *root, std::string &output) {
    if (!root) {
      output.append("#");
      return;
    }
    output.append(",");
    output.append(std::to_string(root->val));
    serialize(root->left, output);
    serialize(root->right, output);
  }
};
#endif

// Basic soution:
#if defined(BASIC)
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
#endif