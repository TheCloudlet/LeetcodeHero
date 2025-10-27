// Leetcode 105. Construct Binary Tree from Preorder and Inorder Traversal
// @tags: array, hash-table, divide-and-conquer, tree, binary-tree, neetcode150
// @difficulty: medium

// NOTE:
// This is an O(N^2) solution because of the search of root in inorder array.
// We can optimize it to O(N) by using a hash map to store the value -> index
// mapping.
#include <vector>

class Solution {
public:
  TreeNode *buildTree(std::vector<int> &preorder, std::vector<int> &inorder) {
    assert(preorder.size() == inorder.size());
    int nodeCount = preorder.size();
    if (nodeCount == 0) {
      return nullptr;
    }
    return buildTreeHelper(preorder, 0, nodeCount - 1, inorder, 0,
                           nodeCount - 1);
  }

private:
  TreeNode *buildTreeHelper(std::vector<int> &preorder, size_t preStart,
                            size_t preEnd, std::vector<int> &inorder,
                            size_t inStart, size_t inEnd) {
    if (preStart > preEnd || inStart > inEnd) {
      return nullptr;
    }
    int rootVal = preorder[preStart];
    TreeNode *root = new TreeNode(rootVal);

    // Find the left and right region of inorder
    size_t inorderRootIdx = inStart;
    for (; inorderRootIdx <= inEnd; inorderRootIdx++) {
      if (inorder[inorderRootIdx] == rootVal) {
        break;
      }
    }

    size_t leftSubtreeSize = inorderRootIdx - inStart;

    root->left =
        buildTreeHelper(preorder, preStart + 1, preStart + leftSubtreeSize,
                        inorder, inStart, inorderRootIdx - 1);

    root->right = buildTreeHelper(preorder, preStart + leftSubtreeSize + 1,
                                  preEnd, inorder, inorderRootIdx + 1, inEnd);

    return root;
  }
};