// Leetcode 2. Add Two Numbers
// @tag: neetcode150, linked-list
// @difficulty: medium
//
// NOTE:
// 1. Practice indirect pointer usage
// 2. Handle same patten together

// Definition for singly-linked list.
// struct ListNode {
//   int val;
//   ListNode *next;
//   ListNode() : val(0), next(nullptr) {}
//   ListNode(int x) : val(x), next(nullptr) {}
//   ListNode(int x, ListNode *next) : val(x), next(next) {}
// };

class Solution {
public:
  ListNode *addTwoNumbers(ListNode *l1, ListNode *l2) {
    ListNode *resultHead = nullptr;
    ListNode **indirect = &resultHead;
    int c = 0; // carry

    while (l1 || l2 || c) {
      int a = l1 ? l1->val : 0;
      int b = l2 ? l2->val : 0;
      int sum = (a + b + c);
      c = (a + b + c) / 10;

      (*indirect) = new ListNode(sum % 10);
      indirect = &(*indirect)->next;

      if (l1)
        l1 = l1->next;
      if (l2)
        l2 = l2->next;
    }
    return resultHead;
  }
};
