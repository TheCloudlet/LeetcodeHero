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
  ListNode* addTwoNumbers(ListNode* a, ListNode* b) {
    if (!a || !b) return a ? a : b;

    ListNode* ans = nullptr;
    ListNode** nextp = &ans;

    int carry = 0;
    while (a || b || carry) {
      const int sum = (a ? a->val : 0) + (b ? b->val : 0) + carry;

      *nextp = new ListNode(sum % 10);  // CAUTION: not &(new ListNode())
      carry = sum / 10;

      if (a) a = a->next;
      if (b) b = b->next;
      nextp = &((*nextp)->next);
    }

    return ans;
  }
};
