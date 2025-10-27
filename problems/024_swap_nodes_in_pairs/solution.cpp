// Leetcode 24. Swap Nodes in Pairs

// Definition for singly-linked list.
struct ListNode {
  int val;
  ListNode *next;
  ListNode() : val(0), next(nullptr) {}
  ListNode(int x) : val(x), next(nullptr) {}
  ListNode(int x, ListNode *next) : val(x), next(next) {}
};

class Solution {
public:
  ListNode *swapPairs(ListNode *head) {
    if (!head) {
      return nullptr;
    }

    ListNode dummyHead = ListNode(0, head);
    ListNode *prev = &dummyHead;
    ListNode *first;
    ListNode *second;
    while (prev) {
      first = prev->next;
      if (!first) {
        break; // Zero element left
      }
      second = first->next;
      if (!second) {
        break; // The last pair only has 1 element
      }
      prev->next = second;
      first->next = second->next;
      second->next = first;
      prev = first;
    }
    return dummyHead.next;
  }
};
