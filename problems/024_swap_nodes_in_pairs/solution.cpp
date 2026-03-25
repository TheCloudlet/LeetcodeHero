// Leetcode 24. Swap Nodes in Pairs

// Definition for singly-linked list.
// struct ListNode {
//  int val;
//  ListNode* next;
//  ListNode() : val(0), next(nullptr) {}
//  ListNode(int x) : val(x), next(nullptr) {}
//  ListNode(int x, ListNode* next) : val(x), next(next) {}
//};

#if defined(DUMMY_NODE)
class Solution {
 public:
  ListNode* swapPairs(ListNode* head) {
    ListNode dummy(0);
    dummy.next = head;
    ListNode* prev = &dummy;

    while (prev->next && prev->next->next) {
      ListNode* first = prev->next;
      ListNode* second = first->next;

      // Swap
      first->next = second->next;
      second->next = first;
      prev->next = second;

      // Move forward
      prev = first;
    }

    return dummy.next;
  }
};
#endif

#if defined(INDIRECT)
class Solution {
 public:
  ListNode* swapPairs(ListNode* head) {
    ListNode** indirect = &head;

    while (*indirect && (*indirect)->next) {
      ListNode* first = *indirect;
      ListNode* second = (*indirect)->next;

      // Swap
      first->next = second->next;
      second->next = first;
      *indirect = second;

      indirect = &(first->next);
    }

    return head;  // CAUTION
  }
};
#endif
