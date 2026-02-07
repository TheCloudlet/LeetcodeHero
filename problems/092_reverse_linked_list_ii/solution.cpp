// Leetcode 92. Reverse Linked List II

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     ListNode *next;
 *     ListNode() : val(0), next(nullptr) {}
 *     ListNode(int x) : val(x), next(nullptr) {}
 *     ListNode(int x, ListNode *next) : val(x), next(next) {}
 * };
 */

#if defined(firsr_attempt)
// Biggest mistake:
// I treated the `head` node as a guaranteed existence, ignoring the case where
// the `head` itself needs to be moved/reversed.
//
// - Case: `left == 1`
// - Result: `prev` is nullptr -> Crash on re-linking.
//
// Will the identity of the head pointer change after this operation?
// - Yes: Use dummy node

class Solution {
 public:
  ListNode* reverseBetween(ListNode* head, int left, int right) {
    if (!head || left == right) {
      return head;
    }

    // NOTE: I made miskake here
    ListNode* dummy = new ListNode(0, head);

    ListNode* prev = dummy;  // Alwasy not nullptr
    ListNode* curr = head;

    int pos = 1;

    while (pos < left && curr) {
      prev = curr;
      curr = curr->next;
      ++pos;
    }

    if (!curr) {
      return head;
    }

    ListNode* a = prev;
    ListNode* b = curr;

    prev = curr;
    curr = curr->next;
    ++pos;

    while (pos < right + 1 && curr) {
      ListNode* next = curr->next;
      curr->next = prev;
      prev = curr;
      curr = next;
      ++pos;
    }

    a->next = prev;
    b->next = curr;

    return dummy->next;
  }
};
#endif

#if defined(better_taste)

class Solution {
 public:
  ListNode* reverseBetween(ListNode* head, int left, int right) {
    ListNode** pivot = &head;

    // Move the pivot to the "prev->next" of the start reversing node
    for (int i = 0; i < left - 1; ++i) {
      pivot = &((*pivot)->next);
    }

    ListNode* tail = *pivot;

    // Reverse link list
    ListNode* prev = nullptr;
    ListNode* curr = tail;
    ListNode* next = nullptr;

    for (int i = 0; i < right - left + 1; ++i) {
      next = curr->next;
      curr->next = prev;
      prev = curr;
      curr = next;
    }

    // Relink
    *pivot = prev;
    tail->next = curr;

    return head;
  }
};
#endif
