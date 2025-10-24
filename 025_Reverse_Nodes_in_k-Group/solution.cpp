// Leetcode 25. Reverse Nodes in k-Group
// @tag: linked-list
// @difficulty: hard

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
class Solution {
 public:
  ListNode* reverseKGroup(ListNode* head, int k) {
    // If k is 1, no reversal is needed.
    if (k <= 1 || !head) {
      return head;
    }

    ListNode* new_head = head;
    // `indirect_tail` is the "linker" pointer. It points to the `next`
    // pointer of the *last* node of the previous group.
    // Initially, it points to new_head itself.
    ListNode** indirect_tail = &new_head;

    ListNode* group_head = head;

    while (group_head) {
      // Walk k steps further and check whether we have another group
      ListNode* next_group_start = group_head;
      int count = 0;
      while (count < k && next_group_start) {
        next_group_start = next_group_start->next;
        count++;
      }

      // Check if this is the last group
      if (count < k) {
        *indirect_tail = group_head;
        break;
      }

      // Reverse the group
      ListNode* prev = next_group_start;  // KEY TRICK: Start prev points to the
                                          // next group's head
      ListNode* curr = group_head;
      for (int i = 0; i < k; i++) {
        ListNode* next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
      }

      // Link-up
      *indirect_tail = prev;

      // Setup indirect_tail and group_head for next iteration
      indirect_tail = &(group_head->next);
      group_head = next_group_start;
    }

    return new_head;
  }
};