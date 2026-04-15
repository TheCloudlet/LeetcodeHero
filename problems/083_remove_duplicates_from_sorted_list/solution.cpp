// Leetcode 83. Remove Duplicates from Sorted List

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
  ListNode* deleteDuplicates(ListNode* head) {
    if (!head) return nullptr;

    constexpr int INF = 1e9;
    int prev_val = INF;

    ListNode** indirect = &head;

    while (*indirect) {
      if ((*indirect)->val == prev_val) {
        ListNode* to_remove = *indirect;
        *indirect = to_remove->next;
        delete to_remove;
      } else {
        prev_val = (*indirect)->val;
        indirect = &((*indirect)->next);
      }
    }

    return head;
  }
};
