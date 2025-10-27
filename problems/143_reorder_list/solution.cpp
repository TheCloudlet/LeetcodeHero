// Leetcode 143. Reorder List
// @tag: linked-list, two-pointer, fast-and-slow-pointer, reverse, neetcode150
// @difficulty: medium

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
  void reorderList(ListNode *head) {
    if (!head || !head->next) {
      return;
    }

    // Step 1: Find the middle of the list
    ListNode *slow = head;
    ListNode *fast = head;

    // Use fast->next && fast->next->next instead of fast && fast->next.
    // This ensures we check two nodes ahead before advancing fast by 2 steps,
    // stopping slow at the END of first half rather than beginning of second.
    //
    // Wrong condition (fast && fast->next) creates uneven splits:
    //   [1,2,3,4,5] -> [1,2] and [3,4,5] (first half too short)
    // Correct condition ensures proper splits:
    //   [1,2,3,4,5] -> [1,2,3] and [4,5] (first half >= second half)
    while (fast->next && fast->next->next) {
      slow = slow->next;
      fast = fast->next->next;
    }

    // Step 2: Split the list into two parts
    ListNode *second = slow->next;
    slow->next = nullptr;  // Cut the connection

    // Step 3: Reverse the second half
    ListNode *prev = nullptr;
    ListNode *curr = second;

    while (curr) {
      ListNode *next = curr->next;
      curr->next = prev;
      prev = curr;
      curr = next;
    }

    // Step 4: Merge the two lists alternately (in-place)
    ListNode *first = head;
    second = prev;  // prev is now the head of reversed second half
    ListNode *list1 = first;
    ListNode *list2 = second;

    while (second) {
      ListNode *temp1 = first->next;
      ListNode *temp2 = second->next;

      first->next = second;
      second->next = temp1;

      first = temp1;
      second = temp2;
    }
  }
};