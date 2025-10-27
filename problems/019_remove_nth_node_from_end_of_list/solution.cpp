// Leetcode 19. Remove Nth Node From End of List
// @tag: neetcode150, linked-list, two-pointers
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

/// Removes the nth node from the end of a linked list.
///
/// Uses a two-pointer approach with double pointers (pointer to pointer) for
/// elegant handling of all cases including removing the head.
///
/// \complexity O(n) time with a single pass, O(1) space
///
/// \details
/// 1. Creates a gap of n nodes between fast and slow pointers
/// 2. When fast reaches the end, slow points to the node before the target
/// 3. Using double pointers (**) simplifies edge cases like removing the head
ListNode *removeNthFromEnd(ListNode *head, int n) {
  // Using indirect pointers for more elegant pointer manipulation
  ListNode **fast = &head;
  ListNode **slow = &head;

  // Advance fast pointer n steps ahead
  int steps_taken = 0;
  for (; steps_taken < n && *fast; ++steps_taken) {
    fast = &(*fast)->next;
  }

  // Handle case where n is greater than list length
  if (steps_taken < n) {
    return head;
  }

  // Move both pointers until fast reaches the end
  // When fast points to nullptr, slow will point to the node before the nth
  // node from end
  while (*fast) {
    fast = &(*fast)->next;
    slow = &(*slow)->next;
  }

  // Remove the target node
  ListNode *nodeToDelete = *slow;
  *slow = (*slow)->next;
  delete nodeToDelete;

  return head;
}