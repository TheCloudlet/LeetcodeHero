// Leetcode 61. Rotate List
// @tag: linked-list
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

#include <cstddef>

class Solution {
public:
  ListNode *rotateRight(ListNode *head, int k) {
    if (!head || !head->next) {
      return head;
    }
    ListNode **indirect = &head;
    ListNode *tail = head;
    std::size_t listLength = 0;

    // Chain the linked list to a circular linked list
    while (tail->next) {
      tail = tail->next;
      ++listLength;
    }
    ++listLength; // Count the last node

    // OPTIMIZATION: This early return skips making the list circular - safe
    // since no rotation needed
    if (k % listLength == 0) {
      return head;
    }

    tail->next = (*indirect);
    indirect = &tail->next;

    // Calculate rotation point and move to new head position:
    std::size_t rotationOffset = listLength - k % listLength;
    for (std::size_t pos = 0; pos < rotationOffset; ++pos) {
      indirect = &(*indirect)->next;
    }

    ListNode *newHead = *indirect;
    (*indirect) = nullptr;

    return newHead;
  }
};