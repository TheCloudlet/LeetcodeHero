# A First-Principles Guide to Problem Solving

This guide is a living document intended to build a "thought map" for tackling algorithmic challenges. Instead of memorizing individual solutions, we focus on identifying the underlying principles—the "first principles"—that govern entire classes of problems.

## Principle 1: Monotonicity as a Tool for Elimination

### What is Monotonicity?

At its core, monotonicity describes a predictable, **one-way trend**. A sequence, function, or property is monotonic if it only ever moves in one direction—it never doubles back on itself.

This "one-way" behavior can be categorized precisely:

- **Non-decreasing**: Values can increase or stay the same, but never decrease. (e.g., `[1, 2, 2, 5, 8]`)
- **Non-increasing**: Values can decrease or stay the same, but never increase. (e.g., `[10, 5, 5, 2, 1]`)
- **Strictly Increasing**: Values must always increase. (e.g., `[1, 2, 5, 8, 10]`)
- **Strictly Decreasing**: Values must always decrease. (e.g., `[10, 8, 5, 2, 1]`)

### The Core Strategy: Pruning Candidates

This is the central insight for problem-solving:

> **When a problem exhibits any form of monotonicity, it provides a predictable order. This order allows for the aggressive elimination of non-viable candidates.**

Instead of brute-forcing every possibility, the monotonic property gives us the confidence to discard large portions of the search space, often leading to highly efficient solutions (e.g., from $O(N^2)$ to $O(N \log N)$ or $O(N)$).

### Common Algorithmic Manifestations

Here is how this principle of elimination manifests in common algorithms and data structures:

- **Binary Search**
  - **Eliminates:** Half of the remaining search space with every single comparison.
  - **Applies to:** Sorted data or monotonic answer spaces.

- **Two Pointers / Sliding Window**
  - **Eliminates:** The element at the lagging pointer when the window no longer satisfies a condition.
  - **Applies to:** Problems on contiguous subarrays where a property (like sum or count) changes monotonically as the window expands.

- **Monotonic Stack**
  - **Eliminates:** Previous elements from the stack that are rendered irrelevant or "blocked" by the current element's arrival.
  - **Applies to:** "Next/previous greater/smaller element" type problems.

- **Monotonic Queue (Deque)**
  - **Eliminates:** Older elements that are no longer in the current window (from the front) and suboptimal elements that are worse than the current element (from the back).
  - **Applies to:** Sliding window minimum/maximum problems.


