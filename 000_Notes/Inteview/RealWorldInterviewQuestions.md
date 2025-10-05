---
title: "Real World Interview Questions"
author: "Yi-Ping Pan (Cloudlet)"
description: "A collection of real-world interview questions and solutions"
version: "1.0"
created: "2025-10-02"
updated: "2025-10-05"
format: "markdown"
language: "en"
status: "draft"
---

# Real World Interview Questions

## 2025-10-02

### Problem 1:

**Problem Statement:**

Given an array of integers, find three numbers whose product is maximum and return the maximum $(A - B) * C$.

1. A, B, C are different indices, and A < B < C
2. The array may contain negative numbers
3. The array length is at least 3
4. The array is not sorted

Example:

Input: [5,4,6,2,1] <-- Max = $(5 - 4) * 6 = 6$

**Solution:**

Use a two-pass approach:

Dynamic Programming + Prefix and Suffix Arrays

1. Prepare two auxiliary arrays, `leftMax` and `rightMax`. `leftMax[i]` will store the maximum value to the left of index `i`, and `rightMax[i]` will store the maximum value to the right of index `i`.
2. Iterate through the array from left to right to fill `leftMax`, and from right to left to fill `rightMax`.
3. Finally, iterate through the array again (from index 1 to n-2) to calculate the maximum value of `(leftMax[i] - nums[i]) * rightMax[i]`.
4. Return the indicies from maximum. `{leftMax[i], i , rightMax[i]}`

### Problem 2: Rectangle Coverage

**Problem Statement:**

Given N blue rectangles defined by their bottom-left and top-right coordinates (x1, y1, x2, y2), when provided with the coordinates of a yellow rectangle (x1', y1', x2', y2'), determine if the blue rectangles completely cover the yellow rectangle (True or False). For visualization, the yellow rectangle is represented with dashed lines, but like the blue rectangles, both are standard rectangles. Value range M [0, 10^9], number of rectangles N [0, 10^5]

**My Solution:**

1. Reindex the coordinates to set the upper-left corner as the origin (0,0) and the lower-right corner as (M, N). This simplifies the coordinate system and makes it easier to work with. Reduce the 2D problem to a series of 1D problems by sweeping a horizontal line from the top to the bottom of the yellow rectangle. The question becomes: can the blue rectangles cover the horizontal line segment of the yellow rectangle? [0, M * N - 1]
2. Re-index every blue rectangle and the yellow rectangle based on the new coordinate system. Then use min/max to reset the boundaries of the blue rectangles to ensure they are within the yellow rectangle's boundaries.
3. Output the possible horizontal line segments in a list.
4. For each blue rectangle, add the list of horizontal intervals to the global interval list and merge the intervals in the global interval list.
5. The answer is True if and only if the global interval list contains only one interval that covers the entire horizontal line segment of the yellow rectangle.
