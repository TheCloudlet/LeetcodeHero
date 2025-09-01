// Leetcode 300. Longest Increasing Subsequence
// @tag: dp, binary-search
// @difficulty: medium
//
// Thinking process:
//
// 1. Brute force
// The trivial way is to pick or not pick each number, and use a recursive
// function to find the longest increasing subsequence. The time complexity
// is O(2^n). Which is definitely not acceptable.
//
// 2. Trasnform to graph problem
// In previous practive, we learn monotonic stack technique to solve similar
// problems, such as next greater element. I think we can use first find the
// next greater element O(n), then transfer the problem into a graph problem
// O(n^2). The the problem becomes to find the longest path in a directed
// acyclic graph DAG. We can use topological sort to find the longest path in
// O(V+E).
//
// 3. Traditional DP solution
// We can use a DP array to store the longest increasing subsequence ending
// at each index. The time complexity is O(n^2).
//
// 4. DP with binary search
// TO BE COMPLETED...
