// Leetcode 994. Rotting Oranges
//
// Core Concept: Multi-Source BFS & Iterative State (Wave of Frontiers)
// The queue must strictly represent the *Guaranteed Next Wave* of state
// transformations, not a "waiting room" for pending inspections.
//
// What I Did Wrong (Anti-Patterns):
// 1. DFS Mindset in a BFS Problem (Hidden Side-Effects)
//    I used a lambda helper to handle queue pushing and state mutation. In BFS,
//    the explicit queue is the engine. Hiding mutations inside a helper breaks
//    the "Visual Locality" of the data-flow.
//
// 2. Delayed Mutation (The Double-Push Hazard)
//    I pushed coordinates first and mutated them (grid = 2, --fresh_count)
//    AFTER popping. This caused adjacent rotten oranges to push the same fresh
//    orange multiple times because its state hadn't changed yet. It also caused
//    initial rotten oranges to incorrectly decrement the fresh count.
//
// 3. Phantom Waves
//    I blindly pushed all 4 directional coordinates (including out-of-bounds).
//    When the last orange rotted, it pushed 4 invalid coordinates, inflating
//    the queue size and causing `minutes_elapsed` to incorrectly increment.
//
// The Golden Rule: Atomic "Verify, Mutate, Push"
// To maintain a clean data-flow, state changes must be atomic with the queue
// insertion. Do this BEFORE pushing:
//   1. Verify validity (Is it in-bounds? Is it '1'?).
//   2. Mutate IMMEDIATELY (grid[r][c] = 2; --fresh_count;).
//   3. Push the confirmed node to the frontier.
#include <queue>
#include <vector>

class Solution {
 public:
  int orangesRotting(std::vector<std::vector<int>>& grid) {
    int minutes_elapsed = 0;
    int fresh_orange_count = 0;

    const int num_rows = static_cast<int>(grid.size());
    const int num_cols = static_cast<int>(grid[0].size());

    std::queue<std::pair<int, int>> rotten_queue;

    constexpr int kDirs[] = {1, 0, -1, 0, 1};

    auto IsTargetable = [&](const int row, const int col) -> bool {
      if (row < 0 || row >= num_rows || col < 0 || col >= num_cols) {
        return false;
      }
      return grid[row][col] == 1;
    };

    for (int row = 0; row < num_rows; ++row) {
      for (int col = 0; col < num_cols; ++col) {
        if (grid[row][col] == 1) {
          ++fresh_orange_count;
        } else if (grid[row][col] == 2) {
          rotten_queue.push({row, col});
        }
      }
    }

    while (!rotten_queue.empty() && fresh_orange_count > 0) {
      // ==========================================
      // [MACRO SCOPE] Check Wave Size
      // ==========================================
      const int current_wave_size = static_cast<int>(rotten_queue.size());

      for (int i = 0; i < current_wave_size; ++i) {
        const auto [curr_row, curr_col] = rotten_queue.front();
        rotten_queue.pop();

        for (int dir = 0; dir < 4; ++dir) {
          const int next_row = curr_row + kDirs[dir];
          const int next_col = curr_col + kDirs[dir + 1];

          // ==========================================
          // [MICRO SCOPE] Find New Target
          // ==========================================
          if (IsTargetable(next_row, next_col)) {
            grid[next_row][next_col] = 2;
            --fresh_orange_count;
            rotten_queue.push({next_row, next_col});
          }
        }
      }
      // ==========================================
      // [MACRO SCOPE] Global Elapsed + 1;
      // ==========================================
      ++minutes_elapsed;
    }

    return (fresh_orange_count == 0) ? minutes_elapsed : -1;
  }
};
