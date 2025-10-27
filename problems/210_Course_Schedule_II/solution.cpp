// Leetcode 210. Course Schedule II
// @tag: topological-sort, bfs, graph
// @difficulty: medium


#include <queue>
#include <vector>

class Solution {
public:
  std::vector<int> findOrder(int numCourses,
                             std::vector<std::vector<int>> &prerequisites) {

    std::vector<std::vector<int>> adjVec(numCourses, std::vector<int>());
    std::vector<int> indeg(numCourses, 0);
    std::vector<int> result;
    result.reserve(numCourses);

    for (const auto &pair : prerequisites) {
      int course = pair.at(0);
      int prereqCourse = pair.at(1);
      adjVec[prereqCourse].push_back(course);
      ++indeg[course];
    }

    std::queue<int> waitToProcess;

    // Find all courses with zero in-degree
    for (int course = 0; course < numCourses; ++course) {
      if (indeg[course] == 0) {
        waitToProcess.push(course);
      }
    }

    // Remove courses with zero in-degree and update neighbors
    while (!waitToProcess.empty()) {
      int course = waitToProcess.front();
      waitToProcess.pop();
      result.push_back(course);
      for (const auto &neighborCourse : adjVec[course]) {
        if (--indeg[neighborCourse] == 0) {
          waitToProcess.push(neighborCourse);
        }
      }
      adjVec[course].clear();
    }

    // There is a cycle
    if (result.size() < numCourses) {
      return {};
    }

    return result;
  }
};