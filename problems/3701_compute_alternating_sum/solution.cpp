// Leetcode 3701. Compute Alternating Sum
// @tag: weekly-contest470

#include <vector>

class Solution {
public:
    int alternatingSum(std::vector<int>& nums) {
        int sum = 0;
        for (int idx = 0; idx < nums.size(); ++idx) {
            if (idx % 2 == 0) {
                sum += nums[idx];
            } else {
                sum -= nums[idx];
            }
        }
        return sum;
    }
};