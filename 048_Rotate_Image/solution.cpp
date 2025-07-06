// NOTE: This has exsist in my interview on 2025-06-12 and I failed it
//
// The solution sould be knowing the rotation could be separate into two process
//
// Start
// [[ 1, 2, 3 ]
//  [ 4, 5, 6 ]
//  [ 7, 8, 9 ]]
//
// Swap rol and col
// [[ 1, 4, 7 ]
//  [ 2, 5, 8 ]
//  [ 3, 6, 9 ]]
//
// Reverse each row
// [[ 1, 4, 7 ]
//  [ 2, 5, 8 ]
//  [ 3, 6, 9 ]]
//

class Solution {
public:
    void rotate(vector<vector<int>>& matrix) {
        const size_t N = matrix[0].size();

        for (int i = 0; i < N; ++i) {
            for (int j = i + 1; j < N; ++j) { // Beware the index here
                std::swap(matrix[i][j], matrix[j][i]);
            }
        }

        for (int i = 0; i < N; ++i) {
            std::reverse(matrix[i].begin(), matrix[i].end());
        }
    }
};
