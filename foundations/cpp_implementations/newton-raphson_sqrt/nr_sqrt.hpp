// Newton-Raphson sqrt

#include <cmath>
#include <limits>

float my_sqrt(float S, float epsilon = 1e-6f) {
  if (S < 0.0f) return std::numeric_limits<float>::quiet_NaN();

  if (S == 0.0f) return 0.0f;

  float guess = (S > 1.0f) ? S / 2.0f : 1.0f;
  constexpr int kMaxIter = 100;
  for (int i = 0; i < kMaxIter; ++i) {
    float next_guess = 0.5f * (guess + S / guess);
    if (std::fabs(next_guess - guess) <= epsilon * std::fabs(next_guess)) {
      return next_guess;
    }
    guess = next_guess;
  }

  return guess;
}
