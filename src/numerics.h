#ifndef DISTIONARY_NUMERICS_H
#define DISTIONARY_NUMERICS_H

// Reusable one-dimensional numerical routines for distionary.
//
// These are deliberately decoupled from any particular distributional
// representation so that other algorithms in the package (for example, the
// quantile inversion that is planned to move from R to C++) can share them.
// Every routine works on a plain `std::function<double(double)>`, so the
// caller decides what is being solved.

#include <cmath>
#include <functional>
#include <limits>

namespace distionary {

// Stopping rules shared by the solvers below: `tol` bounds both the residual
// and the bracket width, and `max_iter` caps the number of iterations.
struct root_control {
  double tol;
  int max_iter;
  root_control(double tol_ = 1e-9, int max_iter_ = 200)
      : tol(tol_), max_iter(max_iter_) {}
};

// Grow a bracket [lo, hi] that straddles the unique root of a continuous,
// strictly decreasing function `g` (so g(lo) >= 0 >= g(hi)). The search starts
// at `seed`, which should be near the root, and steps outward by `scale`,
// doubling the step each time. Returns false if no sign change is found within
// `max_expand` doublings.
inline bool bracket_decreasing(const std::function<double(double)> &g,
                               double seed, double scale, double &lo,
                               double &hi, int max_expand = 60) {
  double step = scale > 0.0 ? scale : 1.0;
  double g_seed = g(seed);
  if (g_seed == 0.0) {
    lo = hi = seed;
    return true;
  }
  if (g_seed > 0.0) {
    lo = seed;
    hi = seed + step;
    for (int i = 0; i < max_expand; ++i) {
      if (g(hi) <= 0.0) return true;
      lo = hi;
      step *= 2.0;
      hi += step;
    }
  } else {
    hi = seed;
    lo = seed - step;
    for (int i = 0; i < max_expand; ++i) {
      if (g(lo) >= 0.0) return true;
      hi = lo;
      step *= 2.0;
      lo -= step;
    }
  }
  return false;
}

// Bisection for the root of a continuous, strictly decreasing function `g` on
// a bracket [lo, hi] with g(lo) >= 0 >= g(hi). Robust but linearly convergent;
// used when no derivative is available.
inline double bisect_decreasing(const std::function<double(double)> &g,
                                double lo, double hi,
                                const root_control &ctrl) {
  if (lo == hi) return lo;
  double mid = 0.5 * (lo + hi);
  for (int i = 0; i < ctrl.max_iter; ++i) {
    mid = 0.5 * (lo + hi);
    double gm = g(mid);
    if (gm > 0.0) {
      lo = mid;
    } else {
      hi = mid;
    }
    if (std::fabs(gm) <= ctrl.tol || (hi - lo) <= ctrl.tol) return mid;
  }
  return mid;
}

// Newton-Raphson for the root of a strictly decreasing function `g` with
// derivative `dg`, confined to the bracket [lo, hi] (g(lo) >= 0 >= g(hi)).
// Each step that would leave the bracket, or that does not move, falls back to
// a bisection step, so convergence is guaranteed while retaining the quadratic
// rate where Newton behaves well.
inline double newton_decreasing(const std::function<double(double)> &g,
                                const std::function<double(double)> &dg,
                                double lo, double hi,
                                const root_control &ctrl) {
  if (lo == hi) return lo;
  double x = 0.5 * (lo + hi);
  for (int i = 0; i < ctrl.max_iter; ++i) {
    double gx = g(x);
    if (gx > 0.0) {
      lo = x;
    } else {
      hi = x;
    }
    if (std::fabs(gx) <= ctrl.tol || (hi - lo) <= ctrl.tol) return x;
    double slope = dg(x);
    double x_newton = (slope != 0.0) ? x - gx / slope : x;
    x = (x_newton > lo && x_newton < hi) ? x_newton : 0.5 * (lo + hi);
  }
  return x;
}

// Convenience wrapper: bracket a strictly decreasing `g` starting from `seed`,
// then solve with safeguarded Newton. Returns NaN if no bracket is found.
inline double solve_decreasing(const std::function<double(double)> &g,
                               const std::function<double(double)> &dg,
                               double seed, double scale,
                               const root_control &ctrl) {
  double lo, hi;
  if (!bracket_decreasing(g, seed, scale, lo, hi)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  if (lo == hi) return lo;
  return newton_decreasing(g, dg, lo, hi, ctrl);
}

}  // namespace distionary

#endif  // DISTIONARY_NUMERICS_H
