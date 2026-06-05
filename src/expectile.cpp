#include <Rcpp.h>

#include <algorithm>
#include <cmath>

#include "numerics.h"

using namespace Rcpp;

// Both algorithms below are built on the expectile identification equation
// (Daouia, Stupfler & Usseglio-Carleve, 2023; Newey & Powell, 1987). Writing
// phi(x) = E[(X - x)^+] = \int_x^\infty S(t) dt for the survival function S,
// and m for the mean, the tau-expectile xi is the unique root in x of
//
//   g_tau(x) = (2 tau - 1) / (1 - tau) * phi(x) + m - x.
//
// g_tau is continuous and strictly decreasing for every tau, which is what
// lets a single safeguarded solver serve both directions.

// Evaluate a one-argument R function at a scalar and return a scalar double.
static inline double call1(const Function &f, double x) {
  return as<double>(f(x));
}

// Forward direction: expectiles from the survival function and the mean.
//
// `phi` evaluates phi(x) (supplied from R, where the survival function is
// integrated), `survival` evaluates S(x), and the root of g_tau is found with
// safeguarded Newton. The derivative g_tau'(x) = -k S(x) - 1 follows from
// phi'(x) = -S(x).
//
// [[Rcpp::export]]
NumericVector cpp_expectile_forward(NumericVector tau, Function phi,
                                    Function survival, double mean, double tol,
                                    int max_iter) {
  int n = tau.size();
  NumericVector out(n);
  distionary::root_control ctrl(tol, max_iter);
  double scale = std::max(1.0, std::fabs(mean));
  for (int i = 0; i < n; ++i) {
    double t = tau[i];
    if (NumericVector::is_na(t)) {
      out[i] = NA_REAL;
    } else if (t <= 0.0) {
      out[i] = (t == 0.0) ? R_NegInf : R_NaN;
    } else if (t >= 1.0) {
      out[i] = (t == 1.0) ? R_PosInf : R_NaN;
    } else if (t == 0.5) {
      out[i] = mean;  // The 1/2-expectile is the mean.
    } else {
      double k = (2.0 * t - 1.0) / (1.0 - t);
      auto g = [&](double x) { return k * call1(phi, x) + mean - x; };
      auto dg = [&](double x) { return -k * call1(survival, x) - 1.0; };
      out[i] = distionary::solve_decreasing(g, dg, mean, scale, ctrl);
    }
  }
  return out;
}

// Reverse direction: the CDF from the expectile function and the mean.
//
// The expectile function xi(tau) is increasing, so for a point x we first find
// tau(x) by solving xi(tau) = x. Rearranging the identification equation gives
// phi(x) = (1 - tau)(x - m) / (2 tau - 1), an explicit, smooth function of x
// (the apparent singularity at tau = 1/2 is removable). The survival function
// is S(x) = -phi'(x), obtained here by a central difference; the CDF is
// 1 - S(x). The inner tau-solve is held to a tight tolerance so that the
// difference quotient is not swamped by solver noise.
//
// [[Rcpp::export]]
NumericVector cpp_expectile_reverse_cdf(NumericVector at, Function expectile,
                                        double mean, double tol, int max_iter) {
  int n = at.size();
  NumericVector out(n);
  const double tau_lo = 1e-12;
  const double tau_hi = 1.0 - 1e-12;
  double xi_lo = call1(expectile, tau_lo);
  double xi_hi = call1(expectile, tau_hi);
  distionary::root_control inner(tol, max_iter);

  // tau(x): root of xi(tau) - x, which is decreasing in tau.
  auto tau_of = [&](double x) -> double {
    if (x <= xi_lo) return tau_lo;
    if (x >= xi_hi) return tau_hi;
    auto g = [&](double t) { return x - call1(expectile, t); };
    return distionary::bisect_decreasing(g, tau_lo, tau_hi, inner);
  };
  // phi(x) via the rearranged identification equation.
  auto phi_of = [&](double x) -> double {
    double t = tau_of(x);
    return (1.0 - t) * (x - mean) / (2.0 * t - 1.0);
  };

  for (int i = 0; i < n; ++i) {
    double x = at[i];
    if (NumericVector::is_na(x)) {
      out[i] = NA_REAL;
    } else if (x <= xi_lo) {
      out[i] = 0.0;
    } else if (x >= xi_hi) {
      out[i] = 1.0;
    } else {
      double h = 1e-4 * std::max(1.0, std::fabs(x));
      double survival = -(phi_of(x + h) - phi_of(x - h)) / (2.0 * h);
      survival = std::min(1.0, std::max(0.0, survival));
      out[i] = 1.0 - survival;
    }
  }
  return out;
}
