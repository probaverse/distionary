// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/math/interpolators/pchip.hpp>

using namespace Rcpp;
using boost::math::interpolators::pchip;

// ============================================================
// ExpectileModel
// ============================================================
// This class represents a distribution via its expectile function.
// It reconstructs distributional quantities through:
//
//   φ(x) = E[(X - x)^+]
//
// Key identity:
//   survival(x) = -φ'(x)
//
// ============================================================

class ExpectileModel {
private:
  pchip<double> xi_of_tau;   // ξ(τ)
  pchip<double> tau_of_x;    // inverse τ(x)
  double mean;

public:

  // ------------------------------------------------------------
  // Constructor
  // ------------------------------------------------------------
  ExpectileModel(NumericVector tau,
                 NumericVector xi,
                 double m)
    : xi_of_tau(tau.begin(), tau.end(), xi.begin()),
      tau_of_x(xi.begin(), xi.end(), tau.begin()),
      mean(m) {}

  // ------------------------------------------------------------
  // τ(x)
  // ------------------------------------------------------------
  double tau(double x) const {
    return tau_of_x(x);
  }

  // ------------------------------------------------------------
  // ξ(τ)
  // ------------------------------------------------------------
  double xi(double t) const {
    return xi_of_tau(t);
  }

  // ------------------------------------------------------------
  // φ(x) = E[(X - x)^+]
  // Derived from expectile identity
  // ------------------------------------------------------------
  double phi(double x) const {
    double t = tau(x);
    double denom = 2.0 * t - 1.0;

    // numerical safeguard near τ = 0.5
    if (std::abs(denom) < 1e-8) {
      return 0.5 * std::max(0.0, mean - x);
    }

    return (1.0 - t) * (mean - x) / denom;
  }

  // ------------------------------------------------------------
  // φ'(x) via local finite differences
  // ------------------------------------------------------------
  double phi_prime(double x) const {
    double h = std::sqrt(std::numeric_limits<double>::epsilon()) *
               std::max(1.0, std::abs(x));

    double left  = phi(x - h);
    double right = phi(x + h);

    return (right - left) / (2.0 * h);
  }

  // ------------------------------------------------------------
  // Survival function: S(x) = P(X > x)
  // ------------------------------------------------------------
  double survival(double x) const {
    double val = -phi_prime(x);

    if (val < 0.0) return 0.0;
    if (val > 1.0) return 1.0;

    return val;
  }

  // ------------------------------------------------------------
  // CDF
  // ------------------------------------------------------------
  double cdf(double x) const {
    return 1.0 - survival(x);
  }

  // ------------------------------------------------------------
  // Discrete mass at x (slope jump)
  // ------------------------------------------------------------
  double discrete_mass(double x) const {
    double eps = 1e-6;

    double d_left  = (phi(x) - phi(x - eps)) / eps;
    double d_right = (phi(x + eps) - phi(x)) / eps;

    return d_left - d_right;
  }
};

// ============================================================
// Rcpp Module
// ============================================================

RCPP_MODULE(expectile_module) {

  class_<ExpectileModel>("ExpectileModel")

    .constructor<NumericVector, NumericVector, double>()

    .method("tau", &ExpectileModel::tau)
    .method("xi", &ExpectileModel::xi)
    .method("phi", &ExpectileModel::phi)
    .method("phi_prime", &ExpectileModel::phi_prime)
    .method("survival", &ExpectileModel::survival)
    .method("cdf", &ExpectileModel::cdf)
    .method("discrete_mass", &ExpectileModel::discrete_mass)
  ;
}