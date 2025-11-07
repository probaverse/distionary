# Package index

## Specify

### Built-in Distribution Families

- [`dst_bern()`](https://distionary.probaverse.com/reference/dst_bern.md)
  : Bernoulli Distribution
- [`dst_beta()`](https://distionary.probaverse.com/reference/dst_beta.md)
  : Beta Distribution
- [`dst_binom()`](https://distionary.probaverse.com/reference/dst_binom.md)
  : Binomial Distribution
- [`dst_cauchy()`](https://distionary.probaverse.com/reference/dst_cauchy.md)
  : Cauchy Distribution
- [`dst_chisq()`](https://distionary.probaverse.com/reference/dst_chisq.md)
  : Chi-Squared Distribution
- [`dst_degenerate()`](https://distionary.probaverse.com/reference/dst_degenerate.md)
  : Degenerate Distribution
- [`dst_empirical()`](https://distionary.probaverse.com/reference/dst_empirical.md)
  : Empirical Distribution
- [`dst_exp()`](https://distionary.probaverse.com/reference/dst_exp.md)
  : Exponential Distribution
- [`dst_f()`](https://distionary.probaverse.com/reference/dst_f.md) : F
  Distribution
- [`dst_finite()`](https://distionary.probaverse.com/reference/dst_finite.md)
  : Finite Distribution
- [`dst_gamma()`](https://distionary.probaverse.com/reference/dst_gamma.md)
  : Gamma Distribution
- [`dst_geom()`](https://distionary.probaverse.com/reference/dst_geom.md)
  : Geometric Distribution
- [`dst_gev()`](https://distionary.probaverse.com/reference/dst_gev.md)
  : Generalised Extreme Value Distribution
- [`dst_gp()`](https://distionary.probaverse.com/reference/dst_gp.md) :
  Generalised Pareto Distribution
- [`dst_hyper()`](https://distionary.probaverse.com/reference/dst_hyper.md)
  : Hypergeometric Distribution
- [`dst_lnorm()`](https://distionary.probaverse.com/reference/dst_lnorm.md)
  : Log Normal Distribution
- [`dst_lp3()`](https://distionary.probaverse.com/reference/dst_lp3.md)
  : Log Pearson Type III distribution
- [`dst_nbinom()`](https://distionary.probaverse.com/reference/dst_nbinom.md)
  : Negative binomial Distribution
- [`dst_norm()`](https://distionary.probaverse.com/reference/dst_norm.md)
  : Normal (Gaussian) Distribution
- [`dst_null()`](https://distionary.probaverse.com/reference/dst_null.md)
  : Null Distribution
- [`dst_pearson3()`](https://distionary.probaverse.com/reference/dst_pearson3.md)
  : Pearson Type III distribution
- [`dst_pois()`](https://distionary.probaverse.com/reference/dst_pois.md)
  : Poisson Distribution
- [`dst_t()`](https://distionary.probaverse.com/reference/dst_t.md) :
  Student t Distribution
- [`dst_unif()`](https://distionary.probaverse.com/reference/dst_unif.md)
  : Uniform Distribution
- [`dst_weibull()`](https://distionary.probaverse.com/reference/dst_weibull.md)
  : Weibull Distribution

### General Distribution Creation

- [`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
  [`is_distribution()`](https://distionary.probaverse.com/reference/distribution.md)
  [`is.distribution()`](https://distionary.probaverse.com/reference/distribution.md)
  : Distribution Objects

### Distribution Characteristics

- [`parameters()`](https://distionary.probaverse.com/reference/parameters.md)
  [`` `parameters<-`() ``](https://distionary.probaverse.com/reference/parameters.md)
  : Parameters of a Distribution
- [`plot(`*`<dst>`*`)`](https://distionary.probaverse.com/reference/plot.dst.md)
  : Plot a Distribution
- [`pretty_name()`](https://distionary.probaverse.com/reference/pretty_name.md)
  : Distribution name
- [`vtype()`](https://distionary.probaverse.com/reference/vtype.md) :
  Variable Type of a Distribution

## Evaluate

### Distributional Representations

- [`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md)
  [`enframe_cdf()`](https://distionary.probaverse.com/reference/cdf.md)
  : Cumulative Distribution Function
- [`eval_chf()`](https://distionary.probaverse.com/reference/chf.md)
  [`enframe_chf()`](https://distionary.probaverse.com/reference/chf.md)
  : Cumulative Hazard Function
- [`eval_density()`](https://distionary.probaverse.com/reference/density.md)
  [`enframe_density()`](https://distionary.probaverse.com/reference/density.md)
  : Probability Density Function
- [`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md)
  [`enframe_hazard()`](https://distionary.probaverse.com/reference/hazard.md)
  : Hazard Function
- [`eval_odds()`](https://distionary.probaverse.com/reference/odds.md)
  [`enframe_odds()`](https://distionary.probaverse.com/reference/odds.md)
  : Odds Function
- [`eval_pmf()`](https://distionary.probaverse.com/reference/pmf.md)
  [`enframe_pmf()`](https://distionary.probaverse.com/reference/pmf.md)
  : Probability Mass Function
- [`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md)
  [`enframe_quantile()`](https://distionary.probaverse.com/reference/quantile.md)
  : Distribution Quantiles
- [`eval_return()`](https://distionary.probaverse.com/reference/return.md)
  [`enframe_return()`](https://distionary.probaverse.com/reference/return.md)
  : Return Level Function
- [`eval_survival()`](https://distionary.probaverse.com/reference/survival.md)
  [`enframe_survival()`](https://distionary.probaverse.com/reference/survival.md)
  : Survival Function
- [`eval_property()`](https://distionary.probaverse.com/reference/eval_property.md)
  : Evaluate a distribution
- [`prob_left()`](https://distionary.probaverse.com/reference/flexible_cdf.md)
  [`prob_right()`](https://distionary.probaverse.com/reference/flexible_cdf.md)
  : Find the probability left or right of a number

### Distribution Properties

- [`kurtosis()`](https://distionary.probaverse.com/reference/moments.md)
  [`kurtosis_exc()`](https://distionary.probaverse.com/reference/moments.md)
  [`mean(`*`<dst>`*`)`](https://distionary.probaverse.com/reference/moments.md)
  [`skewness()`](https://distionary.probaverse.com/reference/moments.md)
  [`stdev()`](https://distionary.probaverse.com/reference/moments.md)
  [`variance()`](https://distionary.probaverse.com/reference/moments.md)
  : Moments of a Distribution
- [`median(`*`<dst>`*`)`](https://distionary.probaverse.com/reference/median.dst.md)
  : Median of a Distribution
- [`range(`*`<dst>`*`)`](https://distionary.probaverse.com/reference/range.md)
  : Range of Distribution

### Random Sampling

- [`realise()`](https://distionary.probaverse.com/reference/realise.md)
  [`realize()`](https://distionary.probaverse.com/reference/realise.md)
  : Generate a Sample from a Distribution

### Package Overview

- [`distionary`](https://distionary.probaverse.com/reference/distionary-package.md)
  [`distionary-package`](https://distionary.probaverse.com/reference/distionary-package.md)
  : distionary: Create and Evaluate Probability Distributions

## Built-in Distributional Representations

Distributional representations not found in the `stats` package.

- [`dst_gev()`](https://distionary.probaverse.com/reference/dst_gev.md)
  : Generalised Extreme Value Distribution
- [`pgev()`](https://distionary.probaverse.com/reference/gev_raw.md)
  [`qgev()`](https://distionary.probaverse.com/reference/gev_raw.md)
  [`dgev()`](https://distionary.probaverse.com/reference/gev_raw.md) :
  Representations of the Generalized Extreme Value Distribution
- [`dst_gp()`](https://distionary.probaverse.com/reference/dst_gp.md) :
  Generalised Pareto Distribution
- [`pgp()`](https://distionary.probaverse.com/reference/gp_raw.md)
  [`qgp()`](https://distionary.probaverse.com/reference/gp_raw.md)
  [`dgp()`](https://distionary.probaverse.com/reference/gp_raw.md) :
  Representations of the Generalized Pareto Distribution
