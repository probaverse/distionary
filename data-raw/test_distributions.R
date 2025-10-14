# A list of distributions to be used for systematic testing
#
# This object defines a comprehensive set of distributions used for systematic
# testing of distribution representations. The testing strategy involves:
#
# 1. **Property Recovery Testing**: Each distribution is tested to ensure
#    that all defined properties (CDF, PDF/PMF, quantile, mean, etc.)
#    can be accurately recovered from other representations. The volume of
#    distributions tested acts as further verification that the formulas /
#    algorithms for recovering these properties are correctly implemented.
#
# 2. **Representation Validation**: Each representation is tested to
#    ensure it satisfies its mathematical properties (e.g., CDFs are
#    non-decreasing, densities integrate to 1, PMFs sum to 1, etc.).
#
# Each distribution entry contains:
# - `distribution`: The constructor function name
# - `invalid`: Parameter sets that should fail validation
# - `valid`: Parameter sets for systematic testing

test_distributions <- list(
  bern = list(
    distribution = "dst_bern",
    invalid = list(
      list(prob = -1),
      list(prob = 2)
    ),
    valid = list(
      list(prob = 0.5),
      list(prob = 0.7)
    )
  ),
  beta = list(
    distribution = "dst_beta",
    invalid = list(
      list(shape1 = -1, shape2 = 1),
      list(shape1 = 1, shape2 = -1)
    ),
    valid = list(
      list(shape1 = 2, shape2 = 0.5),
      list(shape1 = 0.5, shape2 = 0.5)
    )
  ),
  binom = list(
    distribution = "dst_binom",
    invalid = list(
      list(size = -1, prob = 0.5),
      list(size = 4, prob = -1),
      list(size = 4, prob = 2)
    ),
    valid = list(
      list(size = 3, prob = 0.3)
    )
  ),
  cauchy = list(
    distribution = "dst_cauchy",
    invalid = list(
      list(location = 0, scale = -1)
    ),
    valid = list(
      list(location = 0, scale = 1),
      list(location = 2, scale = 0.5)
    )
  ),
  chisq = list(
    distribution = "dst_chisq",
    invalid = list(
      list(df = -1)
    ),
    valid = list(
      list(df = 1),
      list(df = 10)
    )
  ),
  degenerate = list(
    distribution = "dst_degenerate",
    invalid = list(
      list(location = "a")
    ),
    valid = list(
      list(location = 1),
      list(location = -4)
    )
  ),
  empirical = list(
    distribution = "dst_empirical",
    invalid = list(
      list(y = 1:3, weights = -1:1),
      list(y = 1:3, weights = 1:2)
    ),
    valid = list(
      list(y = 1:3, weights = 1:3),
      list(y = -1:2),
      list(y = 1:10000)
    )
  ),
  exp = list(
    distribution = "dst_exp",
    invalid = list(
      list(rate = -1)
    ),
    valid = list(
      list(rate = 1),
      list(rate = 2.5)
    )
  ),
  f = list(
    distribution = "dst_f",
    invalid = list(
      list(df1 = -1, df2 = 2),
      list(df1 = 3, df2 = -2)
    ),
    valid = list(
      list(df1 = 2, df2 = 1),
      list(df1 = 3, df2 = 3),
      list(df1 = 1.5, df2 = 5),
      list(df1 = 3.5, df2 = 7),
      list(df1 = 2.2, df2 = 9)
    )
  ),
  finite = list(
    distribution = "dst_finite",
    invalid = list(
      list(outcomes = 1:3, probs = rep(0.25, 4)),
      list(outcomes = letters[1:4], probs = c(0.5, 0.3, 0.1)),
      list(outcomes = 1:3, probs = letters[1:3]),
      list(outcomes = 1:3, probs = rep(0.1, 3)),
      list(outcomes = 1:3, probs = c(-0.2, 0.8, 0.4)),
      list(outcomes = 1:3, probs = c(1.2, 0.2, 0.2))
    ),
    valid = list(
      list(outcomes = -1:2, probs = rep(0.25, 4)),
      list(outcomes = 8, probs = 1),
      list(outcomes = c(0.34, 0.55, 0.12), probs = c(0.5, 0.2, 0.3)),
      list(outcomes = 1:5, probs = 0:4 / 10)
    )
  ),
  gamma = list(
    distribution = "dst_gamma",
    invalid = list(
      list(shape = -1, rate = 1),
      list(shape = 1, rate = -1)
    ),
    valid = list(
      list(shape = 2, rate = 3),
      list(shape = 4, rate = 1.5)
    )
  ),
  geom = list(
    distribution = "dst_geom",
    invalid = list(
      list(prob = -1),
      list(prob = 2)
    ),
    valid = list(
      list(prob = 0.3),
      list(prob = 0.5),
      list(prob = 0.8)
    )
  ),
  gev = list(
    distribution = "dst_gev",
    invalid = list(
      list(location = 0, scale = -1, shape = 1)
    ),
    valid = list(
      list(location = 0, scale = 1.5, shape = 1.2),
      list(location = 0, scale = 1.5, shape = 0),
      list(location = 0, scale = 1.5, shape = -1.2)
    )
  ),
  gpd = list(
    distribution = "dst_gpd",
    invalid = list(
      list(scale = -1, shape = 1)
    ),
    valid = list(
      list(scale = 1.5, shape = 1.2),
      list(scale = 1.5, shape = 0),
      list(scale = 1.5, shape = -1.2)
    )
  ),
  hyper = list(
    distribution = "dst_hyper",
    invalid = list(
      list(m = -2, n = 4, k = 5),
      list(m = 2, n = -4, k = 5),
      list(m = 2, n = 4, k = -5),
      list(m = 2, n = 4, k = 7)
    ),
    valid = list(
      list(m = 8, n = 4, k = 5),
      list(m = 3, n = 4, k = 5),
      list(m = 8, n = 5, k = 3),
      list(m = 2, n = 5, k = 3)
    )
  ),
  lnorm = list(
    distribution = "dst_lnorm",
    invalid = list(
      list(sdlog = -1.2)
    ),
    valid = list(
      list(meanlog = -1, sdlog = 1.2),
      list(meanlog = 0, sdlog = 1.1)
    )
  ),
  lp3 = list(
    distribution = "dst_lp3",
    invalid = list(
      list(meanlog = 0, sdlog = -1, skew = 1)
    ),
    valid = list(
      list(meanlog = 0, sdlog = 1.1, skew = 0.7),
      list(meanlog = -1, sdlog = 0.7, skew = -0.7)
    )
  ),
  nbinom = list(
    distribution = "dst_nbinom",
    invalid = list(
      list(size = -3, prob = 0.4),
      list(size = 3, prob = -1),
      list(size = 3, prob = 2)
    ),
    valid = list(
      list(size = 3, prob = 0.4),
      list(size = 5, prob = 0.8)
    )
  ),
  norm = list(
    distribution = "dst_norm",
    invalid = list(
      list(mean = 0, sd = -1)
    ),
    valid = list(
      list(mean = 1.1, sd = 2.2),
      list(mean = -1.5, sd = 3.7)
    )
  ),
  pearson3 = list(
    distribution = "dst_pearson3",
    invalid = list(
      list(location = 0, scale = -1, shape = 1),
      list(location = 0, scale = 1, shape = -1)
    ),
    valid = list(
      list(location = 1.1, scale = 2.2, shape = 3.3),
      list(location = 0, scale = 1, shape = 1)
    )
  ),
  pois = list(
    distribution = "dst_pois",
    invalid = list(
      list(lambda = -1)
    ),
    valid = list(
      list(lambda = 1),
      list(lambda = 2.2)
    )
  ),
  t = list(
    distribution = "dst_t",
    invalid = list(
      list(df = -2),
      list(df = -1)
    ),
    valid = list(
      list(df = 1),
      list(df = 2),
      list(df = 3),
      list(df = 4),
      list(df = 5)
    )
  ),
  unif = list(
    distribution = "dst_unif",
    invalid = list(
      list(min = 9, max = 0)
    ),
    valid = list(
      list(min = 0, max = 1),
      list(min = -2, max = 1)
    )
  ),
  weibull = list(
    distribution = "dst_weibull",
    invalid = list(
      list(shape = -1, scale = 1),
      list(shape = 1, scale = -1)
    ),
    valid = list(
      list(shape = 0.8, scale = 1.5),
      list(shape = 3.3, scale = 2.4)
    )
  )
)

usethis::use_data(test_distributions, overwrite = TRUE, internal = TRUE)
