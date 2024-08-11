test_that("Distribution setting", {
  # 1. Explicitly specify.
  my_dst <- distribution3(.density = \(x) x^2)
  eval_density3(my_dst, at = 1:10)
  # 2. Through a package
  my_dst <- distribution3(.density = extRemes::pevd)
  eval_density3(my_dst, at = 1:10)
  # 3. Homemade functions. This is like prompting the user to make their own
  # R package by curating their own environment.
  my_dst <- distribution3(
    a = function(x, y) x + y,
    dfoo = function(x) a(x, 2 * x) / b(3 * x, 1),
    pfoo = function(x) a(x, x),
    .density = \(x) pfoo(x) * dfoo(x),
    .vtype = "continuous"
  )
  my_dst$b <- function(x, y) x - y
  eval_density3(my_dst, at = 1:10)
  # 4. As a developer of distionary, I want to be able to use this infrastructure,
  # too.
  dst_norm3 <- distribution3(
    .density = function(x) stats::dnorm(x, mean = mu, sd = sd),
    .parameters = c("mu", "sd")
  )
  set_params(dst_norm3, mu = 3, sd = 1) # Garbage collector kicks in since new environment has no binding.

})
