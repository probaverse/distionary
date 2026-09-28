# The conditional distribution through the property network, as distplyr's
# `conditional()` reaches it: known variables by name, values alongside.
condition <- function(distribution, given) {
  idx <- match(names(given), variables(distribution))
  eval_property(distribution, "conditional", idx, unname(unlist(given)))
}
