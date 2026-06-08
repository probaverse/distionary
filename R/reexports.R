# Re-exports from the discretes package ---------------------------------------
#
# distionary re-exports the {discretes} series constructors most useful for
# specifying the atomic (discrete) part of a support, so users can write e.g.
# `discrete(natural0())` without attaching {discretes}. The remaining {discretes}
# tools (series manipulation, membership testing, etc.) are deliberately not
# re-exported here; they belong to the manipulation layer (distplyr) and remain
# available via `discretes::`.

#' @importFrom discretes natural0
#' @export
discretes::natural0

#' @importFrom discretes natural1
#' @export
discretes::natural1

#' @importFrom discretes integers
#' @export
discretes::integers

#' @importFrom discretes arithmetic
#' @export
discretes::arithmetic

#' @importFrom discretes as_discretes
#' @export
discretes::as_discretes
