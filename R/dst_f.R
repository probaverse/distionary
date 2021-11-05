#' F Distribution
#'
#' Makes a distribution belonging to the family of
#' F distributions.
#'
#' @param df1 > 0 degree of freedom
#' @param df2 > 0 degree of freedom
#'
#' @example dst_f(2, 3)
#'
#' @export
dst_f <- function(df1, df2){
  if(df1 <= 0){
    stop('df1 must be positive')
  }
  if(df2 <= 0){
    stop('df2 must be positive')
  }
  dst_parametric("f",
                 df1 = df1,
                 df2 = df2,
                 .variable = "continuous")
}