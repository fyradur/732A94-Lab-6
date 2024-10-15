#' Solving the knapsack problem in the case of discrete weights
#'
#' @param x A dataframe.
#' @param W A non-negative number.
#'
#' @return A list detailing the maximum value and the elements to get it.
#' @export
#'
knapsack_dynamic <- function(x, W){
  m <- function(i,w){
    if (i == 0) {0}
    else if (x$w[i] > w) {m(i-1, w)}
    else if (x$w[i] <= w) {max(m(i-1, w), m(i-1, w-x$w[i] + x$v[i]))}
  }

  return(m(nrow(x), W))
}
