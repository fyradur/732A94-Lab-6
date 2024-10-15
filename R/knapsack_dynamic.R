#' Solving the knapsack problem in the case of discrete weights
#'
#' @param x A dataframe.
#' @param W A non-negative number.
#'
#' @return A list detailing the maximum value and the elements to get it.
#' @export
#'
#' @importFrom memoise memoise


knapsack_dynamic <- function(x, W){
  if (!all(c('w', 'v') %in% names(x))) {
    stop("Error: Data frame 'x' must contain columns named 'w' (weights) and 'v' (values).")
  }

  # Check if weights and values are numeric and non-negative
  if (!is.numeric(x$w) || any(x$w < 0)) {
    stop("Error: Column 'w' must contain non-negative numeric values.")
  }

  if (!is.numeric(x$v) || any(x$v < 0)) {
    stop("Error: Column 'v' must contain non-negative numeric values.")
  }

  # Check if W is a single positive numeric value
  if (!is.numeric(W) || length(W) != 1 || W <= 0) {
    stop("Error: 'W' must be a single positive numeric value representing the knapsack capacity.")
  }

  # Check for missing values in x and W
  if (any(is.na(x$w)) || any(is.na(x$v))) {
    stop("Error: Columns 'w' and 'v' must not contain missing values (NA).")
  }

  if (is.na(W)) {
    stop("Error: 'W' must not be missing (NA).")
  }


  m <- function(i,w){
    if (i == 0) {0}
    else if (x$w[i] > w) {m(i-1, w)}
    else if (x$w[i] <= w) {max(m(i-1, w), m(i-1, w-x$w[i]) + x$v[i])}
  }

  m <- memoise::memoise(m)


  knapsack <- function(i,j){
      if (i == 0){c()}
      else if (m(i,j) > m(i-1, j)) {c(i,knapsack(i-1, j-x$w[i]))}
      else {knapsack(i-1, j)}
  }

  knapsack <- memoise::memoise(knapsack)

  return(list(value = m(nrow(x), W), elements = knapsack(nrow(x), W)))
}
