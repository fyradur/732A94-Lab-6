#' Greedy Approximation Knapsack Solver
#'
#' This function solves the bounded knapsack problem using a greedy approximation algorithm.
#'
#' @param x A data frame with two columns:
#'   \describe{
#'     \item{w}{A numeric vector representing the weights of the items.}
#'     \item{v}{A numeric vector representing the values of the items.}
#'   }
#' @param W A numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{value}{The maximum total value of the items that can be carried.}
#'   \item{elements}{A vector of indices corresponding to the items selected for the optimal solution.}
#' }
#'
#' @export
greedy_knapsack <- function(x, W) {
  # Input validation
  if (!is.data.frame(x)) {
    stop("Input x must be a data frame.")
  }
  if (W < 0) {
    stop("Input W must be non-negative.")
  }
  if (!all(c("w", "v") %in% names(x))) {
    stop("Data frame x must contain columns named w and v.")
  }
  if (any(x$w < 0) || any(x$v < 0)) {
    stop("Columns w and v must contain non-negative numeric values.")
  }
  
  # Remove NA values from the data frame for w and v
  x <- na.omit(x)
  
  # Calculate value-to-weight ratio and sort items by this ratio in descending order
  x$ratio <- x$v / x$w
  items <- x[order(-x$ratio), ]
  
  # Initialize total value, weight, and selected indices
  total_value <- 0
  total_weight <- 0
  selected_indices <- integer()
  
  # Loop through the sorted items and add to the knapsack
  for (i in seq_len(nrow(items))) {
    if (total_weight + items$w[i] <= W) {
      total_weight <- total_weight + items$w[i]
      total_value <- total_value + items$v[i]
      selected_indices <- c(selected_indices, which(x$w == items$w[i] & x$v == items$v[i]))
    } else {
      break
    }
  }
  
  # Return the result as a list
  list(value = round(total_value), elements = selected_indices)
}
