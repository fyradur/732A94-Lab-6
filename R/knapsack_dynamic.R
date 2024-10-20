#' Dynamic Programming Knapsack Solver
#'
#' This function solves the knapsack problem using a dynamic programming approach.
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
#'   \item{elements}{A vector of indices corresponding to the items in the optimal solution.}
#' }
#'
#' @export
knapsack_dynamic <- function(x, W) {
  # Check if input data frame is a data frame
  if (!is.data.frame(x)) {
    stop("Input x must be a data frame.")
  }

  # Check if W is non-negative
  if (W < 0) {
    stop("Input W must be non-negative.")
  }

  # Check if input data frame has the required columns w (weights) and v (values)
  if (!all(c("w", "v") %in% names(x))) {
    stop("Data frame x must contain columns named w and v.")
  }

  # Validate that w contains non-negative numeric values
  if (!is.numeric(x$w) || any(x$w < 0)) {
    stop("Column w must contain non-negative numeric values.")
  }

  # Validate that v contains non-negative numeric values
  if (!is.numeric(x$v) || any(x$v < 0)) {
    stop("Column v must contain non-negative numeric values.")
  }

  n <- nrow(x) # Number of items
  # Create a matrix to store the maximum value at each n, W combination
  m <- matrix(0, n + 1, W + 1)

  # Fill the matrix based on the dynamic programming approach
  for (j in 0:W) {
    m[1, j + 1] <- 0 # Base case: no items lead to a value of 0
  }

  for (i in 1:n) {
    for (j in 1:W) {
      if (x$w[i] > j) {
        m[i + 1, j + 1] <- m[i, j + 1] # Item can't be included
      } else {
        # Max value is either including or excluding the item
        m[i + 1, j + 1] <- max(m[i, j + 1], m[i, j + 1 - x$w[i]] + x$v[i])
      }
    }
  }

  # The maximum value achievable with full capacity W is in m[n, W]
  max_val <- m[n + 1, W + 1]

  # To find the selected items, we backtrack
  sel_ind <- numeric(0) # Initialize vector to store selected indices
  j <- W # Start with the maximum weight capacity

  for (i in n:1) {
    # Check if the item was included in the optimal solution
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      sel_ind <- c(sel_ind, i) # Store the index of the selected item
      j <- j - x$w[i] # Reduce the weight capacity
    }
  }

  # Return the result as a list
  knapsack_rezzie <- list(value = round(max_val), elements = rev(sel_ind)) # Reverse to maintain original order

  return(knapsack_rezzie)
}
