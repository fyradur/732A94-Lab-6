#' Brute Force Knapsack Solver
#'
#' This function solves the knapsack problem using a brute-force approach.
#'
#' @param x A data frame with two columns:
#'   \describe{
#'     \item{w}{A numeric vector representing the weights of the items.}
#'     \item{v}{A numeric vector representing the values of the items.}
#'   }
#' @param W A numeric value representing the maximum weight capacity of the knapsack.
#' @param fast A logical value indicating whether to use the fast C++ implementation. Default is FALSE.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{value}{The maximum total value of the items that can be carried.}
#'   \item{elements}{A vector of indices corresponding to the items in the optimal solution.}
#' }
#'
#' @importFrom stats na.omit
#' @importFrom Rcpp cppFunction
#'
#' @export
brute_force_knapsack <- function(x, W, fast = FALSE) {
  # Input validation
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("Input x must be a data frame with columns 'w' and 'v'.")
  }
  if (W < 0) {
    stop("Input W must be non-negative.")
  }
  if (!is.numeric(x$w) || any(x$w < 0) || !is.numeric(x$v) || any(x$v < 0)) {
    stop("Columns w and v must contain non-negative numeric values.")
  }

  if (fast) {
    # C++ implementation using Rcpp
    Rcpp::cppFunction(
      '
      List knapsack_cpp(NumericVector w, NumericVector v, double W) {
        int n = w.size();
        double max_value = 0;
        IntegerVector best_combination;

        // Iterate through all possible combinations of items
        for (int i = 0; i < (1 << n); i++) {
          double total_weight = 0;
          double total_value = 0;
          IntegerVector combination;

          for (int j = 0; j < n; j++) {
            // Check if the j-th item is included in the current combination
            if (i & (1 << j)) {
              total_weight += w[j];
              total_value += v[j];
              combination.push_back(j + 1); // Store 1-based index
            }
          }

          // If the current combination is valid and has a higher value, update the best combination
          if (total_weight <= W && total_value > max_value) {
            max_value = total_value;
            best_combination = combination;
          }
        }

        return List::create(Named("value") = max_value, Named("elements") = best_combination);
      }
      '
    )

    # Call the C++ function
    result <- knapsack_cpp(as.numeric(x$w), as.numeric(x$v), W)
    return(result)
  } else {
    n <- nrow(x) # Number of items
    max_value <- 0 # Variable to track maximum value
    best_combination <- integer(0) # Best combination of items

    # Iterate through all possible combinations of items
    for (i in 0:(2^n - 1)) {
      combination <- integer(0) # Current combination of items
      total_weight <- 0 # Weight of the current combination
      total_value <- 0 # Value of the current combination

      for (j in 1:n) {
        # Check if the j-th item is included in the current combination
        if (bitwAnd(i, 2^(j - 1)) != 0) {
          total_weight <- total_weight + x$w[j]
          total_value <- total_value + x$v[j]
          combination <- c(combination, j) # Store the index of the included item
        }
      }

      # If the current combination is valid and has a higher value, update the best combination
      if (total_weight <= W && total_value > max_value) {
        max_value <- total_value
        best_combination <- combination
      }
    }

    # Return the result
    knapsack_rezzie <- list(value = max_value, elements = best_combination)
    return(knapsack_rezzie)
  }
}
