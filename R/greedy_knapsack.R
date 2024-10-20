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
  
  # Remove NA values from the data frame for w and v
  x <- x[!is.na(x$w) & !is.na(x$v), ]
  
  # Calculate value-to-weight ratio and store it with indices
  ratios <- x$v / x$w
  items <- data.frame(index = 1:nrow(x), weight = x$w, value = x$v, ratio = ratios)
  
  # Sort items by value-to-weight ratio in descending order
  items <- items[order(-items$ratio), ]
  
  total_value <- 0
  total_weight <- 0
  selected_indices <- integer(0)
  
  i <- 1  # Initialize the index for the repeat loop
  repeat {
    # Break the loop if we exceed the number of items
    if (i > nrow(items)) {
      break
    }
    
    # Check if adding the current item exceeds the weight limit
    if (total_weight + items$weight[i] <= W) {
      total_weight <- total_weight + items$weight[i]
      total_value <- total_value + items$value[i]
      selected_indices <- c(selected_indices, items$index[i])  # Store the original index
    } else {
      # If it exceeds the weight limit, break without adding the last item
      break
    }
    
    i <- i + 1  # Increment the index
  }
  
  # Return the result as a list
  knapsack_rezzie <- list(value = round(total_value), elements = selected_indices)
  
  return(knapsack_rezzie)
  
    }
