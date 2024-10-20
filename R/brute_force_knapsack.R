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
#' 
#' @return A list with two elements:
#' \describe{
#'   \item{value}{The maximum total value of the items that can be carried.}
#'   \item{elements}{A vector of indices corresponding to the items in the optimal solution.}
#' }
#'
#' @export
brute_force_knapsack <- function(x, W) {
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
  
  # Helper function to recursively generate all combinations of item indices
  gen_combies <- function(l, prefix = c()) {
    if (length(l) == 0) {
      return(list(prefix))
    } else {
      w_first <- gen_combies(l[-1], c(prefix, l[1]))  # Include first item
      wout_first <- gen_combies(l[-1], prefix)  # Exclude first item
      return(c(w_first, wout_first))
    }
  }
  
  # Generate all possible combinations of item indices
  item_ind <- 1:length(x$w)
  all_combies <- gen_combies(item_ind)
  
  # Remove the empty combination
  all_combies <- all_combies[sapply(all_combies, length) > 0]
  
  # Calculate total weight and value for each combination of items
  calc_w_val <- function(combie) {
    total_w <- sum(x$w[combie])  # Sum of weights for this combination
    total_v <- sum(x$v[combie])  # Sum of values for this combination
    obj <- paste(combie, collapse = ",")  # Store combination as a string
    return(list(object = obj, weight = total_w, value = total_v))
  }
  
  # Apply the weight and value calculation to all combinations
  combies_data <- lapply(all_combies, calc_w_val)
  
  # Convert the list of combinations into a data frame
  all_combies_df <- do.call(rbind, lapply(combies_data, as.data.frame))
  
  # Filter valid combinations whose total weight is less than or equal to W
  valid_combies <- all_combies_df[all_combies_df$weight <= W, ]
  
  # Find the combination with the maximum value
  max_val <- max(valid_combies$value)
  best_combie <- valid_combies[valid_combies$value == max_val, ]
  
  # Extract the indices of the selected items
  sel_ind <- as.numeric(unlist(strsplit(as.character(best_combie$object[1]), ",")))
  
  # Return the result as a list
  knapsack_rezzie <- list(value = round(max_val), elements = sel_ind)
  
  return(knapsack_rezzie)
}
