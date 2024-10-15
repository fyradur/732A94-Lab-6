#' Brute force the knapsack problem
#'
#' @param x A dataframe.
#' @param W A non-negative number.
#'
#' @return A list detailing the maximum value and the elements to get it.
#' @export
#'
brute_force_knapsack <-
function(x, W){
  # Check if x has columns 'w' and 'v'
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
    subset_max_elements <- NA
    v_sum_max <- NA
    n <- nrow(x)

    for(i in 1:(2^n - 1)){
        subset_elements <- which(intToBits(i) == 1)
        x_subset <- x[subset_elements,]
        x_subset_sums <- colSums(x_subset)

        v_sum <- x_subset_sums['v']
        w_sum <- x_subset_sums['w']

        is_not_to_heavy <- w_sum <= W
        is_largest_observation <- if(is.na(v_sum_max)){TRUE}
        else {v_sum > v_sum_max}

        if(is_not_to_heavy & is_largest_observation) {
            subset_max_elements <- subset_elements
            v_sum_max <- v_sum
        }

    }

    names(v_sum_max) = NULL
    return(list(value=v_sum_max, elements=subset_max_elements))
}
