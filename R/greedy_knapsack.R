greedy_knapsack <- function(x, W){
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

  x$element <- 1:nrow(x)
  x_filtered <- x[which(x$w <= W),]
  x_filtered$value_per_weight <- x_filtered$v / x_filtered$w
  x_sorted_filtered <- x_filtered[order(x_filtered$value_per_weight, decreasing = TRUE),]

  w <- x_sorted_filtered$w
  v <- x_sorted_filtered$v
  element <- x_sorted_filtered$element

  i <- 1
  total_weight <- w[i]
  total_value <- v[i]
  while ((total_weight + w[i+1] <= W) & (i < nrow(x_sorted_filtered))) {
    total_weight <- total_weight + w[i+1]
    total_value <- total_value + v[i+1]
    i <- i+1
  }

  if (i == nrow(x_sorted_filtered)){
    return(list(value = total_value, elements = x_sorted_filtered$element[1:i]))
  }
  else if (total_value > v[i+1]) {
    return(list(value = total_value, elements = x_sorted_filtered$element[1:i]))
    }
  else {
    return(list(value = v[i+1], elements = x_sorted_filtered$element[i+1]))
  }
}
