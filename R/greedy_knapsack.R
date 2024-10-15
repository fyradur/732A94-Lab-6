greedy_knapsack <- function(x, W){
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
  while (total_weight + w[i+1] <= W) {
    total_weight <- total_weight + w[i+1]
    total_value <- total_value + v[i+1]
    i <- i+1
  }

  if (total_value > v[i+1]) {
    return(list(value = total_value, elements = x_sorted_filtered$element[1:i]))
    }
  else {
    return(list(value = v[i+1], elements = x_sorted_filtered$element[i+1]))
  }
}
