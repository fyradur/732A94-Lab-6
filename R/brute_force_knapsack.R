brute_force_knapsack <-
function(x, W){
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
