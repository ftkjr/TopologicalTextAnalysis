find_starting_point <- function(adjacency_matrix, minimum_connections, untraversed_points) {
  
  for (starting_point in untraversed_points) {
    
    if (sum(adjacency_matrix[starting_point, ]) >= minimum_connections) {
      untraversed_points <- untraversed_points[untraversed_points != starting_point]
      
      starting_info <- list(starting_point, untraversed_points)
      
      return(starting_info)
    } 
    
    if (starting_point == untraversed_points[length(untraversed_points)]) {
      return(NULL)
    }
  }
  
  
}

