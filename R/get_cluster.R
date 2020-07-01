get_cluster <- function(adjacency_matrix, minimum_connections, untraversed_points) {
  
  cluster <- c()
  
  starting_point <- find_starting_point(adjacency_matrix, minimum_connections, untraversed_points)
  
  
  if (is.null(starting_point[[1]])) {
    return(NULL)
  } else {
    cluster <- append(cluster, as.integer(starting_point[[1]]))
    untraversed_points <- starting_point[[2]]
  }
  
  for (point in cluster) {
    for (other_point in untraversed_points) {
      untraversed_points <- untraversed_points[untraversed_points != other_point]
      
      if (sum(adjacency_matrix[other_point]) >= minimum_connections) {
        cluster <- append(cluster, other_point)
      }
      
    }
  }
  
  return(cluster)
}
