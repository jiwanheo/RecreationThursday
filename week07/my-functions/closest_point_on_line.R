closest_point_on_line <- function(start_x, start_y, end_x, end_y, point_x, point_y) {
  
  point_to_start_x <- point_x - start_x
  point_to_start_y <- point_y - start_y
  point_to_start_vec <- c(point_to_start_x, point_to_start_y)
  
  end_to_start_x <- end_x - start_x
  end_to_start_y <- end_y - start_y
  end_to_start_vec <- c(end_to_start_x, end_to_start_y)
  
  lambda <- (point_to_start_vec %*% end_to_start_vec) / (end_to_start_vec %*% end_to_start_vec)
  lambda <- lambda[[1,1]]

  if(lambda >= 1) {
    list(x = end_x, y = end_y)
  } 
  else if (lambda <= 0) {
    list(x = start_x, y = start_y)
  } 
  else{
    new_point_x <- start_x + lambda*(end_x - start_x)
    new_point_y <- start_y + lambda*(end_y - start_y)
    
    return(list(x = new_point_x, y = new_point_y))
  }
}