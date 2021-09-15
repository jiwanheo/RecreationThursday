line_to_circle_collision <- function(line_x, line_y, line_xend, line_yend,
                                     circle_x, circle_y, circle_r) {
  
  # Find the closest point on the line, to the circle center
  # compare the distance from that point to circle center, to circle radius
  
  closest <- closest_point_on_line(
    start_x = line_x,
    start_y = line_y,
    end_x   = line_xend,
    end_y   = line_yend,
    point_x = circle_x,
    point_y = circle_y
  )
  
  distance_to_circle_center <- sqrt( (closest$x-circle_x)^2 + (closest$y-circle_y)^2 ) 
  
  if(distance_to_circle_center <= circle_r) { # I could easily add padding
    return(TRUE)
  } else {
    return(FALSE)
  } 
}