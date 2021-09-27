step_into_next_curve_segment <- function(start_x, 
                                         start_y,
                                         left_x,
                                         bot_y,
                                         resolution,
                                         x_dim_range,
                                         y_dim_range,
                                         flow_field,
                                         step_length,
                                         what_to_return = "xy_coords",
                                         first_angle = NULL) 
{
  # Get the current x/y position (in relation to grid size)
  x_offset <- start_x - left_x
  y_offset <- start_y - bot_y
  
  # Scale it down, to match grid dimension
  curr_x_index <- round(x_offset / resolution, digits = 0)
  curr_y_index <- round(y_offset / resolution, digits = 0)
  
  # Find the closest point on the grid at each x/y 
  closest_x_index <- which(abs(x_dim_range - curr_x_index) == min(abs(x_dim_range - curr_x_index)))
  closest_y_index <- which(abs(y_dim_range - curr_y_index) == min(abs(y_dim_range - curr_y_index)))
  
  # Grab that angle
  closest_angle <- if(!is.null(first_angle)){first_angle} else {flow_field[[closest_y_index, closest_x_index]]}
  
  # Extend the current line into that angle (scale it up again)
  x_step  <- step_length * cos(closest_angle) * resolution
  y_step  <- step_length * sin(closest_angle) * resolution
  res_x <- start_x + x_step
  res_y <- start_y + y_step
  
  
  if(what_to_return == "angle") {return(closest_angle)}
  
  return(list(x = res_x, y = res_y))
}
