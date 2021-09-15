step_into_next_curve_segment <- function(start_x, 
                                         start_y,
                                         left_x,
                                         bot_y,
                                         resolution,
                                         x_dim_range,
                                         y_dim_range,
                                         flow_field,
                                         step_length) 
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
  closest_angle <- flow_field[[closest_y_index, closest_x_index]]
  
  # Extend the current line into that angle (scale it up again)
  x_step  <- step_length * cos(closest_angle) * resolution
  y_step  <- step_length * sin(closest_angle) * resolution
  res_x <- start_x + x_step
  res_y <- start_y + y_step
  
  list(x = res_x, y = res_y)
  
}
