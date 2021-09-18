draw_curve <- function(start_x, 
                       start_y, 
                       row_num, 
                       flow_field, 
                       resolution, 
                       left_x     = 1 * resolution, # actual grid bottom-left coords
                       bot_y      = 1 * resolution,
                       num_steps,   # This controls how smooth/curvy the line is
                       step_length, # This controls how far the line should go each step
                       circles) # circles here, are still in list form
  
{ 
  # Collision check #1
  # Initial check to see if the starting points are in the circles to begin with.
  for (circle in circles){
    point_to_circle_center_distance <- sqrt( (start_x - circle$x0)^2 + (start_y - circle$y0)^2 )
    
    if(point_to_circle_center_distance <= circle$r) {
      return(NULL)
    }
  }
  
  # If not, initialize the curve with the starting points, and keep generating
  x_container <- vector("numeric", num_steps + 1)
  y_container <- vector("numeric", num_steps + 1)
  
  x_container[1] <- start_x
  y_container[1] <- start_y
  
  
  # get the grid dimension to lookup the closest angles, from anywhere on the plot.
  x_dim_range <- 1:ncol(flow_field) 
  y_dim_range <- 1:nrow(flow_field)
  
  # With the rest of num_steps, move through the flow field, with the closest angle we can grab.
  for (i in 1:num_steps) {
    
    next_step <- step_into_next_curve_segment( 
      start_x     = x_container[i], 
      start_y     = y_container[i],
      left_x      = left_x,
      bot_y       = bot_y,
      resolution  = resolution,
      x_dim_range = x_dim_range,
      y_dim_range = y_dim_range,
      flow_field  = flow_field,
      step_length = step_length
    )
    
    # Collision check #2
    # Subsequent curve building steps will be cheecked against the circles
    for (circle in circles){
      line_crosses_circle <- line_to_circle_collision(line_x    = x_container[[i]], 
                                                      line_y    = y_container[[i]], 
                                                      line_xend = next_step$x, 
                                                      line_yend = next_step$y,
                                                      
                                                      circle_x       = circle$x, 
                                                      circle_y       = circle$y, 
                                                      circle_r       = circle$r,
                                                      circle_padding = circle$padding)
      
      if (line_crosses_circle) {
        return(
          tibble::tibble(
            x = x_container %>% subset(. != 0), # return the progress right up until collision
            y = y_container %>% subset(. != 0),
            row_num = row_num # curve grouping
          ) %>%  
            dplyr::mutate(plot_order = dplyr::row_number())
        )
      }
    }
    
    # If no collision, record
    x_container[i+1] <- next_step$x
    y_container[i+1] <- next_step$y
    
  }
  
  tibble::tibble(
    x = x_container,
    y = y_container,
    row_num = row_num # curve grouping
  ) %>%   
    dplyr::mutate(plot_order = dplyr::row_number()) 
  
}