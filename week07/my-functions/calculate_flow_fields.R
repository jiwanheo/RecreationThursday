calculate_flow_fields <- function(n_out = 800,
                                  flow_field_width,
                                  num_steps,  
                                  step_length,
                                  flow_field,
                                  resolution_factor,
                                  circles) # circles is in list form
{      
  df <- tibble::tibble(
    start_x = runif(-flow_field_width*0.1, flow_field_width*1.1, n = n_out),
    start_y = runif(-flow_field_width*0.1, flow_field_width*1.1, n = n_out)
    
    # start_x = seq(100, 500, length.out = 10), 
    # start_y = seq(100, 1000, length.out = 10)
    
  ) %>%
    dplyr::mutate(row_num = dplyr::row_number(),
                  resolution = flow_field_width * resolution_factor,
                  num_steps = num_steps,
                  step_length = step_length)
  
  df %>% 
    purrr::pmap_dfr(draw_curve, 
                    flow_field = flow_field,
                    circle = circles)
}