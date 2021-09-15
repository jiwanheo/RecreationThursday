call_everything <- function(flow_field_width,
                            resolution_factor,
                            perlin_scale_factor,
                            perlin_seed,
                            perlin_freq,
                            
                            n_out,
                            num_steps,
                            step_length, 
                            
                            circles) # circles is in list form
{
  flow_field_list <- generate_flow_field(flow_field_width = flow_field_width, 
                                         resolution_factor = resolution_factor,
                                         perlin_scale_factor = perlin_scale_factor,
                                         perlin_seed = perlin_seed,
                                         perlin_freq = perlin_freq)
  
  flowed_curves <- calculate_flow_fields(n_out = n_out,
                                         flow_field_width = flow_field_width,
                                         num_steps = num_steps, 
                                         step_length = step_length,
                                         flow_field = flow_field_list[[1]],
                                         resolution_factor = resolution_factor,
                                         circles = circles)
  
  plot_friendly_circles <- tibble::tibble(
    x0 = circles %>% purrr::map_dbl("x0"),
    y0 = circles %>% purrr::map_dbl("y0"),
    r  = circles %>% purrr::map_dbl("r")
  )
  
  my_plot <- plotter(flow_field_width = flow_field_width,
                     flow_field_outline = flow_field_list[[2]],
                     flow_field_curves = flowed_curves,
                     circles = plot_friendly_circles)
  
  plot_name <- paste("flow_field_width", flow_field_width,
                     "resolution_factor", round(resolution_factor, digits = 5),
                     "perlin_scale_factor", round(perlin_scale_factor, digits = 5),
                     "perlin_seed", perlin_seed,
                     "perlin_freq", perlin_freq,
                     "num_steps", num_steps,
                     "step_length", round(step_length, digits = 5),
                     ".png")
  
  list(plot = my_plot, plot_name = plot_name, flowed_curves = flowed_curves)
}