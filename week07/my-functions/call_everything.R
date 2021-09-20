call_everything <- function(flow_field_width,
                            resolution_factor,
                            perlin_scale_factor,
                            perlin_seed,
                            perlin_freq,
                            
                            n_out,
                            num_steps,
                            step_length, 
                            
                            circles,# circles is in list form
                            
                            clr1,
                            clr2,
                            max_clr_prob,
                            
                            background_clr,
                            circle_clr,
                            border_length,
                            current_time,
                            plot_save,
                            row_num) 
{
  flow_field_list <- generate_flow_field(flow_field_width = flow_field_width, 
                                         resolution_factor = resolution_factor,
                                         perlin_scale_factor = perlin_scale_factor,
                                         perlin_seed = perlin_seed,
                                         perlin_freq = perlin_freq)
  
  flowed_curves <- start_particles(n_out = n_out,
                                   flow_field_width = flow_field_width,
                                   num_steps = num_steps, 
                                   step_length = step_length,
                                   flow_field = flow_field_list[[1]],
                                   resolution_factor = resolution_factor,
                                   circles = circles)
  
  flow_curves_clrs <- colour_assign(flowed_curves = flowed_curves, 
                                    clr1 = clr1, 
                                    clr2 = clr2, 
                                    max_clr_prob = max_clr_prob)
  
  flowed_curves <- flowed_curves %>% 
    dplyr::left_join(flow_curves_clrs$assigned_clrs, by = c("row_num", "plot_order"))
  
  plot_friendly_circles <- tibble::tibble(
    x0 = circles %>% purrr::map_dbl("x0"),
    y0 = circles %>% purrr::map_dbl("y0"),
    r  = circles %>% purrr::map_dbl("r")
  )
  
  my_plot <- plotter(background_clr = background_clr,
                     circle_clr = circle_clr,
                     flow_field_width = flow_field_width,
                     border_length = border_length,
                     flow_field_outline = flow_field_list[[2]],
                     flow_field_curves = flowed_curves,
                     circles = plot_friendly_circles,
                     clr_palette = flow_curves_clrs$palette)
  
  plot_name <- paste(flow_field_width,
                     round(resolution_factor, digits = 5),
                     round(perlin_scale_factor, digits = 5),
                     perlin_seed,
                     perlin_freq,
                     
                     n_out,
                     num_steps,
                     round(step_length, digits = 5),
                     
                     clr1,
                     clr2,
                     round(max_clr_prob, digits = 5),
                     
                     background_clr,
                     border_length,
                     ".png")
  
  
  print(my_plot)
  
  if(plot_save) {
    
    print(paste0("Saving plot #", row_num))
    
    ggplot2::ggsave(
      here::here("week07", "progress", current_time, plot_name),
      plot      = my_plot,
      device    = ragg::agg_png,
      res       = 100,
      units     = "in",
      width     = 1000,
      height    = 1000,
      limitsize = FALSE
    )
  } 
  
}