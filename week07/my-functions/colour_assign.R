colour_assign <- function(flowed_curves,
                          clr_method,
                          clr1, 
                          clr2, 
                          max_clr_prob,
                          min_clr_prob = 1-max_clr_prob) 
{
  
  # Grouping by mean y position per line--------------------
  if(clr_method == "line-mean") {
    y_meaned <- flowed_curves %>%
      dplyr::group_by(row_num) %>% 
      dplyr::summarize(y = mean(y), .groups = "drop")
    
    max_y_in_ff <- max(y_meaned$y)
    min_y_in_ff <- min(y_meaned$y)
    
    
    y_scaled <- y_meaned %>%  
      dplyr::mutate(y = (max_clr_prob-min_clr_prob) / (max_y_in_ff-min_y_in_ff) * (y-max_y_in_ff) + max_clr_prob) %>%
      dplyr::mutate(clr = purrr::map_chr(y, ~sample(c(clr1, clr2), size = 1, prob = c(.x, 1-.x))))
    
    y_clr_drawn <- flowed_curves %>%
      dplyr::left_join(y_scaled, by = "row_num") %>%
      dplyr::select(row_num, plot_order, clr)
  }
  
  # Grouping by line's starting point------------------------
  if(clr_method == "line-start") {
    first_y <- flowed_curves %>%
      dplyr::filter(plot_order == 1) %>%
      dplyr::select(-plot_order)
    
    max_y_in_ff <- max(first_y$y)
    min_y_in_ff <- min(first_y$y)
    
    y_scaled <- first_y %>%  # this grouping gives colour per curve segment
      dplyr::mutate(y = (max_clr_prob-min_clr_prob) / (max_y_in_ff-min_y_in_ff) * (y-max_y_in_ff) + max_clr_prob) %>%
      dplyr::mutate(clr = purrr::map_chr(y, ~sample(c(clr1, clr2), size = 1, prob = c(.x, 1-.x))))
    
    y_clr_drawn <- flowed_curves %>%
      dplyr::left_join(y_scaled, by = "row_num") %>%
      dplyr::select(row_num, plot_order, clr)
  }
  
  # Grouping by single line segment------------------------
  if(clr_method == "segment") {
    max_y_in_ff <- max(flowed_curves$y)
    min_y_in_ff <- min(flowed_curves$y)
    
    y_scaled <- flowed_curves %>%  # this grouping gives colour per curve segment
      dplyr::mutate(y = (max_clr_prob-min_clr_prob) / (max_y_in_ff-min_y_in_ff) * (y-max_y_in_ff) + max_clr_prob)
    
    y_clr_drawn <- y_scaled %>%
      dplyr::mutate(clr = purrr::map_chr(y, ~sample(c(clr1, clr2), size = 1, prob = c(.x, 1-.x)))) %>%
      dplyr::select(row_num, plot_order, clr)
  }
  
  # Return values -------------------------------------
  
  my_clrs <- c(clr1, clr2)
  my_clrs <- my_clrs %>% rlang::set_names(nm = my_clrs)
  
  list(assigned_clrs = y_clr_drawn, palette = my_clrs)
}

