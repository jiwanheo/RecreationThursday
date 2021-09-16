colour_assign <- function(flowed_curves,
                          clr1, 
                          clr2, 
                          max_clr_prob,
                          min_clr_prob = 1-max_clr_prob) 
{
  
  # y_meaned <- flowed_curves %>% 
  #   dplyr::group_by(row_num) %>% # this grouping gives colour assignment per unique curve.
  #   dplyr::summarize(y = mean(y), .groups = "drop")
  # 
  # max_y_in_ff <- max(y_meaned$y)
  # min_y_in_ff <- min(y_meaned$y)
  
  max_y_in_ff <- max(flowed_curves$y)
  min_y_in_ff <- min(flowed_curves$y)
  
  y_scaled <- flowed_curves %>%  # this grouping gives colour per curve segment
    dplyr::mutate(y = (max_clr_prob-min_clr_prob) / (max_y_in_ff-min_y_in_ff) * (y-max_y_in_ff) + max_clr_prob)
  
  y_clr_drawn <- y_scaled %>% 
    dplyr::mutate(clr = purrr::map_chr(y, ~sample(c(clr1, clr2), size = 1, prob = c(.x, 1-.x)))) %>% 
    dplyr::select(row_num, plot_order, clr)
  
  my_clrs <- c(clr1, clr2)
  my_clrs <- my_clrs %>% rlang::set_names(nm = my_clrs)
  
  list(assigned_clrs = y_clr_drawn, palette = my_clrs)
}

