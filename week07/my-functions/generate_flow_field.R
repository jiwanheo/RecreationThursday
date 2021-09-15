generate_flow_field <- function(flow_field_width = 1000, 
                                resolution_factor = 0.025,
                                perlin_scale_factor = 0.005,
                                perlin_seed,
                                perlin_freq) {
  
  # Flow field grid dimension
  resolution <- flow_field_width * resolution_factor 
  num_cols <- flow_field_width %/% resolution
  num_rows <- num_cols
  
  # Generating perlin noise
  long_grid_ff <- ambient::long_grid(x = 1:num_cols,
                                     y = 1:num_rows) %>% 
    dplyr::mutate(x = x * perlin_scale_factor,
                  y = y * perlin_scale_factor) %>% 
    dplyr::mutate(angle = ambient::gen_perlin(x, 
                                              y,
                                              seed = perlin_seed,
                                              frequency = perlin_freq))
  
  min_per <- min(long_grid_ff$angle)
  max_per <- max(long_grid_ff$angle)
  
  # normalize angles to be between 0 & 2pi
  long_grid_ff <- long_grid_ff %>% 
    dplyr::mutate(angle = (angle-min_per) / (max_per-min_per) *  (2*pi-0) + 0)
  
  my_flow_field <- matrix(data = long_grid_ff$angle,
                          ncol = num_cols,
                          nrow = num_rows)
  
  
  visualized_flow_field <- tidyr::crossing(
    x = 1:num_cols,
    y = 1:num_rows
  ) %>% 
    # Look up the angles in the matrix at the x/y cols
    dplyr::mutate(angle = purrr::map2_dbl(x, y, ~my_flow_field[[.y, .x]])) %>% 
    dplyr::mutate(xend = x + cos(angle) * 0.5,
                  yend = y + sin(angle) * 0.5) %>% 
    dplyr::mutate(x_index = x, y_index = y) %>% 
    dplyr::mutate(dplyr::across(c(x, y, xend, yend), ~ .x * resolution))
  
  
  list(my_flow_field, visualized_flow_field)
}