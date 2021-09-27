plotter <- function(background_clr,
                    circle_clr,
                    flow_field_width,
                    border_length, 
                    flow_field_outline, 
                    flow_field_curves,
                    circles, # circles is in plot friendly form
                    row_num,
                    line_alpha,
                    clr_palette,
                    show_ff,
                    use_frame = FALSE,
                    frame_length = NULL,
                    frame_colour = NULL) 
{
  
  # Canvas size (bigger than flow field)
  canvas_small_coord   <- flow_field_width * -border_length #Need to define better
  canvas_big_coord  <- flow_field_width * (1+border_length)
  
  # border around the drawing
  my_borders <- tibble::tibble(
    x = c(canvas_small_coord, canvas_small_coord, canvas_big_coord, canvas_big_coord,
          canvas_small_coord, canvas_small_coord, canvas_big_coord, canvas_big_coord,
          canvas_small_coord, canvas_small_coord, 0, 0,
          flow_field_width, flow_field_width, canvas_big_coord, canvas_big_coord),
    y = c(canvas_small_coord, 0, 0, canvas_small_coord,
          flow_field_width, canvas_big_coord, canvas_big_coord, flow_field_width,
          canvas_small_coord, canvas_big_coord, canvas_big_coord, canvas_small_coord,
          canvas_small_coord, canvas_big_coord, canvas_big_coord, canvas_small_coord),
    group = c(rep("bot", 4),
              rep("top", 4),
              rep("left", 4),
              rep("right", 4))
  )
  
  if(use_frame) {
    frame_small_coord <- canvas_small_coord - flow_field_width*frame_length
    frame_big_coord <- canvas_big_coord + flow_field_width*frame_length
    
    my_frame <- tibble::tibble(
      x = c(frame_small_coord, frame_small_coord, frame_big_coord, frame_big_coord,
            frame_small_coord, frame_small_coord, frame_big_coord, frame_big_coord,
            frame_small_coord, frame_small_coord, canvas_small_coord, canvas_small_coord,
            canvas_big_coord, canvas_big_coord, frame_big_coord, frame_big_coord),
      y = c(frame_small_coord, canvas_small_coord, canvas_small_coord, frame_small_coord,
            canvas_big_coord, frame_big_coord, frame_big_coord, canvas_big_coord,
            frame_small_coord, frame_big_coord, frame_big_coord, frame_small_coord,
            frame_small_coord, frame_big_coord, frame_big_coord, frame_small_coord),
      group = c(rep("bot", 4),
                rep("top", 4),
                rep("left", 4),
                rep("right", 4))
    )
    
    frame <- ggplot2::geom_polygon(data = my_frame,
                          ggplot2::aes(x = x, y = y, group = group),
                          fill = frame_colour)
    
    output_range <- c(frame_small_coord, frame_big_coord)
  } else{
    frame <- NULL
    output_range <- c(canvas_small_coord, canvas_big_coord)
  }
  
  ff_helper <- if (show_ff) { ggplot2::geom_segment(data = flow_field_outline,
                                                 ggplot2::aes(x = x,
                                                              y = y,
                                                              xend = xend,
                                                              yend = yend),
                                                 color = "#ffffff")} else{NULL}
  
  ggplot2::ggplot() +
    ff_helper +
    ggforce::geom_circle(data = circles,
                         ggplot2::aes(x0 = x0,
                                      y0 = y0,
                                      r = r),
                         fill = circle_clr,
                         color = circle_clr) +
    ggplot2::geom_path(data = flow_field_curves,
                       ggplot2::aes(x = x,
                                    y = y,
                                    group = row_num,
                                    color = clr),
                       alpha = line_alpha,
                       show.legend = FALSE) +
    ggplot2::geom_polygon(data = my_borders,
                          ggplot2::aes(x = x, y = y, group = group),
                          fill = background_clr) +
    frame +
    ggplot2::coord_equal(
      xlim = output_range,
      ylim = output_range,
      expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background_clr,
                                               color = background_clr)
    ) +
    ggplot2::scale_color_manual(values = clr_palette)
}