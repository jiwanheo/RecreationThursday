plotter <- function(background_clr = "#e7e2df",
                    line_clr = "#914062",
                    circle_clr = "#a48370",
                    flow_field_width,
                    flow_field_outline, 
                    flow_field_curves,
                    circles, # circles is in plot friendly form
                    row_num) 
{
  
  # The actual size of the flow field
  flow_field_width  <- flow_field_width
  flow_field_height <- flow_field_width
  
  # Canvas size (bigger than flow field)
  canvas_left_x   <- flow_field_width * -0.5
  canvas_right_x  <- flow_field_width * 1.5
  canvas_top_y    <- flow_field_height * -0.5
  canvas_bottom_y <- flow_field_height * 1.5
  
  ggplot2::ggplot() +
    # ggplot2::geom_segment(data = flow_field_outline,
    #                       ggplot2::aes(x = x,
    #                                    y = y,
    #                                    xend = xend,
    #                                    yend = yend),
    #                       color = "grey20") +
    ggforce::geom_circle(data = circles,
                         ggplot2::aes(x0 = x0,
                                      y0 = y0,
                                      r = r),
                         fill = circle_clr,
                         color = circle_clr) +
    ggplot2::geom_path(data = flow_field_curves,
                       ggplot2::aes(x = x,
                                    y = y,
                                    group = row_num),
                       color = line_clr,
                       alpha = 1) +
    ggplot2::coord_equal(xlim = c(canvas_left_x, canvas_right_x),
                         ylim = c(canvas_top_y, canvas_bottom_y)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background_clr,
                                              color = background_clr)
    )
}