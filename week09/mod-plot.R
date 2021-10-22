plot_server <- function(id, dfs) {
  moduleServer(id, function(input, output, session) {
    p <- reactive({
      req(!is.null(dfs()$data$outer_grid_plot_ready))
      
      ggplot() +
        geom_polygon(data = dfs()$data$outer_grid_plot_ready, aes(x = x, y = y, group = square_id), fill = "#e9f7f7") +
        geom_polygon(data = dfs()$data$shrunk_grid_plot_ready, aes(x = x, y = y, group = square_id), fill = "#e9f7f7") +
        geom_segment(data = dfs()$data$gradient_lines, aes(x = x, xend = new_xend, y = y, yend = new_yend), color = "#072f62") +
        coord_equal() +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "#072f62", color = "#072f62")
        )
    })

    p
    
  })
}
