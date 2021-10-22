download_ui <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("download"), label = "Download image!")
}

download_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    
    output$download <- downloadHandler(
      filename = function() {"awesome-grid.png"},
      content = function(file) {
        ggsave(
          file,
          plot = p(),
          device = ragg::agg_png,
          res = 300,
          width = 3000,
          height = 3000,
          units = "px"
        )
        
        showNotification("Downloaded")
      }
    )
  }) 
}