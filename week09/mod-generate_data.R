generate_data_input <- function(id) {
  ns <- NS(id)
  
  tagList(
    labeled_input(id = "input-num_rows",
                  label = "Grid dimensions",
                  input = numericInput(ns("num_rows"), NULL, min = 1, max = 50, value = 3)),
    labeled_input(id = "input-n_lines",
                  label = "How many gradient lines?",
                  input = numericInput(ns("n_lines"), NULL, min = 1, max = 100, value = 10)),
    div(id = "action-generate_data",
        actionButton(ns("generate_data"), "Generate Image!"))
  )
}

generate_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    all_dfs <- reactiveValues(data = NULL)

    observeEvent(input$generate_data, { 
      all_dfs$data <- generate_output(num_rows = input$num_rows, n_lines = input$n_lines)
    })
    
    reactive(all_dfs)
  })
}