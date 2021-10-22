library(shiny)
library(tidyverse)
library(bslib)
source(here::here("week09", "my-functions.R"))
source(here::here("week09", "mod-generate_data.R"))
source(here::here("week09", "mod-plot.R"))
source(here::here("week09", "mod-download.R"))

# Custom themes ----
my_theme <- bs_theme(
  bootswatch = "sandstone",
  heading_font = font_google("Fira Sans")
) %>% 
  bs_add_rules(sass::sass_file(here::here("week09", "style.scss")))

ui <- fluidPage(
  theme = my_theme,
  
  div(id = "app-title",
      titlePanel("Jiwan's awesome grid generator")),
  
  sidebarLayout(
    sidebarPanel(
      generate_data_input("my_data"),
      div(id = "download-btn",
          download_ui("download"))
    ),
    mainPanel(
      textOutput("app_title"),
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output, session) {
  
  all_dfs <- generate_data_server("my_data")
  
  p <- plot_server("my_plot", dfs = all_dfs)
  output$my_plot <- renderPlot(p(), res = 50, width = 500, height = 500)
  
  download_server("download", p)
  
}

shinyApp(ui, server)