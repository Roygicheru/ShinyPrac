# # Load packages ----------------------------------------------------------------
# 
# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(DT)
# 
# # Load data --------------------------------------------------------------------
# 
# load("movies.RData")
# n_total <- nrow(movies)
# 
# # Define UI --------------------------------------------------------------------
# 
# ui <- fluidPage(
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       
#       HTML(paste("Enter a value between 1 and", n_total)),
#       
#       numericInput(inputId = "n",
#                    label = "Sample size:",
#                    value = 30,
#                    step = 1,
#                    min = 1, max = n_total)
#       
#     ),
#     
#     mainPanel(
#       DT::dataTableOutput(outputId = "moviestable")
#     )
#   )
# )
# 
# # Define server ----------------------------------------------------------------
# 
# server <- function(input, output, session) {
#   
#   output$moviestable <- DT::renderDataTable({
#     req(input$n)
#     movies_sample <- movies %>%
#       sample_n(input$n) %>%
#       select(title:studio)
#     datatable(
#       data = movies_sample,
#       options = list(pageLength = 10),
#       rownames = FALSE)
#   })
#   
# }
# 
# # Create the Shiny app object --------------------------------------------------
# # shinyApp(ui = ui, server = server)




# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

load("movies.RData")
min_date <- min(movies$thtr_rel_date)
max_date <- max(movies$thtr_rel_date)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      
      HTML(paste0("Movies released since the following date will be plotted.
                 Pick a date between ", min_date, " and ", max_date, ".")),
      
      br(), br(),
      
      dateRangeInput(
        inputId = "date",
        label = "Select date range:",
        start = "2013-01-01", end = "2014-01-01",
        min = min_date, max = max_date,
        startview = "year")
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    req(input$date)
    movies_selected_date <- movies %>%
      #filter(thtr_rel_date >= as.POSIXct(input$date) & thtr_rel_date <= as.POSIXct(input$date[2]))
      filter(between(thtr_rel_date, input$date[1], input$date[2]))
    ggplot(data = movies_selected_date, aes(x = critics_score, y = audience_score, color = mpaa_rating)) +
      geom_point()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)