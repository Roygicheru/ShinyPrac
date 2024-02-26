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
library(DT)
# options("shiny.sanitize.errors" = FALSE) # Turn off error sanitization

# Load data --------------------------------------------------------------------

load("movies.RData")
all_studios <- sort(unique(movies$studio))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "studio",
                  label = "Select studio:",
                  choices = all_studios,
                  selectize = TRUE,
                  multiple = TRUE,
                  selected = "20th Century Fox")
      
    ),
    
    mainPanel(
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$moviestable <- renderDataTable({
    req(input$studio)
    movies_from_selected_studios <- movies %>%
      #filter(studio == input$studio) %>%
      filter(studio %in% input$studio) %>%
      select(title:studio)
    DT::datatable(data = movies_from_selected_studios,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
}

# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)