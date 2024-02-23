# # library(shiny)
# # 
# # ui <- fluidPage(
# #   
# #   textInput(
# #     inputId = "custom_text",
# #     label = "Input some text here"
# #   ),
# #   
# #   strong("Text is shown below:"),
# #   
# #   textOutput(outputId = "user_text")
# #   
# # )
# # 
# # server <- function(input, output, session){
# #   
# #   output$user_text <- renderText({ input$custom_text })
# #   
# # }
# # 
# # shinyApp(ui = ui, server = server)
# 
# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score" = "critics_score",
          "Audience score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "audience_score"
      ),

      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score" = "critics_score",
          "Audience score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "critics_score"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      # Select variable for color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Title type" = "title_type",
          "Genre" = "genre",
          "MPAA rating" = "mpaa_rating",
          "Critics rating" = "critics_rating",
          "Audience rating" = "audience_rating"
        ),
        selected = "mpaa_rating"
      )
    ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200)
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({

    ggplot(data = movies, aes(x = .data[[input$x]], y = .data[[input$y]], color = .data[[input$z]])) +
      geom_point(alpha = input$alpha)

  })
  
  output$densityplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x)) +
      geom_density()
  })

}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)