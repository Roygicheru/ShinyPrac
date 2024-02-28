# Load packages

library(shiny)
library(ggplot2)
library(dplyr)

# Get the data

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destfile <- "movies.RData"

download.file(file, destfile)

# Load data

load("movies.RData")

# Define UI

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        c("IMDB Rating" = "imdb_rating",
          "IMDB Number of Votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"),
        selected = "audience_score"
      ),
      
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        c("IMDB Rating" = "imdb_rating",
          "IMDB Number of Votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"),
        selected = "critics_score"
      ),
      
      # Select variable for the color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("Title Type" = "title_type", 
                    "Genre" = "genre", 
                    "MPAA Rating" = "mpaa_rating", 
                    "Critics Rating" = "critics_rating", 
                    "Audience Rating" = "audience_rating"),
        selected = "mpaa_rating"
      ),
      
      # Subset for title types
      checkboxGroupInput(inputId = "selected_title_type",
                         label = "Select title type:",
                         choices = levels(movies$title_type),
                         selected = levels(movies$title_type)),
      ),
    
    mainPanel(
      # Show scatterplot
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      # Show data table
      tableOutput(outputId = "summarytable"),
      # Show data table
      dataTableOutput(outputId = "moviestable"),
      br()
    )
  )
)

# Define server function required to create the scatterplot

server <- function(input, output, session) {
  
  #create the scatterplot object to the plotOutput function
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes(x = .data[[input$x]], y = .data[[input$y]], color = .data[[input$z]])) +
      geom_point()
  })
  
  output$summarytable <- renderTable(
    {
      movies <- movies %>%
        mutate(score_ratio = audience_score / critics_score) %>%
        filter(title_type %in% input$selected_title_type) %>%
        group_by(mpaa_rating) %>%
        summarise(mean_score_ratio = mean(score_ratio), SD = sd(score_ratio), n = n())
    },
    striped = TRUE,
    spacing = "l",
    align = "lccr",
    digits = 4,
    width = "90%",
    caption = "Score ratio (audience / critics' scores) summary statistics by MPAA rating."
  )
  
  output$moviestable <- renderDataTable({
    nearPoints(movies, brush = input$plot_hover) %>%
      select(title, audience_score, critics_score)
  })
}


# Create a Shiny app object

shinyApp(ui = ui, server = server)