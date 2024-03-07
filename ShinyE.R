# Load packages
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)

# Authentication options
options(
  # Use cached token if found
  gargle_oauth_email = TRUE,
  # Store tokens in a hidden directory
  gargle_oauth_cache = "C:/Users/Roy Gicheru/Documents/R PROJECTS/Shiny/ShinyPrac/.secrets"
)

# Get survey sheet ID
sheet_id <- drive_get("surveys")$id

# Handle sheet creation if not found
if (is.null(sheet_id)) {
  # Create the sheet if it doesn't exist
  sheet_id <- drive_create(title = "surveys", mimeType = "application/vnd.google-apps.spreadsheet")$id
  message("Created a new sheet called 'surveys'.")
} else {
  message("Using existing sheet 'surveys'.")
}

# Define survey questions
survey_questions <- data.frame(
  question = c("What is your favorite food?",
               "What's your name?"),
  option = NA,
  input_type = "text",
  input_id = c("favorite_food", "name"),
  dependence = NA,
  dependence_value = NA,
  required = c(TRUE, FALSE)
)

# Shiny UI
ui <- fluidPage(
  surveyOutput(survey_questions,
               survey_title = "Hello, World!",
               survey_description = "A demo survey")
)

# Shiny server
server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    response_data <- getSurveyData()
    
    # Read the sheet (handle potential errors)
    values <- tryCatch({
      read_sheet(ss = sheet_id, sheet = "surveys")
    }, error = function(e) {
      message(paste("Error reading sheet:", e))
      return(NULL)
    })
    
    # Check for existing data
    if (is.null(values) || nrow(values) == 0) {
      # Write column names and data if sheet is empty or error occurred
      sheet_write(data = response_data, ss = sheet_id, sheet = "surveys")
      message("Survey data written to a new 'surveys' sheet.")
    } else {
      # Append data to existing sheet
      sheet_append(data = response_data, ss = sheet_id, sheet = "surveys")
      message("Survey data appended to existing 'surveys' sheet.")
    }
  })
}

# Run the Shiny application
shinyApp(ui, server)
