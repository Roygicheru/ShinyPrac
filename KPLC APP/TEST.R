# # Load the shiny and openxlsx packages
# library(shiny)
# library(openxlsx)
# 
# # Define the UI
# ui <- fluidPage(
#   # A title for the app
#   titlePanel("Employee Training App"),
#   
#   tags$head(
#     tags$style(HTML("
#       #internal:hover, #corporate:hover {
#         background-color: #00008B; /* Dark blue */
#         color: white; /* White text */
#       }
#     "))
#   ),
#   
#   # A main panel with two columns
#   mainPanel(
#     # A column for internal employee training
#     column(6,
#            # A button to toggle the input fields
#            actionButton("internal", "Internal Employee Training"),
#            # A conditional panel that shows the input fields when the button is clicked
#            conditionalPanel(
#              condition = "input.internal > 0",
#              # An input field for the first name of the participant
#              textInput("fname", "First Name"),
#              # An input field for the second name of the participant
#              textInput("sname", "Second Name"),
#              # An input field for the surname of the participant
#              textInput("lname", "Surname"),
#              # An input field for the staff number of the participant
#              numericInput("staff", "Staff Number", value = NULL),
#              # An input field for the designation of the participant
#              textInput("desig", "Designation"),
#              # An input field for the mobile number of the participant
#              numericInput("mobile", "Mobile Number", value = NULL),
#              # An input field for the station/region of the participant
#              textInput("station", "Station/Region"),
#              # An input field for the supervisor of the participant
#              textInput("super", "Supervisor"),
#              # A submit button that saves the input to an excel sheet
#              actionButton("submit1", "Submit")
#            )
#     ),
#     
#     # A column for corporate employee training
#     column(6,
#            # A button to toggle the input fields
#            actionButton("corporate", "Corporate Employee Training"),
#            # A conditional panel that shows the input fields when the button is clicked
#            conditionalPanel(
#              condition = "input.corporate > 0",
#              # An input field for the name of the client
#              textInput("name2", "Name"),
#              # An input field for the ID number of the client
#              numericInput("id2", "ID Number", value = NULL),
#              # A submit button that saves the input to an excel sheet
#              actionButton("submit2", "Submit")
#            )
#     )
#   )
# )
# 
# # Define the server
# server <- function(input, output, session) {
#   # A reactive value to store the data
#   data <- reactiveValues()
#   
#   # A function to append a row to an excel sheet
#   append_row <- function(file, row) {
#     # If the file does not exist, create it with column names
#     if (!file.exists(file)) {
#       write.xlsx(c("First Name", "Second Name", "Surname", "Staff Number", "Designation", "Mobile Number", "Station/Region", "Supervisor"), file, colNames = FALSE)
#     }
#     # Read the existing data from the file
#     old_data <- read.xlsx(file)
#     # Append the new row to the old data
#     new_data <- rbind(old_data, row)
#     # Write the new data to the file
#     write.xlsx(new_data, file, colNames = FALSE)
#   }
#   
#   
#   # Observe the submit button for internal employee training
#   observeEvent(input$submit1, {
#     # Get the input values
#     fname <- input$fname
#     sname <- input$sname
#     lname <- input$lname
#     staff <- input$staff
#     desig <- input$desig
#     mobile <- input$mobile
#     station <- input$station
#     super <- input$super
#     # Validate the input values
#     if (is.null(fname) || is.null(sname) || is.null(lname) || is.null(staff) || is.null(desig) || is.null(mobile) || is.null(station) || is.null(super)) {
#       # Show an error message
#       showNotification("Please enter all the required fields", type = "error")
#     } else {
#       # Save the input values to a reactive value
#       data$internal <- c(fname, sname, lname, staff, desig, mobile, station, super)
#       # Append the input values to the excel sheet for internal employee training
#       append_row("internal.xlsx", data$internal)
#       # Show a success message
#       showNotification("Data saved successfully", type = "message")
#     }
#   })
#   
#   # Observe the submit button for corporate employee training
#   observeEvent(input$submit2, {
#     # Get the input values
#     name <- input$name2
#     id <- input$id2
#     # Validate the input values
#     if (is.null(name) || is.null(id)) {
#       # Show an error message
#       showNotification("Please enter both name and ID number", type = "error")
#     } else {
#       # Save the input values to a reactive value
#       data$corporate <- c(name, id)
#       # Append the input values to the excel sheet for corporate employee training
#       append_row("corporate.xlsx", data$corporate)
#       # Show a success message
#       showNotification("Data saved successfully", type = "message")
#     }
#   })
# }
# 
# # Run the app
# shinyApp(ui, server)



library(shiny)
library(openxlsx)

# Define the UI
ui <- fluidPage(
  # A title for the app
  titlePanel("Employee Training App"),
  
  tags$head(
    tags$style(HTML("
      #internal:hover, #corporate:hover {
        background-color: #00008B; /* Dark blue */
        color: white; /* White text */
      }
    "))
  ),
  
  # A main panel with two columns
  mainPanel(
    column(6,
           actionButton("internal", "Internal Employee Training"),
           uiOutput("internalUI")
    ),
    column(6,
           actionButton("corporate", "Corporate Employee Training"),
           uiOutput("corporateUI")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # A reactive value to store the data
  data <- reactiveValues()
  
  # Dynamic UI for internal employee training
  output$internalUI <- renderUI({
    if(input$internal > 0 && input$corporate == 0) {
      fluidRow(
        textInput("fname", "First Name"),
        textInput("sname", "Second Name"),
        textInput("lname", "Surname"),
        numericInput("staff", "Staff Number", value = NULL),
        textInput("desig", "Designation"),
        numericInput("mobile", "Mobile Number", value = NULL),
        textInput("station", "Station/Region"),
        textInput("super", "Supervisor"),
        actionButton("submit1", "Submit")
      )
    }
  })
  
  # Dynamic UI for corporate employee training
  output$corporateUI <- renderUI({
    if(input$corporate > 0 && input$internal == 0) {
      fluidRow(
        textInput("name2", "Name"),
        numericInput("id2", "ID Number", value = NULL),
        actionButton("submit2", "Submit")
      )
    }
  })
  
  # Reset action buttons when the other is pressed
  observe({
    if(input$internal > 0) {
      updateActionButton(session, "corporate", label = "Corporate Employee Training")
    }
    if(input$corporate > 0) {
      updateActionButton(session, "internal", label = "Internal Employee Training")
    }
  })
  
  # A function to append a row to an excel sheet
  append_row <- function(file, row) {
    # If the file does not exist, create it with column names
    if (!file.exists(file)) {
      write.xlsx(c("First Name", "Second Name", "Surname", "Staff Number", "Designation", "Mobile Number", "Station/Region", "Supervisor"), file, colNames = FALSE)
    }
    # Read the existing data from the file
    old_data <- read.xlsx(file)
    # Append the new row to the old data
    new_data <- rbind(old_data, row)
    # Write the new data to the file
    write.xlsx(new_data, file, colNames = FALSE)
  }
  
  # Observe the submit button for internal employee training
  observeEvent(input$submit1, {
    # Get the input values
    fname <- input$fname
    sname <- input$sname
    lname <- input$lname
    staff <- input$staff
    desig <- input$desig
    mobile <- input$mobile
    station <- input$station
    super <- input$super
    # Validate the input values
    if (is.null(fname) || is.null(sname) || is.null(lname) || is.null(staff) || is.null(desig) || is.null(mobile) || is.null(station) || is.null(super)) {
      # Show an error message
      showNotification("Please enter all the required fields", type = "error")
    } else {
      # Save the input values to a reactive value
      data$internal <- c(fname, sname, lname, staff, desig, mobile, station, super)
      # Append the input values to the excel sheet for internal employee training
      append_row("internal.xlsx", data$internal)
      # Show a success message
      showNotification("Data saved successfully", type = "message")
    }
  })
  
  # Observe the submit button for corporate employee training
  observeEvent(input$submit2, {
    # Get the input values
    name <- input$name2
    id <- input$id2
    # Validate the input values
    if (is.null(name) || is.null(id)) {
      # Show an error message
      showNotification("Please enter both name and ID number", type = "error")
    } else {
      # Save the input values to a reactive value
      data$corporate <- c(name, id)
      # Append the input values to the excel sheet for corporate employee training
      append_row("corporate.xlsx", data$corporate)
      # Show a success message
      showNotification("Data saved successfully", type = "message")
    }
  })
}

# Run the app
shinyApp(ui, server)
