library(shiny)
library(openxlsx)
library(shinyjs)

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .btn-primary {
      background-color: #2c4c8b; /* Custom button color */
      border: none; /* No border */
      color: white; /* White text color */
    }
    .btn-primary:hover {
      background-color: #1a3360; /* Darker button color on hover */
    }
       #header {
      color: white;
      background-color: #2c4c8b;
      padding: 20px;
      display: flex;
      align-items: center;
      font-size: 40px;
    }
    #header img {
      align-items: left;
      margin-right: 10px
    }
    .button-row {
      display: flex;
    }
      /* Add this part for responsive design */
      @media (max-width: 600px) {
        .button-row {
          flex-direction: column;
        }
      }
    "))
  ),
  tags$header(
    id = "header",
    img(src = "https://is3-ssl.mzstatic.com/image/thumb/Purple115/v4/16/95/55/16955554-bfca-ecd4-9ec7-d9c92350df9c/source/512x512bb.jpg", 
        height = "120px", width = "250px"),
    "Training Registration Page"),
  titlePanel(" ", windowTitle = "Training Registration"),
  
  tags$head(
    tags$style(HTML("
      #internal:hover, #corporate:hover {
        background-color: #2c4c8b;
        color: white;
      }
    "))
  ),
  
  div(class = "button-row",
      column(6,
             actionButton("internal", "Internal Employee Training"),
             br(), br(), # Line break after the internal button
             uiOutput("internalUI")
      ),
      column(6,
             actionButton("corporate", "Corporate Employee Training"),
             br(), br(), # Line break after the corporate button
             uiOutput("corporateUI")
      )
  )
)

# Define the server
server <- function(input, output, session) {
  activeMenu <- reactiveVal(NULL)
  
  output$internalUI <- renderUI({
    if(!is.null(activeMenu()) && activeMenu() == "internal") {
      fluidRow(
        textInput("fname", "First Name"),
        textInput("sname", "Second Name"),
        textInput("lname", "Surname"),
        numericInput("staff", "Staff Number", value = NULL),
        textInput("desig", "Designation"),
        numericInput("mobile", "Mobile Number", value = NULL),
        textInput("station", "Station/Region"),
        textInput("super", "Supervisor"),
        actionButton("submit1", "Submit", class = "btn-primary"),
        tags$br(), tags$br(), tags$br()
      )
    }
  })
  
  output$corporateUI <- renderUI({
    if(!is.null(activeMenu()) && activeMenu() == "corporate") {
      fluidRow(
        textInput("participant", "Name of Participant"),
        numericInput("nationalID", "National ID", value = NULL),
        textInput("company", "Company"),
        numericInput("mobileNo", "Mobile No.", value = NULL),
        actionButton("submit2", "Submit",class = "btn-primary")
      )
    }
  })
  
  observeEvent(input$internal, {
    activeMenu("internal")
  })
  
  observeEvent(input$corporate, {
    activeMenu("corporate")
  })
  
  observeEvent(input$submit1, {
    # Check if all fields are filled
    validate(
      need(input$fname, "Please fill all fields before submitting."),
      need(input$sname, "Please fill all fields before submitting."),
      need(input$lname, "Please fill all fields before submitting."),
      need(input$staff, "Please fill all fields before submitting."),
      need(input$desig, "Please fill all fields before submitting."),
      need(input$mobile, "Please fill all fields before submitting."),
      need(input$station, "Please fill all fields before submitting."),
      need(input$super, "Please fill all fields before submitting.")
    )
    # Get the input values
    data <- data.frame(
      First_Name = input$fname,
      Second_Name = input$sname,
      Surname = input$lname,
      Staff_Number = input$staff,
      Designation = input$desig,
      Mobile_Number = input$mobile,
      Station_Region = input$station,
      Supervisor = input$super
    )
    # Append the input values to the excel sheet
    append_row("internal.xlsx", data)
    # Show a success message
    showNotification("Internal employee data saved successfully", type = "message", id = "message1")
  })
  
  observeEvent(input$submit2, {
    # Check if all fields are filled
    validate(
      need(input$participant, "Please fill all fields before submitting."),
      need(input$nationalID, "Please fill all fields before submitting."),
      need(input$company, "Please fill all fields before submitting."),
      need(input$mobileNo, "Please fill all fields before submitting.")
    )
    # Get the input values
    data <- data.frame(
      Name_of_Participant = input$participant,
      National_ID = input$nationalID,
      Company = input$company,
      Mobile_No = input$mobileNo
    )
    # Append the input values to the excel sheet
    append_row("corporate.xlsx", data)
    # Show a success message
    showNotification("Corporate client data saved successfully", type = "message", id = "message2")
  })
  
  append_row <- function(file, row) {
    if (!file.exists(file)) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", row, startRow = 1, colNames = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    } else {
      wb <- loadWorkbook(file)
      # Read the existing data from the file
      old_data <- read.xlsx(file)
      # Check if the new row is duplicate
      if (any(duplicated(rbind(old_data, row)))) {
        # Show a warning message
        showNotification("Duplicate data entered!", type = "warning", id = "warning3")
      } else {
        # Write the new row to the file
        writeData(wb, "Sheet1", row, startRow = getSheetDims(wb, "Sheet1")[1] + 1, colNames = FALSE)
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  }
}

# Run the app
shinyApp(ui, server)
