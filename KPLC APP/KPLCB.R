library(shiny)
library(shinyjs)
library(googledrive)
library(googlesheets4)

# Authentication options
#options(
  # Use cached token if found
  #gargle_oauth_email = TRUE,
  # Store tokens in a hidden directory
  #gargle_oauth_cache = "C:/Users/Roy Gicheru/Documents/R PROJECTS/Shiny/ShinyPrac/KPLC APP/.secrets"  # Adjust the path if needed
#)

# Get sheet IDs (create if not found)
internal_sheet_id <- drive_get("internal")$id
if (is.null(internal_sheet_id)) {
  internal_sheet_id <- drive_create(title = "internal", mimeType = "application/vnd.google-apps.spreadsheet")$id
}

corporate_sheet_id <- drive_get("corporate")$id
if (is.null(corporate_sheet_id)) {
  corporate_sheet_id <- drive_create(title = "corporate", mimeType = "application/vnd.google-apps.spreadsheet")$id
}


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
             actionButton("internal", "KPLC Internal Employee Training Registration"),
             br(), br(), # Line break after the internal button
             uiOutput("internalUI")
      ),
      column(6,
             actionButton("corporate", "Corporate Employee Training Registration"),
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
        textInput("mobileNo", "Mobile No."),
        textInput("email", "E-mail Address"),
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
      need(input$fname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$sname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$lname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$staff, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$desig, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$mobile, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$station, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$super, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4"))
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
    sheet_append(data, ss = internal_sheet_id)
    # Show a success message
    showNotification("Internal employee data saved successfully", type = "message", id = "message1")
  })
  
  observeEvent(input$submit2, {
    # Check if all fields are filled
    validate(
      need(input$participant, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$nationalID, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$company, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$mobileNo, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
      need(input$email, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4"))
    )
    # Get the input values
    data <- data.frame(
      Name_of_Participant = input$participant,
      National_ID = input$nationalID,
      Company = input$company,
      Mobile_No = input$mobileNo,
      Email = input$email
    )
    # Append the input values to the excel sheet
    sheet_append(data, ss = corporate_sheet_id)
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
            showNotification("Duplicate data entered!", type = "warning", id = "warning3")
          } else {
            sheet_append(row, ss = internal_sheet_id)  # Use sheet_append to write to Google Sheet
          }
      if (any(duplicated(rbind(old_data, row)))) {
        showNotification("Duplicate data entered!", type = "warning", id = "warning3")
      } else {
        sheet_append(row, ss = corporate_sheet_id)  # Use sheet_append to write to Google Sheet
      }
    }
  }
  
  # append_row <- function(sheet_id, row) {
  #   # ... (existing code)
  #   if (any(duplicated(rbind(old_data, row)))) {
  #     showNotification("Duplicate data entered!", type = "warning", id = "warning3")
  #   } else {
  #     sheet_append(row, ss = sheet_id)  # Use sheet_append to write to Google Sheet
  #   }
  # }
}

# Run the app
shinyApp(ui, server)
