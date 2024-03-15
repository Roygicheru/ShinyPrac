library(shiny)
library(openxlsx)
library(shinyjs)
library(writexl)
library(readxl)

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .btn-primary {
      # background-color: #2c4c8b; /* Custom button color */
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
  ),
  
  # Add the download access button here
  actionButton("downloadAccess", "Download Access", class = "btn-primary"),
  
  # Add the download button here
  downloadButton("download", "Download Excel Files", class = "btn-primary", style = "display: none;") # Initially hidden
  
)

# Define the server
server <- function(input, output, session) {
  activeMenu <- reactiveVal(NULL)
  submissionMade <- reactiveVal(FALSE) # Track if submission is made
  
  output$internalUI <- renderUI({
    if(!is.null(activeMenu()) && activeMenu() == "internal") {
      fluidRow(
        textInput("fname", "First Name"),
        textInput("sname", "Second Name"),
        textInput("lname", "Surname"),
        numericInput("staff", "Staff Number", value = NULL),
        textInput("desig", "Designation"),
        textInput("mobile", "Mobile Number"),
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
        textInput("mobileNo", "Mobile Number"),
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
  
  # Observe the submit button for internal
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
    # Append the input values to the Excel sheet
    append_data_to_excel("internal.xlsx", data)
    # Show a success message
    showNotification("Internal employee data saved successfully", type = "message", id = "message1")
    
    submissionMade(TRUE) # Set the flag to TRUE after submission
  })
  
  # Observe the submit button for corporate
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
    # Append the input values to the Excel sheet
    append_data_to_excel("corporate.xlsx", data)
    # Show a success message
    showNotification("Corporate client data saved successfully", type = "message", id = "message2")
    
    submissionMade(TRUE) # Set the flag to TRUE after submission
  })
  
  # Add the modal dialog for password input
  observeEvent(input$downloadAccess, {
    showModal(modalDialog(
      id = "passwordModal",
      passwordInput("password", "Enter Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("passwordSubmit", "Submit", class = "btn-primary")
      )
    ))
  })
  
  # Define the password check and access grant
  correctPassword <- "TrainingRegistration@IESR" # Replace with your actual password
  
  observeEvent(input$passwordSubmit, {
    if (input$password == correctPassword) {
      removeModal()
      shinyjs::show("download")
    } else {
      showNotification("Incorrect password. Please try again.", type = "error")
    }
  })
  
  # Define the download handler for the download button
  output$download <- downloadHandler(
    filename = function() {
      paste("training-data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      # Create a temporary directory to store the Excel files
      tempDir <- tempdir()
      internalFilePath <- file.path(tempDir, "internal.xlsx")
      corporateFilePath <- file.path(tempDir, "corporate.xlsx")
      
      # Check if the files exist before attempting to copy
      if (file.exists("internal.xlsx")) {
        file.copy("internal.xlsx", internalFilePath)
      } else {
        stop("Internal file not found.")
      }
      
      if (file.exists("corporate.xlsx")) {
        file.copy("corporate.xlsx", corporateFilePath)
      } else {
        stop("Corporate file not found.")
      }
      
      # Zip the Excel files
      zip(zipfile = file, files = c(internalFilePath, corporateFilePath))
    }
  )
}

# Function to append data to an existing Excel file
append_data_to_excel <- function(file_path, new_data) {
  if (file.exists(file_path)) {
    # Load the existing workbook
    wb <- loadWorkbook(file_path)
    # Read the existing data from the first sheet
    existing_data <- read.xlsx(file_path, sheet = 1)
    # Combine the existing data with the new data
    combined_data <- rbind(existing_data, new_data)
    # Write the combined data back to the first sheet
    writeData(wb, sheet = 1, x = combined_data)
    # Save the workbook
    saveWorkbook(wb, file_path, overwrite = TRUE)
  } else {
    # Show an error notification if the file does not exist
    showNotification(paste("File", file_path, "not found. Please create the file first."), type = "error", id = "error1")
  }
}

# Run the app
shinyApp(ui, server)