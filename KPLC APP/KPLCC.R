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
  # Add the download button here
  downloadButton("download", "Download Excel Files", class = "btn-primary")
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
  
  # Add a modal dialog for password input when the download button is clicked
  observeEvent(input$download, {
    showModal(modalDialog(
      title = "Password Required",
      textInput("passwordInput", "Enter Password", value = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submitPassword", "Submit")
      )
    ))
  })
  
  # Handle password submission and download initiation
  observeEvent(input$submitPassword, {
    if (input$passwordInput == "YourPasswordHere") {
      removeModal()
      shinyjs::enable("download")
    } else {
      showNotification("Incorrect password. Download Denied.", type = "error")
      shinyjs::disable("download")
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










#' library(shiny)
#' library(shinyjs)
#' library(googledrive)
#' library(googlesheets4)
#' 
#' # Authentication (non-interactive approach)
#' options(gargle_oauth_cache = TRUE)  # Enable caching of access tokens
#' 
#' # Sheet IDs (assumed to be existing)
#' internal_sheet_id <- "1E7lF4ilMVjFWPbHr0RhxblhR-7nwa9-iD6nnFgULuQs"
#' corporate_sheet_id <- "1bLlG_0ds5Z9-S_HZj_HWvIlkW8pxlxrzaVNH0igW8aI"
#' 
#' # Define the UI
#' ui <- fluidPage(
#'   useShinyjs(),
#'   tags$head(
#'     tags$style(HTML("
#'       .btn-primary {
#'         background-color: #2c4c8b; /* Custom button color */
#'         border: none; /* No border */
#'         color: white; /* White text color */
#'       }
#'       .btn-primary:hover {
#'         background-color: #1a3360; /* Darker button color on hover */
#'       }
#'       #header {
#'         color: white;
#'         background-color: #2c4c8b;
#'         padding: 20px;
#'         display: flex;
#'         align-items: center;
#'         font-size: 40px;
#'       }
#'       #header img {
#'         align-items: left;
#'         margin-right: 10px
#'       }
#'       .button-row {
#'         display: flex;
#'       }
#'       /* Add this part for responsive design */
#'       @media (max-width: 600px) {
#'         .button-row {
#'           flex-direction: column;
#'         }
#'       }
#'     "))
#'   ),
#'   tags$header(
#'     id = "header",
#'     img(src = "https://is3-ssl.mzstatic.com/image/thumb/Purple115/v4/16/95/55/16955554-bfca-ecd4-9ec7-d9c92350df9c/source/512x512bb.jpg",
#'         height = "120px", width = "250px"),
#'     "Training Registration Page"),
#'   titlePanel(" ", windowTitle = "Training Registration"),
#'   
#'   tags$head(
#'     tags$style(HTML("
#'       #internal:hover, #corporate:hover {
#'         background-color: #2c4c8b;
#'         color: white;
#'       }
#'     "))
#'   ),
#'   
#'   div(class = "button-row",
#'       column(6,
#'              actionButton("internal", "KPLC Internal Employee Training Registration"),
#'              br(), br(), # Line break after the internal button
#'              uiOutput("internalUI")
#'       ),
#'       column(6,
#'              actionButton("corporate", "Corporate Employee Training Registration"),
#'              br(), br(), # Line break after the corporate button
#'              uiOutput("corporateUI")
#'       )
#'   )
#' )
#' 
#' # Define the server
#' server <- function(input, output, session) {
#'   activeMenu <- reactiveVal(NULL)
#'   
#'   output$internalUI <- renderUI({
#'     if(!is.null(activeMenu()) && activeMenu() == "internal") {
#'       fluidRow(
#'         textInput("fname", "First Name"),
#'         textInput("sname", "Second Name"),
#'         textInput("lname", "Surname"),
#'         numericInput("staff", "Staff Number", value = NULL),
#'         textInput("desig", "Designation"),
#'         numericInput("mobile", "Mobile Number", value = NULL),
#'         textInput("station", "Station/Region"),
#'         textInput("super", "Supervisor"),
#'         actionButton("submit1", "Submit", class = "btn-primary"),
#'         tags$br(), tags$br(), tags$br()
#'       )
#'     }
#'   })
#'   
#'   output$corporateUI <- renderUI({
#'     if(!is.null(activeMenu()) && activeMenu() == "corporate") {
#'       fluidRow(
#'         textInput("participant", "Name of Participant"),
#'         numericInput("nationalID", "National ID", value = NULL),
#'         textInput("company", "Company"),
#'         textInput("mobileNo", "Mobile No."),
#'         textInput("email", "E-mail Address"),
#'         actionButton("submit2", "Submit",class = "btn-primary")
#'       )
#'     }
#'   })
#'   
#'   observeEvent(input$submit2, {  # For corporate submissions (similar logic)
#'     # Validate input fields
#'     validate(
#'       need(input$participant, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
#'       need(input$nationalID, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
#'       need(input$company, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
#'       need(input$mobileNo, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
#'       need(input$email, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5"))
#'     )
#'     
#'     # Prepare data
#'     data <- data.frame(
#'       Participant_Name = input$participant,
#'       National_ID = input$nationalID,
#'       Company = input$company,
#'       Mobile_Number = input$mobileNo,
#'       Email_Address = input$email
#'     )
#'     
#'     # Send a background task to write data to the sheet
#'     session$sendInput(
#'       "write_data",
#'       {
#'         tryCatch({  # Wrap data writing in tryCatch for error handling
#'           sheet_append(data, ss = corporate_sheet_id)
#'           showNotification("Corporate client data saved successfully", type = "message", id = "message2")
#'         }, error = function(e) {
#'           showNotification("Error saving data: "  , type = "error", id = "error2")
#'           # Consider logging the error for further investigation
#'         })
#'       }
#'     )
#'   })
#'   
#'   # Background task triggered by sendInput (common logic)
#'   observeEvent(input$write_data, {
#'     # Copy the logic from the previous section within observeEvent(input$submit1)
#'     # This assumes the logic involves writing data based on the sheet ID provided
#'     # and displaying a success/error message.
#'     tryCatch({
#'       sheet_append(data, ss = input$write_data$data)  # Use data argument from sendInput
#'       showNotification("Data saved successfully", type = "message")
#'     }, error = function(e) {
#'       showNotification("Error saving data: "  , type = "error")
#'       # Consider logging the error for further investigation
#'     })
#'   })
#'   
#'   # ... rest of the server logic remains the same ...
#' }
#' 
#' # Run the app
#' shinyApp(ui, server)









#' library(shiny)
#' library(openxlsx)
#' library(shinyjs)
#' 
#' # Define the UI
#' ui <- fluidPage(
#'   useShinyjs(),
#'   tags$head(
#'     tags$style(HTML("
#'     .btn-primary {
#'       # background-color: #2c4c8b; /* Custom button color */
#'       border: none; /* No border */
#'       color: white; /* White text color */
#'     }
#'     .btn-primary:hover {
#'       background-color: #1a3360; /* Darker button color on hover */
#'     }
#'        #header {
#'       color: white;
#'       background-color: #2c4c8b;
#'       padding: 20px;
#'       display: flex;
#'       align-items: center;
#'       font-size: 40px;
#'     }
#'     #header img {
#'       align-items: left;
#'       margin-right: 10px
#'     }
#'     .button-row {
#'       display: flex;
#'     }
#'       /* Add this part for responsive design */
#'       @media (max-width: 600px) {
#'         .button-row {
#'           flex-direction: column;
#'         }
#'       }
#'     "))
#'   ),
#'   tags$header(
#'     id = "header",
#'     img(src = "https://is3-ssl.mzstatic.com/image/thumb/Purple115/v4/16/95/55/16955554-bfca-ecd4-9ec7-d9c92350df9c/source/512x512bb.jpg",
#'         height = "120px", width = "250px"),
#'     "Training Registration Page"),
#'   titlePanel(" ", windowTitle = "Training Registration"),
#' 
#'   tags$head(
#'     tags$style(HTML("
#'       #internal:hover, #corporate:hover {
#'         background-color: #2c4c8b;
#'         color: white;
#'       }
#'     "))
#'   ),
#' 
#'   div(class = "button-row",
#'       column(6,
#'              actionButton("internal", "KPLC Internal Employee Training Registration"),
#'              br(), br(), # Line break after the internal button
#'              uiOutput("internalUI")
#'       ),
#'       column(6,
#'              actionButton("corporate", "Corporate Employee Training Registration"),
#'              br(), br(), # Line break after the corporate button
#'              uiOutput("corporateUI")
#'       )
#'   )
#' )
#' 
#' # Define the server
#' server <- function(input, output, session) {
#'   activeMenu <- reactiveVal(NULL)
#' 
#'   output$internalUI <- renderUI({
#'     if(!is.null(activeMenu()) && activeMenu() == "internal") {
#'       fluidRow(
#'         textInput("fname", "First Name"),
#'         textInput("sname", "Second Name"),
#'         textInput("lname", "Surname"),
#'         numericInput("staff", "Staff Number", value = NULL),
#'         textInput("desig", "Designation"),
#'         numericInput("mobile", "Mobile Number", value = NULL),
#'         textInput("station", "Station/Region"),
#'         textInput("super", "Supervisor"),
#'         actionButton("submit1", "Submit", class = "btn-primary"),
#'         tags$br(), tags$br(), tags$br()
#'       )
#'     }
#'   })
#' 
#'   output$corporateUI <- renderUI({
#'     if(!is.null(activeMenu()) && activeMenu() == "corporate") {
#'       fluidRow(
#'         textInput("participant", "Name of Participant"),
#'         numericInput("nationalID", "National ID", value = NULL),
#'         textInput("company", "Company"),
#'         textInput("mobileNo", "Mobile No."),
#'         textInput("email", "E-mail Address"),
#'         actionButton("submit2", "Submit",class = "btn-primary")
#'       )
#'     }
#'   })
#' 
#'   observeEvent(input$internal, {
#'     activeMenu("internal")
#'   })
#' 
#'   observeEvent(input$corporate, {
#'     activeMenu("corporate")
#'   })
#' 
#'   observeEvent(input$submit1, {
#'     # Check if all fields are filled
#'     validate(
#'       need(input$fname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$sname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$lname, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$staff, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$desig, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$mobile, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$station, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$super, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4"))
#'     )
#'     # Get the input values
#'     data <- data.frame(
#'       First_Name = input$fname,
#'       Second_Name = input$sname,
#'       Surname = input$lname,
#'       Staff_Number = input$staff,
#'       Designation = input$desig,
#'       Mobile_Number = input$mobile,
#'       Station_Region = input$station,
#'       Supervisor = input$super
#'     )
#'     # Append the input values to the excel sheet
#'     append_row("internal.xlsx", data)
#'     # Show a success message
#'     showNotification("Internal employee data saved successfully", type = "message", id = "message1")
#'   })
#' 
#'   observeEvent(input$submit2, {
#'     # Check if all fields are filled
#'     validate(
#'       need(input$participant, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$nationalID, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$company, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$mobileNo, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4")),
#'       need(input$email, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning4"))
#'     )
#'     # Get the input values
#'     data <- data.frame(
#'       Name_of_Participant = input$participant,
#'       National_ID = input$nationalID,
#'       Company = input$company,
#'       Mobile_No = input$mobileNo,
#'       Email = input$email
#'     )
#'     # Append the input values to the excel sheet
#'     append_row("corporate.xlsx", data)
#'     # Show a success message
#'     showNotification("Corporate client data saved successfully", type = "message", id = "message2")
#'   })
#' 
#'   append_row <- function(file, row) {
#'     if (!file.exists(file)) {
#'       wb <- createWorkbook()
#'       addWorksheet(wb, "Sheet1")
#'       writeData(wb, "Sheet1", row, startRow = 1, colNames = TRUE)
#'       saveWorkbook(wb, file, overwrite = TRUE)
#'     } else {
#'       wb <- loadWorkbook(file)
#'       # Read the existing data from the file
#'       old_data <- read.xlsx(file)
#'       # Check if the new row is duplicate
#'       if (any(duplicated(rbind(old_data, row)))) {
#'         # Show a warning message
#'         showNotification("Duplicate data entered!", type = "warning", id = "warning3")
#'       } else {
#'         # Write the new row to the file
#'         writeData(wb, "Sheet1", row, startRow = getSheetDims(wb, "Sheet1")[1] + 1, colNames = FALSE)
#'         saveWorkbook(wb, file, overwrite = TRUE)
#'       }
#'     }
#'   }
#' }
#' 
#' # Run the app
#' shinyApp(ui, server)
