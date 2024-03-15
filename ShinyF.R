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
  # Added download buttons
  downloadButton("download1", "Download Internal Data", class = "btn-primary"),
  downloadButton("download2", "Download Corporate Data", class = "btn-primary")
)

# Define the server
server <- function(input, output, session) {
  activeMenu <- reactiveVal(NULL)
  
  # Reactive values to store the input data for each registration type
  internalData <- reactiveVal(data.frame(First_Name = character(),
                                         Second_Name = character(),
                                         Surname = character(),
                                         Staff_Number = numeric(),
                                         Designation = character(),
                                         Mobile_Number = numeric(),
                                         Station_Region = character(),
                                         Supervisor = character()))
  
  corporateData <- reactiveVal(data.frame(Name_of_Participant = character(),
                                          National_ID = numeric(),
                                          Company = character(),
                                          Mobile_No = numeric(),
                                          Email = character()))
  
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
        actionButton("submit2", "Submit", class = "btn-primary")
      )
    }
  })
  
  observeEvent(input$internal, {
    activeMenu("internal")
  })
  
  observeEvent(input$corporate, {
    activeMenu("corporate")
  })
  
  # Function to check if all fields are filled for Internal registration
  isInternalFilled <- function() {
    all(c(input$fname != "", input$sname != "", input$lname != "", 
          !is.na(input$staff), input$desig != "", !is.na(input$mobile),
          input$station != "", input$super != ""))
  }
  
  # Function to check if all fields are filled for Corporate registration
  isCorporateFilled <- function() {
    all(c(input$participant != "", !is.na(input$nationalID), 
          input$company != "", !is.na(input$mobileNo), input$email != ""))
  }
  
  # Observe clicks on submit buttons
  observeEvent(input$submit1, {
    if (isInternalFilled()) {
      # Create a data frame with the correct structure
      new_entry <- data.frame(
        First_Name = input$fname,
        Second_Name = input$sname,
        Surname = input$lname,
        Staff_Number = input$staff,
        Designation = input$desig,
        Mobile_Number = input$mobile,
        Station_Region = input$station,
        Supervisor = input$super
      )
      # Update internal data with new entry
      internalData(rbind(internalData(), new_entry))
      # Clear input fields for next submission
      updateTextInput(session, "fname", value = "")
      updateTextInput(session, "sname", value = "")
      updateTextInput(session, "lname", value = "")
      updateNumericInput(session, "staff", value = NULL)
      updateTextInput(session, "desig", value = "")
      updateNumericInput(session, "mobile", value = NULL)
      updateTextInput(session, "station", value = "")
      updateTextInput(session, "super", value = "")
    } else {
      # Show error message if all fields are not filled
      showModal(
        modalDialog(
          title = "Error",
          "Please fill all fields for Internal Registration"
        )
      )
    }
  })
  
  observeEvent(input$submit2, {
    if (isCorporateFilled()) {
      # Create a data frame with the correct structure
      new_entry <- data.frame(
        Name_of_Participant = input$participant,
        National_ID = input$nationalID,
        Company = input$company,
        Mobile_No = input$mobileNo,
        Email = input$email
      )
      # Update corporate data with new entry
      corporateData(rbind(corporateData(), new_entry))
      # Clear input fields for next submission
      updateTextInput(session, "participant", value = "")
      updateNumericInput(session, "nationalID", value = NULL)
      updateTextInput(session, "company", value = "")
      updateNumericInput(session, "mobileNo", value = NULL)
      updateTextInput(session, "email", value = "")
    } else {
      # Show error message if all fields are not filled
      showModal(
        modalDialog(
          title = "Error",
          "Please fill all fields for Corporate Registration"
        )
      )
    }
  })
  
  
  # Download buttons functionality
  output$download1 <- downloadHandler(
    filename = function() { "Internal_Data.xlsx" },
    content = function(file) {
      write.xlsx(internalData(), file, sheetName = "Internal Employees", append = FALSE)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() { "Corporate_Data.xlsx" },
    content = function(file) {
      write.xlsx(corporateData(), file, sheetName = "Corporate Employees", append = FALSE)
    }
  )
}

shinyApp(ui, server)







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
#'       # Create a new workbook and add the row with column names
#'       wb <- createWorkbook()
#'       addWorksheet(wb, "Sheet1")
#'       writeData(wb, "Sheet1", row, startRow = 1, colNames = TRUE)
#'       saveWorkbook(wb, file, overwrite = TRUE)
#'     } else {
#'       wb <- loadWorkbook(file)
#'       # Retrieve the last row number in the existing data
#'       last_row <- nrow(readWorkbook(wb, "Sheet1")) + 1
#'       
#'       # Check for duplicates (optional, can be removed if not needed)
#'       existing_data <- readWorkbook(wb, "Sheet1")
#'       if (any(duplicated(rbind(existing_data, row)))) {
#'         showNotification("Duplicate data entered!", type = "warning", id = "warning3")
#'       } else {
#'         # Write the new row to the next empty row (append)
#'         writeData(wb, sheet = "Sheet1", x = row, startRow = last_row, colNames = FALSE)
#'         saveWorkbook(wb, file, overwrite = TRUE)
#'       }
#'     }
#'   }
#'   
#' }
#' 
#' # Run the app
#' shinyApp(ui, server)
