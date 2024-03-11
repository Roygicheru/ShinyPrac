library(shiny)
library(shinyjs)
library(googledrive)
library(googlesheets4)

# Authentication (non-interactive approach)
options(gargle_oauth_cache = TRUE)  # Enable caching of access tokens

# Sheet IDs (assumed to be existing)
internal_sheet_id <- "1E7lF4ilMVjFWPbHr0RhxblhR-7nwa9-iD6nnFgULuQs"
corporate_sheet_id <- "1bLlG_0ds5Z9-S_HZj_HWvIlkW8pxlxrzaVNH0igW8aI"

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
  
  observeEvent(input$submit2, {  # For corporate submissions (similar logic)
    # Validate input fields
    validate(
      need(input$participant, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
      need(input$nationalID, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
      need(input$company, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
      need(input$mobileNo, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5")),
      need(input$email, showNotification("Please fill all fields before submitting.", type = "warning", id = "warning5"))
    )
    
    # Prepare data
    data <- data.frame(
      Participant_Name = input$participant,
      National_ID = input$nationalID,
      Company = input$company,
      Mobile_Number = input$mobileNo,
      Email_Address = input$email
    )
    
    # Send a background task to write data to the sheet
    session$sendInput(
      "write_data",
      {
        tryCatch({  # Wrap data writing in tryCatch for error handling
          sheet_append(data, ss = corporate_sheet_id)
          showNotification("Corporate client data saved successfully", type = "message", id = "message2")
        }, error = function(e) {
          showNotification("Error saving data: "  , type = "error", id = "error2")
          # Consider logging the error for further investigation
        })
      }
    )
  })
  
  # Background task triggered by sendInput (common logic)
  observeEvent(input$write_data, {
    # Copy the logic from the previous section within observeEvent(input$submit1)
    # This assumes the logic involves writing data based on the sheet ID provided
    # and displaying a success/error message.
    tryCatch({
      sheet_append(data, ss = input$write_data$data)  # Use data argument from sendInput
      showNotification("Data saved successfully", type = "message")
    }, error = function(e) {
      showNotification("Error saving data: "  , type = "error")
      # Consider logging the error for further investigation
    })
  })
  
  # ... rest of the server logic remains the same ...
}

# Run the app
shinyApp(ui, server)