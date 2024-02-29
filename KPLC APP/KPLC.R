library(shiny)
library(openxlsx)
library(shinyjs)

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #header {
        color: white;
        background-color: #00008B;
        padding: 20px;
        text-align: center;
        font-size: 24px;
      }
      #header img {
        float: left;
      }
      .button-row {
        display: flex;
        justify-content: space-between;
      }
    "))
  ),
  tags$header(
    id = "header",
    img(src = "https://th.bing.com/th/id/OIP.5m8zEp8lfuvVsi8kJPwU0QHaF5?w=213&h=180&c=7&r=0&o=5&dpr=1.3&pid=1.7", height = "50px"), # Replace with your image path
    "Training Registration Page"
  ),
  titlePanel(" ", windowTitle = "Training Registration"),
  
  tags$head(
    tags$style(HTML("
      #internal:hover, #corporate:hover {
        background-color: #00008B;
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
        actionButton("submit1", "Submit", class = "btn-primary")
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
  
  observeEvent(input$submit1, {
    req(input$fname, input$sname, input$lname, input$staff, input$desig, input$mobile, input$station, input$super)
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
    append_row("internal.xlsx", data)
    showNotification("Internal employee data saved successfully", type = "message")
  })
  
  observeEvent(input$submit2, {
    req(input$participant, input$nationalID, input$company, input$mobileNo)
    data <- data.frame(
      Name_of_Participant = input$participant,
      National_ID = input$nationalID,
      Company = input$company,
      Mobile_No = input$mobileNo
    )
    append_row("corporate.xlsx", data)
    showNotification("Corporate client data saved successfully", type = "message")
  })
  
  append_row <- function(file, row) {
    if (!file.exists(file)) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", row, startRow = 1, colNames = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    } else {
      wb <- loadWorkbook(file)
      writeData(wb, "Sheet1", row, startRow = getSheetDims(wb, "Sheet1")[1] + 1, colNames = FALSE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  }
}

# Run the app
shinyApp(ui, server)
