library(shiny)
library(openxlsx)

ui <- fluidPage(
  textInput("name", "Enter your name"),
  numericInput("age", "Enter your age", value = 0, min = 0, max = 100),
  actionButton("submit", "Submit"),
  downloadButton("download", "Download excel file")
)

server <- function(input, output) {
  # create a reactive data frame to store the inputs
  data <- reactiveVal(data.frame(name = character(), age = numeric()))
  
  # update the data frame when submit button is clicked
  observeEvent(input$submit, {
    data(rbind(data(), data.frame(name = input$name, age = input$age)))
  })
  
  # create a workbook object to write the data to excel file
  wb <- createWorkbook()
  addWorksheet(wb, "stuff")
  
  # download the excel file when download button is clicked
  output$download <- downloadHandler(
    filename = "stuff.xlsx",
    content = function(file) {
      writeData(wb, sheet = "stuff", x = data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
