library(shiny)
library(openxlsx)

ui <- fluidPage(
  textInput("Name_of_Training", "Name of Training"),
  textInput("Company", "Company"),
  textInput("Training_Dates", "Training Dates"),
  textInput("Cost_of_Training", "Cost of Training (Kshs)"),
  numericInput("Total_Number_of_Trainees", "Total number of trainees", value = NULL),
  textInput("SheetName", "Name of the Sheet", value = "June2023"), # New input for sheet name
  actionButton("submit", "Submit"),
  downloadButton("download", "Download excel file")
)

server <- function(input, output) {
  # create a reactive data frame to store the inputs
  data <- reactiveVal(data.frame(
    Name_of_Training = character(),
    Company = character(),
    Training_Dates = character(),
    Cost_of_Training = character(),
    Total_Number_of_Trainees = numeric()
    ))

  # update the data frame when submit button is clicked
  observeEvent(input$submit, {
    data(rbind(data(), data.frame(
      Name_of_Training = input$Name_of_Training,
      Company = input$Company,
      Training_Dates = input$Training_Dates,
      Total_Number_of_Trainees = input$Total_Number_of_Trainees
      )))
  })

  # create a workbook object to write the data to excel file
  wb <- createWorkbook()
  addWorksheet(wb, "June2023")

  # download the excel file when download button is clicked
  output$download <- downloadHandler(
    filename = "stuff.xlsx",
    content = function(file) {
      writeData(wb, sheet = "June2023", x = data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)









# library(shiny)
# library(openxlsx)
# 
# ui <- fluidPage(
#   titlePanel("Training Session Data Entry"),
#   sidebarLayout(
#     sidebarPanel(
#       textInput("Name_of_Training", "Name of Training"),
#       textInput("Company", "Company"),
#       textInput("Training_Dates", "Training Dates"),
#       textInput("Cost_of_Training", "Cost of Training (Kshs)"),
#       numericInput("Total_Number_of_Trainees", "Total number of trainees", value = NULL),
#       textInput("SheetName", "Name of the Sheet", value = "June2023"), # New input for sheet name
#       actionButton("submit", "Submit"),
#       downloadButton("download", "Download Excel File")
#     ),
#     mainPanel(
#       # This can be used to display outputs or messages
#     )
#   )
# )
# 
# server <- function(input, output) {
#   # create a reactive data frame to store the inputs
#   data <- reactiveVal(data.frame(
#     Name_of_Training = character(),
#     Company = character(),
#     Training_Dates = character(),
#     Cost_of_Training = character(),
#     Total_Number_of_Trainees = numeric()
#   ))
#   
#   # update the data frame when submit button is clicked
#   observeEvent(input$submit, {
#     data(rbind(data(), data.frame(
#       Name_of_Training = input$Name_of_Training,
#       Company = input$Company,
#       Training_Dates = input$Training_Dates,
#       Cost_of_Training = input$Cost_of_Training,
#       Total_Number_of_Trainees = input$Total_Number_of_Trainees
#     )))
#   })
#   
#   # download the excel file when download button is clicked
#   output$download <- downloadHandler(
#     filename = function() {
#       paste0(input$SheetName, ".xlsx")
#     },
#     content = function(file) {
#       wb <- createWorkbook()
#       addWorksheet(wb, input$SheetName)
#       writeData(wb, sheet = input$SheetName, x = data())
#       saveWorkbook(wb, file, overwrite = TRUE)
#     }
#   )
# }
# 
# shinyApp(ui, server)







# library(shiny)
# library(openxlsx)
# 
# ui <- fluidPage(
#   textInput("Name_of_Training", "Name of Training"),
#   textInput("Company", "Company"),
#   textInput("Training_Dates", "Training Dates"),
#   textInput("Cost_of_Training", "Cost of Training (Kshs)"),
#   numericInput("Total_Number_of_Trainees", "Total number of trainees", value = NULL),
#   textInput("SheetName", "Name of the Sheet", value = "June2023"), # New input for sheet name
#   actionButton("submit", "Submit"),
#   downloadButton("download", "Download excel file")
# )
# 
# server <- function(input, output) {
#   # create a reactive data frame to store the inputs
#   data <- reactiveVal(data.frame(
#     Name_of_Training = character(),
#     Company = character(),
#     Training_Dates = character(),
#     Cost_of_Training = character(),
#     Total_Number_of_Trainees = numeric()
#     ))
# 
#   # update the data frame when submit button is clicked
#   observeEvent(input$submit, {
#     data(rbind(data(), data.frame(
#       Name_of_Training = input$Name_of_Training,
#       Company = input$Company,
#       Training_Dates = input$Training_Dates,
#       Total_Number_of_Trainees = input$Total_Number_of_Trainees
#       )))
#   })
# 
#   # create a workbook object to write the data to excel file
#   wb <- createWorkbook()
#   addWorksheet(wb, "June2023")
# 
#   # download the excel file when download button is clicked
#   output$download <- downloadHandler(
#     filename = "stuff.xlsx",
#     content = function(file) {
#       writeData(wb, sheet = "June2023", x = data())
#       saveWorkbook(wb, file, overwrite = TRUE)
#     }
#   )
# }
# 
# shinyApp(ui, server)
