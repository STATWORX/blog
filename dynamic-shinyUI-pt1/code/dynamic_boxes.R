library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    selectizeInput(
      inputId = "select",
      label = "Select countries:",
      choices = c("CH", "JP", "GER"),
      multiple = TRUE)
  ),
  
  dashboardBody(column(2, uiOutput("ui1"), offset = 6),
                column(2, uiOutput("ui2")),
                column(2, uiOutput("ui3")))
  )

server <- function(input, output) {
  
  output$ui1 <- renderUI({
    req(input$select)
    
    lapply(seq_along(input$select), function(i) {
      fluidRow(
        valueBox(value = input$select[i],
               subtitle = "Box 1",
               width = 12)
        )
    })
  })
  
  output$ui2 <- renderUI({
    req(input$select)
    
    lapply(seq_along(input$select), function(i) {
      fluidRow(
        valueBox(value = input$select[i],
               subtitle = "Box 2",
               width = 12)
      )
    })
  })
  
  output$ui3 <- renderUI({
    req(input$select)
    
    lapply(seq_along(input$select), function(i) {
      fluidRow(
        valueBox(value = input$select[i],
               subtitle = "Box 3",
               width = 12)
      )
    })
  })
}

shinyApp(ui = ui, server = server)