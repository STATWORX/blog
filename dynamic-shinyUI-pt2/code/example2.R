library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "select", 
                label = "please select an option", 
                choices = LETTERS[1:3]),
    conditionalPanel(
      condition = "input.select == 'B'",
      textAreaInput(inputId = "comment", 
                    label = "please add a comment", 
                    placeholder = "write comment here")
    )
  ),
  dashboardBody(
    uiOutput("selection_text"),
    textOutput("comment_text")
    )
)

server <- function(input, output) {
  
  output$selection_text <- renderUI({
    
    paste("The selected option is", input$select)
  })
  
  output$comment_text <- renderText({
    
    input$comment
  })
}

shinyApp(ui = ui, server = server)