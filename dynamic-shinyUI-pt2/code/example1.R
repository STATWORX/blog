library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "select", 
                label = "please select an option", 
                choices = LETTERS[1:3]),
    uiOutput("conditional_comment")
  ),
  dashboardBody(
    uiOutput("selection_text"),
    uiOutput("comment_text")
  )
)

server <- function(input, output) {
  
  output$selection_text <- renderUI({
    
    paste("The selected option is", input$select)
  })
  
  output$conditional_comment <- renderUI({
    req(input$select == "B")
    
    textAreaInput(inputId = "comment", 
                  label = "please add a comment", 
                  placeholder = "write comment here")
    
  })
  
  output$comment_text <- renderText({
    req(input$select == "B")
    
    input$comment
  })
  
}

shinyApp(ui = ui, server = server)