library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  
  dashboardBody(column(width = 4, 
                       fluidRow(valueBoxOutput("ch_1", width = 12)),
                       fluidRow(valueBoxOutput("jp_1", width = 12)),
                       fluidRow(valueBoxOutput("ger_1", width = 12))),
                column(width = 4,
                       fluidRow(valueBoxOutput("ch_2", width = 12)),
                       fluidRow(valueBoxOutput("jp_2", width = 12)),
                       fluidRow(valueBoxOutput("ger_2", width = 12))),
                column(width = 4, 
                       fluidRow(valueBoxOutput("ch_3", width = 12)),
                       fluidRow(valueBoxOutput("jp_3", width = 12)),
                       fluidRow(valueBoxOutput("ger_3", width = 12)))
  )
)

server <- function(input, output) {
  
  output$ch_1 <- renderValueBox({
    valueBox(value = "CH",
             subtitle = "Box 1")
  })
  
  output$ch_2 <- renderValueBox({
    valueBox(value = "CH",
             subtitle = "Box 2")
  })
  
  output$ch_3 <- renderValueBox({
    valueBox(value = "CH",
             subtitle = "Box 3",
             width = 12)
  })
  
  output$jp_1 <- renderValueBox({
    valueBox(value = "JP",
             subtitle = "Box 1",
             width = 12)
  })
  
  output$jp_2 <- renderValueBox({
    valueBox(value = "JP",
             subtitle = "Box 2",
             width = 12)
  })
  
  output$jp_3 <- renderValueBox({
    valueBox(value = "JP",
             subtitle = "Box 3",
             width = 12)
  })
  
  output$ger_1 <- renderValueBox({
    valueBox(value = "GER",
             subtitle = "Box 1",
             width = 12)
  })
  
  output$ger_2 <- renderValueBox({
    valueBox(value = "GER",
             subtitle = "Box 2",
             width = 12)
  })
  
  output$ger_3 <- renderValueBox({
    valueBox(value = "GER",
             subtitle = "Box 3",
             width = 12)
  })
}

shinyApp(ui = ui, server = server)