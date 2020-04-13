library(shiny)
library(shinydashboard)
library(glue)

ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    sliderInput(inputId = "slider", label = NULL, min = 1, max = 5, value = 3, step = 1)
  ),
  
  dashboardBody(
    fluidRow(
      box(width = 12,
          p(mainPanel(width = 12,
                      column(width = 6, uiOutput("reference")),
                      column(width = 6, uiOutput("comparison"))
                      )
            )
      )
    )
  )
)

server <- function(input, output) {
  
  output$reference <- renderUI({
    tabsetPanel(
      tabPanel(
        "Reference",
        h3("Reference Content"))
    )
  })
  
  output$comparison <- renderUI({
    req(input$slider)
    
    myTabs <- lapply(1:input$slider, function(i) {
      
      tabPanel(title = glue("Tab {i}"),
               h3(glue("Content {i}"))
      )
    })
    do.call(tabsetPanel, myTabs)
  })
}

shinyApp(ui = ui, server = server)