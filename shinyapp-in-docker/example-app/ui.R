shinyUI(
    dashboardPage(
        dashboardHeader(title = "Example ShinyApp"),
        dashboardSidebar(
            sliderInput(inputId = "slider", 
                        label = "dynamically change the number of tabs by using the slider", 
                        min = 1, max = 5, value = 3, step = 1)
    ),
    
    dashboardBody(
        fluidRow(
            box(width = 12,
                p(
                    mainPanel(width = 12,
                            column(width = 6, uiOutput("reference")),
                            column(width = 6, uiOutput("comparison"))
                            )
                    )
                )
            )
        )
    )
)