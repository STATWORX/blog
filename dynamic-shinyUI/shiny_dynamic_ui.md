# Dynamic UI Elements in Shiny

At STATWORX we regularly deploy our project results with the help of Shiny. It is not only an easy way of letting potential users interact with your R-code, it's also fun to design a good-looking app. 

One of the biggest strengths of Shiny is it's inherent reactivity, after all being reactive to user input is a web-applications prime purpose. It is unfortunate that many apps seem to only make use of Shiny's reactivity on the server side while keeping the UI completely static. This does not have to be necessarily bad - some apps would not profit from having dynamic UI elements and adding them regardless could quickly result in the app feeling gimmicky. But in many cases adding reactivity to the UI can not only result in less clutter on the screen, but also cleaner code. And we all like that, don't we? 

### The Tools

Shiny natively provides convenient tools to turn the UI of any app reactive to input. We are namely going to look at the `renderUI` (in conjunction with `lapply`) and `conditionalPanel` functions. 

`renderUI` is helpful because it frees us from the chains of having to define what kind of object we'd like to render in our `render`-function. `renderUI` can render any UI element, so we could, for example, let the type of the content of our `uiOutput` be reactive to an input instead of being set in stone.

`conditionalPanel` simply lets us conditionally show or hide UI elements and thus helps us reduce visual clutter. 

### `renderUI`

Imagine a situation where you are tasked with building a dashboard showing the user three different KPIs for three different countries. The most obvious approach would be to specify the position of each KPI-box on the UI side of the app and creating each element on the server side with the help of `shinydashboard::renderValueBox()` as seen in the example below. 

```R
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
```

This might be a working solution to the task at hand, but it is hardly an elegant one. The valueboxes take up a large amount of space in our app and even though they can be resized or moved around, we always have to look at all the boxes, regardless of which ones are currently of interest. This is of course not optimal. A much more elegant solution would be to only show the boxes for each unit of interest (in our case countries) as chosen by the user. 