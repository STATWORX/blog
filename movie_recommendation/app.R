library(shinyWidgets)
library(DT)
library(tidyverse)
library(dplyr)
library(reshape2)
library(recommenderlab)

source("load_data.R", local = TRUE)$value

ui <- fluidPage(
  sidebarPanel(
    h2("Introduction"),
    h5("This shiny app is a simple example for a movie recommender described in
    a blog post by STATWORX. STATWORX is a consulting company for data science, 
       machine learning and AI based in Frankfurt, Germany."),
    uiOutput("tab"),
    h5("To use this recommender, select up to 10 movies from the dropdown menue 
       and rate them from 0 = bad to 5 = good. When you press the run button a
       movie recommendation based on your rating will be displayed."),
    br(),
    h2("Select and rate movies"),
    pickerInput(inputId = "movie_selection",
                label = "",
                choices = movie_names,
                selected = movie_names[1:2],
                options = pickerOptions(
                  actionsBox = FALSE,
                  maxOptions = 10 # maximum of options
                ), 
                multiple = TRUE),
    h4(" "),
    uiOutput("movie_rating01"),
    uiOutput("movie_rating02"),
    uiOutput("movie_rating03"),
    uiOutput("movie_rating04"),
    uiOutput("movie_rating05"),
    uiOutput("movie_rating06"),
    uiOutput("movie_rating07"),
    uiOutput("movie_rating08"),
    uiOutput("movie_rating09"),
    uiOutput("movie_rating10"),
    actionButton("run", "Run")
  ),
  mainPanel(
    tableOutput("recomm")
  )
)


server <- function(input, output, session) {
  source("ui_server.R", local = TRUE)$value
  source("data_server.R", local = TRUE)$value
  url <- a("here", href="https://www.statworx.com/de/blog/")
  output$tab <- renderUI({
    tagList("You can find this blog post ", url, ".")
  })
}

shinyApp(ui = ui, server = server)
