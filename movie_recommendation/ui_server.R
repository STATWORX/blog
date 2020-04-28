output$movie_rating01 <- renderUI({
  
  if(length(input$movie_selection) > 0){

    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[1],
                           label = input$movie_selection[1],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating02 <- renderUI({
  
  if(length(input$movie_selection) > 1){

    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[2],
                           label = input$movie_selection[2],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating03 <- renderUI({
  
  if(length(input$movie_selection) > 2){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[3],
                           label = input$movie_selection[3],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating04 <- renderUI({
  
  if(length(input$movie_selection) > 3){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[4],
                           label = input$movie_selection[4],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating05 <- renderUI({
  
  if(length(input$movie_selection) > 4){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[5],
                           label = input$movie_selection[5],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})


output$movie_rating06 <- renderUI({
  
  if(length(input$movie_selection) > 5){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[6],
                           label = input$movie_selection[6],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})


output$movie_rating07 <- renderUI({
  
  if(length(input$movie_selection) > 6){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[7],
                           label = input$movie_selection[7],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating08 <- renderUI({
  
  if(length(input$movie_selection) > 7){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[8],
                           label = input$movie_selection[8],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})


output$movie_rating09 <- renderUI({
  
  if(length(input$movie_selection) > 8){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[9],
                           label = input$movie_selection[9],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})

output$movie_rating10 <- renderUI({
  
  if(length(input$movie_selection) > 9){
    
    fluidRow(
      column(
        shinyBS::popify(
          el = sliderInput(inputId = input$movie_selection[10],
                           label = input$movie_selection[10],
                           max = 5,
                           min = 0,
                           step = .5,
                           value = 3),
          title = "",
          placement = "right"
        ),
        width = 10
      )
    )
    
  }
})