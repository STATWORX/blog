shinyServer(function(input, output) {
    
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
})
