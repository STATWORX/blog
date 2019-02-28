shinyServer(function(input, output) {
  output$network <- renderVisNetwork({
    load("nodes.RData")
    load("edges.RData")

    visNetwork(nodes, edges) %>%
      visIgraphLayout()
  })
})
