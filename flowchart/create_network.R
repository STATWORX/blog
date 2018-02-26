
#  settings ---------------------------------------------------------------

library(ggplot2)
library(stringr)

library(GGally)
library(network)
#library(threejs)
#library(htmlwidgets)
library(igraph)
library(sna)
#library(visNetwork)
#library(networkD3)
#library(ndtv)

source("flowchart/getnetwork.R")
# load network ------------------------------------------------------------

dir <- "flowchart/R_network_functions"

net <- getnetwork(dir = dir)
g1 <- net$igraph

# create plots ------------------------------------------------------------
l <- layout_with_fr(g1)
colrs <- rainbow(length(unique(V(g1)$color)))

plot(g1,
     edge.arrow.size=.1,
     #vertex.label=NA,
     edge.width = 5*E(g1)$weight/max(E(g1)$weight),
     vertex.shape="none",
     #vertex.shape="rectangle",
     #vertex.size = (sapply(V(g1)$label, nchar))*2,
     #vertex.size2 = 5,
     #vertex.label=V(g1)$label, 
     #vertex.label.font=2,
     vertex.label.color=colrs[V(g1)$color],
     vertex.label.color="black",
     #vertex.size = V(g1)$size,
     vertex.size = 20,
     #vertex.label.cex=V(g1)$size,
     vertex.color = colrs[V(g1)$color],
     edge.color="steelblue1",
     layout = l)
 legend(x=0,
        unique(V(g1)$folder), pch=21,
        #col="#777777",
        pt.bg= colrs[unique(V(g1)$color)],
        pt.cex=2, cex=.8, bty="n", ncol=1)



# weighted bipartite network
fnet <-network(net$matrix,
               matrix.type = "adjacency",
               ignore.eval = FALSE,
               names.eval = "weights",
               directed = TRUE)

plot.ggnet <- ggnet2(fnet,
  node.size = V(g1)$size,
  color = V(g1)$folder,
  palette = "Blues",
  arrow.size = 12,
  arrow.gap = 0.025,
  label = TRUE,
  edge.label = "weights") +
  guides(size = FALSE)
plot(plot.ggnet)

