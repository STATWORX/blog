# Libraries --------------------------------------------------------------

library(igraph)
library(geomnet)


# Data Preparation --------------------------------------------------------

#Load dataset
data(lesmis)

#Edges
edges <- as.data.frame(lesmis[1])
colnames(edges) <- c("from", "to", "weight")

#Create graph for the algorithms
g <- graph_from_data_frame(edges, directed = FALSE)


# Community Detection -----------------------------------------------------

# Louvain
lc <- cluster_louvain(g)
membership(lc)
communities(lc)
plot(lc, g)

# Infomap
imc <- cluster_infomap(g)
membership(imc)
communities(imc)
plot(lc, g)