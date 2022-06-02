library(igraph)
library(tikzDevice)

graph <- graph_from_adjacency_matrix(
  forestStrLst[[3]],
  mode = "undirected",
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)

tikz(file = "")
plot(graph)
