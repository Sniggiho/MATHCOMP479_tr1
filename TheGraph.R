library(igraph)
library(tidyverse)
library(Matrix)

actr_actr_graph <- make_graph(actr_edges_vctr, directed = FALSE)
E(actr_actr_graph)$weight <- 1
actr_actr_graph <- igraph::simplify(actr_actr_graph, edge.attr.comb = list(weight="sum"))

actr_communities <- cluster_louvain(actr_actr_graph)

length(actr_communities)
sizes(actr_communities)

# vcount(actr_actr_graph)
# ecount(actr_actr_graph)
# 
# plot(actr_actr_graph)
# 
# testg <- make_graph(c("a","b","b","c","c","a","a","b","d","e"), directed = FALSE)
# E(testg)$weight <- 1
# testg <- igraph::simplify(testg, edge.attr.comb = list(weight="sum"))
# plot(testg)
# E(testg)$weight
# communutiesTest <- cluster_louvain(testg)
# communutiesTest
