library(igraph)
library(tidyverse)
library(Matrix)

actr_actr_graph <- make_graph(actr_edges_vctr, directed = FALSE)
E(actr_actr_graph)$weight <- 1
actr_actr_graph <- igraph::simplify(actr_actr_graph, edge.attr.comb = list(weight="sum"))

components(actr_actr_graph)$no
components(actr_actr_graph)$csize


actr_actr_pruned <- igraph::delete_edges(actr_actr_graph, which(E(actr_actr_graph)$weight <= 10))

ecount(actr_actr_graph)
ecount(actr_actr_pruned)

comps <- components(actr_actr_pruned)


which(components(actr_actr_pruned)$csize>2)

big_clump <- lapply(seq_along(comps$csize)[comps$csize > 3], function(x) 
  V(actr_actr_pruned)$name[comps$membership %in% x])[[1]]

big_clump

actors$Label[actors$Id %in% big_clump]

movies_of_big_clumpers <- list()
for (i in 1:length(big_clump)){
  movies_of_big_clumpers[[i]] <- cast_edges$Source[cast_edges$Target == big_clump[i]]
}

movies_of_big_clumpers

# use below for intersect
# big_clump_movies <- movies_of_big_clumpers[[1]]
# for (i in 2:length(movies_of_big_clumpers)){
#   big_clump_movies <- intersect(movies_of_big_clumpers[[i]], big_clump_movies)
# }


big_clump_movies <- unique(unlist(movies_of_big_clumpers))



big_clump_movies # there's no movie they all star in!

movies$Label[movies$Id %in% big_clump_movies]



################################################ clustering vvv

actr_communities <- cluster_louvain(actr_actr_graph)

c(TRUE,TRUE)&c(TRUE,FALSE)


length(actr_communities)
sizes(actr_communities)
hist(sizes(actr_communities))
# vcount(actr_actr_graph)
# ecount(actr_actr_graph)
# 
# plot(actr_actr_graph)
# 
testg <- make_graph(c("a","b","b","c","c","a","a","b","d","e"), directed = FALSE)
E(testg)$weight <- 1
testg <- igraph::simplify(testg, edge.attr.comb = list(weight="sum"))
V(testg)$name
igraph::components(testg)
