library(igraph)
library(tidyverse)
library(Matrix)
library(ggformula)

## This makes the graph, then turns multiedges into weighted edges
actr_actr_graph <- make_graph(actr_edges_vctr, directed = FALSE)
E(actr_actr_graph)$weight <- 1
actr_actr_graph <- igraph::simplify(actr_actr_graph, edge.attr.comb = list(weight="sum"))



########### Compare the relative number of components at each minimum edge weight

min_weight <- 1
pruned_graph <- actr_actr_graph  
num_sig_comps <- length(which(components(pruned_graph)$csize > 1)) # the number of non-isolated components
num_sig_comps_hist <- list(num_sig_comps)

while (num_sig_comps > 1) {
  pruned_graph <- igraph::delete_edges(pruned_graph, which(E(pruned_graph)$weight < min_weight))
  num_sig_comps <- length(which(components(pruned_graph)$csize > 1))
  num_sig_comps_hist[min_weight] <- num_sig_comps
  min_weight <- min_weight + 1
}

gf_point(unlist(num_sig_comps_hist)~1:13, xlab="Minimum Edge Weight", ylab="Number of Non-singleton Components")
gf_point(log(unlist(num_sig_comps_hist))~log(1:13),xlab="Log of Minimum Edge Weight", ylab="Log of Number of Non-singleton Components")
          
# SO:
# Small number of highly connected components (groups of actors who collectively have worked amongst themselves) following power-law-esque curve.
# Suggests that most actors work with a fairly varied selection of people, but there are a handful of groups that've worked together as many as thirteen times over 10 years
# TODO: 
# - is this actually a power law?
# - doesn't consider the size of these components at each step
# - figure out WHO is in each of these components, how/when they split/merge as our filtration goes
################################################################################



########################## Fitting a power law to the connected component sizes
kmin = 2
data_fit = unlist(num_sig_comps_hist)[kmin:13]
num_points = length(data_fit)
data_sum = sum(log(data_fit/(kmin -1/2)))

alpha = 1 + num_points/data_sum

alpha
# so that's not quite what we'd expect... possibly because the dataset is so tiny. 
# if we start at kmin=5 we get alpha = 2.288 but I don't know what the justification 
# for that would be other than it making alpha more like we want lol
################################################################################


################### Figuring out who is in each of these long-lasting components
# - lol maybe not I bet this is WAY easier in Gephi
################################################################################


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
