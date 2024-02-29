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

edges <- uncount(as.data.frame(as_edgelist(pruned_graph)), E(pruned_graph)$weight)

write_csv(as.data.frame(edges), "smaller_actr_edges.csv")

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


################################ figuring out fraction of collaborative actors
min_weight <- 1
pruned_graph <- igraph::delete_edges(actr_actr_graph, which(E(actr_actr_graph)$weight < min_weight))

actrs_in_collabs_n_times <- rep(NA,13)
actrs_starring_n_times <- rep(NA,13)


for (min_weight in 1:13){
  pruned_graph <- igraph::delete_edges(actr_actr_graph, which(E(actr_actr_graph)$weight < min_weight))
  
  actrs_in_collabs_n_times[min_weight] <- sum(components(pruned_graph)$csize[which(components(pruned_graph)$csize>1)])
  actrs_starring_n_times[min_weight] <- sum(table(cast_edges$Target) >= min_weight) 
}

actrs_starring_n_times
actrs_in_collabs_n_times


gf_point(actrs_in_collabs_n_times/actrs_starring_n_times ~ 1:13, xlab="Number of movies, n", ylab = "Actors in an n-time repeat collab / actors starring at least n times")
gf_point(actrs_starring_n_times ~ 1:13, xlab = "Number of Movies", ylab = "Number of Actors", title = "Number of actor's who've starred in at least x movies for x in 1:13")


kmin = 2
data_fit = actrs_in_collabs_n_times[kmin:13]
num_points = length(data_fit)
data_sum = sum(log(data_fit/(kmin -1/2)))

alpha = 1 + num_points/data_sum

alpha

###############################################################################



min_weight <- 8
pruned_graph <- igraph::delete_edges(actr_actr_graph, which(E(actr_actr_graph)$weight < min_weight))
ecount(pruned_graph)

sum(components(pruned_graph)$csize[which(components(pruned_graph)$csize>1)])
sum(table(cast_edges$Target) >= 8)

cast_edges %>% filter(Target == "a17051")
actors %>% filter(Id == "a17051")

max(table(cast_edges$Target))

x
table(x)
sum(table(x)>=1)

comps <- components(pruned_graph)
compsizes <- comps$csize[which(comps$csize>1)]
compsizes

relevant_actrs <- actors %>% filter(Id %in% V(pruned_graph)$name[which(degree(pruned_graph)>1)])
write_csv(relevant_actrs, "relevant_actrs.csv")


big_clump <- lapply(seq_along(comps$csize)[comps$csize >1], function(x) 
  V(pruned_graph)$name[comps$membership %in% x])

big_clump

for (i in 1:7){
  print(actors$Label[actors$Id %in% big_clump[[i]]])
  print("--")
}

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
degree(testg)
igraph::components(testg)
