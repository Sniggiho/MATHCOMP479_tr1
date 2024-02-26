# A script to create a network of actors, with edges corresponding to actors who've starred in the same films
# For MATHCOMP479, 02/24
# Rhys O'Higgins

library(igraph)
library(tidyverse)
library(Matrix)
library(r2r)

# Reading in the files
actors <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-actor.csv")
crew <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-crew.csv")
movies <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-movies.csv")
cast_edges <- read_csv("~/MATHCOMP479/tr1/movienetwork/edge-cast.csv")

# processing  to include only the last 10 years
movies <- movies %>% filter(Year >= 2007)
cast_edges <- cast_edges %>% filter(Year >= 2007)
actors_after_2007 <- unique(cast_edges$Target)
# 
# m <- hashmap()
# for (key in unique(cast_edges$Source)){
#   m[[key]] <- (cast_edges %>% filter(Source==key) %>% select(Target))[[1]]
# }
# 
# m <- hashmap(list(cast_edges$Source, cast_edges$Target))
# m[cast_edges$Source] <- cast_edges$Target
# 
# m[keys(m)[1]]
# keys(m)[1]
# creating links between actors

actr_edges <- matrix(0, 1000000,2)



n <- 1

i1 <- 0
i2 <- 0
i3 <- 0

for (movie in unique(cast_edges$Source)){
  
  t1 <- Sys.time() 
  
  actors_in_movie <- cast_edges$Target[cast_edges$Source == movie]
  
  t2 <- Sys.time() 
  
  if (length(actors_in_movie )>= 2){
    
    actor_pairs <- combn(actors_in_movie,2)
    
    t3 <- Sys.time() 
    
    actr_edges[n:n+length(actor_pairs[1,]),] <- t(actor_pairs)
    
    n <- n+length(actor_pairs[1,])+1
  
    t4 <- Sys.time() 
    
    i1 <- i1 + t2-t1
    i2 <- i2 + t3-t2
    i3 <- i3 + t4-t3
    
    t <- t+1
    
    if (t%%10==0){
      print(paste("i1: ", i1, " i2: ", i2, " i3: ", i3, " total movies processed: ", t))
    }
    
  }
  
}
 
write.matrix(actr_edges,file="actor_edges.csv")


