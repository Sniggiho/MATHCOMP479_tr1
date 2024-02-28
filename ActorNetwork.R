# A script to create a network of actors, with edges corresponding to actors who've starred in the same films
# For MATHCOMP479, 02/24
# Rhys O'Higgins

library(igraph)
library(tidyverse)
library(Matrix)

# Reading in the files
actors <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-actor.csv")
crew <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-crew.csv")
movies <- read_csv("~/MATHCOMP479/tr1/movienetwork/vertex-movies.csv")
cast_edges <- read_csv("~/MATHCOMP479/tr1/movienetwork/edge-cast.csv")

# processing  to include only the last 10 years
movies <- movies %>% filter(Year >= 2007)
cast_edges <- cast_edges %>% filter(Year >= 2007)
actors_after_2007 <- unique(cast_edges$Target)
  

# creating links between actors

actr_edge_list <- list()

t <- 1

i1 <- 0
i2 <- 0
i3 <- 0

for (movie in unique(cast_edges$Source)){
  
  t1 <- Sys.time()  
  
  actors_in_movie <- cast_edges$Target[which(cast_edges$Source==movie)]
    
  t2 <- Sys.time() 
  
  if (length(actors_in_movie )>= 2){
    
    actor_pairs <- combn(actors_in_movie,2)
    
    t3 <- Sys.time() 
    
    actr_edge_list[[t]] <- t(actor_pairs)
  
    t4 <- Sys.time() 
    
    i1 <- i1 + t2-t1
    i2 <- i2 + t3-t2
    i3 <- i3 + t4-t3
    
    
    if (t%%1000==0){
      print(paste("i1: ", i1, " i2: ", i2, " i3: ", i3, " total movies processed: ", t))
    }
    
    t <- t+1
    
  }
  
}

actr_edge_mat <- do.call(rbind, actr_edge_list)
actr_edges <- c(t(actr_edge_mat))

