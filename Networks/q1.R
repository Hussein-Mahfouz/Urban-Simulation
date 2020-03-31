# run the LondonTube script to have the network ready
source('LondonTube/london_tube.R')

library(tidyverse)
library(bc3net) # to extract giant connected component

# LOGIC: 
# calculate measure
# save in dataframe
# sort nodes
# remove highest one

# dataframe to store values in (initially created with 1 column)
    #the length is equal to the number of vertices
inv_shortest_path <- data.frame("Nodes Removed" = 0:gorder(g_london))
gcc <- data.frame("Nodes Removed" = 0:gorder(g_london))


# BETWEENESS CENTRALITY 

# clone the graph so as not to ruin the original one
g_london_bet <- g_london
# get initial number of vertices 
n_ver_init <- gorder(g_london)

# iterative deleting (delete and recalculate at each step)
for (i in 1:gorder(g_london)){
  # get betweeness
  bet_london=betweenness(g_london_bet, v=V(g_london_bet), directed = F, normalized = FALSE)
  #get the fraction of nodes removed (this is the x - axis)
  inv_shortest_path[i,2] <- (1 - ((gorder(g_london) - (i-1)) / gorder(g_london)))
  gcc[i,2] <- (1 - ((gorder(g_london) - (i-1)) / gorder(g_london)))
  # get inverse shortest path and save value to dataframe
      # 1.get distance matrix
  dist_mat <- distances(g_london_bet, algorithm = "dijkstra")
      # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
      # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
      # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
      # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,3] <- sum(dist_inv) / ver_pair
      # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,4] <- inv_shortest_path[i,3] / inv_shortest_path[1,3]
  # get gcc ratio score (this score is normalized)
  gcc[i,3] = (gorder(getgcc(g_london_bet)))/(gorder(g_london_bet))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_between = sort(bet_london, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex (row.names to get the tube station name)
  g_london_bet = delete.vertices(g_london_bet, c(row.names(sorted_between)[1]))
}

# deleting all at once (DOES NOT RE-SORT BETWEEN CALCULATIONS)

# clone the graph so as not to ruin the original one
# need to do this because old one has deleted nodes
g_london_bet <- g_london
# get betweeness
bet_london=betweenness(g_london_bet, v=V(g_london_bet), directed = F, normalized = FALSE)
# convert to dataframe so as to extract names in next step
sorted_between = sort(bet_london, decreasing = TRUE) %>%
  as.data.frame()

for (i in 1:101){
  # get inverse shortest path and save value to dataframe
  # 1.get distance matrix
  dist_mat <- distances(g_london_bet, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,5] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,6] <- inv_shortest_path[i,5] / inv_shortest_path[1,5]
  # gcc score
  gcc[i,4] = (gorder(getgcc(g_london_bet)))/(gorder(g_london_bet))
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_bet = delete.vertices(g_london_bet, c(row.names(sorted_between)[i]))
}

# CLOSENESS CENTRALITY
# this doesn't work well when we have more than one connected component (cannot travel between different components)
# https://stackoverflow.com/questions/55876664/warning-message-when-using-closness-in-igraph

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_close <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:101){
  # get closeness
  close_london=closeness(g_london_close)  
  # normalize
  normalised_close_london=(close_london-min(close_london))/(max(close_london)-min(close_london))
  #save average value to dataframe
  inv_shortest_path[i,4] <- mean(is.finite(1/distances(g_london_close)))
  # gcc score
  gcc[i,4] = (gorder(getgcc(g_london_close)))/(gorder(g_london_close))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_close = sort(normalised_close_london, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
  g_london_close = delete.vertices(g_london_close, c(row.names(sorted_close)[1]))
}

# 2) All at Once

# clone the graph so as not to ruin the original one
# need to do this because old one has deleted nodes
g_london_close <- g_london
# get betweeness
close_london=closeness(g_london_close)  
# normalize
normalised_close_london=(close_london-min(close_london))/(max(close_london)-min(close_london))
# convert to dataframe so as to extract names in next step
sorted_close = sort(normalised_close_london, decreasing = TRUE) %>%
  as.data.frame()

for (i in 1:101){
  #save average value to third column of dataframe
  inv_shortest_path[i,5] <- mean(is.finite(1/distances(g_london_close)))
  # gcc score
  gcc[i,5] = (gorder(getgcc(g_london_close)))/(gorder(g_london_close))
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_close = delete.vertices(g_london_close, c(row.names(sorted_close)[i]))
}


# DEGREE CENTRALITY

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_deg <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:100){
  # get closeness
  deg_london=degree(g_london_deg)  
  #save average value to dataframe
  inv_shortest_path[i,6] <- mean(is.finite(1/distances(g_london_deg)))
  # gcc score
  gcc[i,6] = (gorder(getgcc(g_london_deg)))/(gorder(g_london_deg))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_deg = sort(deg_london, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
  g_london_deg = delete.vertices(g_london_deg, c(row.names(sorted_deg)[1]))
}

# ALL AT ONCE

#    .........    # 



# EIGENVECTOR  (need to think about this)

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_eigen <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:100){
  # get closeness
  eigen_london=eigen_centrality(g_london_eigen)  
  #save average value to dataframe
  inv_shortest_path[i,7] <- mean(is.finite(1/distances(g_london_eigen)))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_eigen = sort(eigen_london$vector, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
  g_london_eigen = delete.vertices(g_london_eigen, c(row.names(sorted_eigen)[1]))
}


# rename dataframe columns
  # inv_shortest_path df
names(inv_shortest_path)[2] <- "% of Nodes Removed"
names(inv_shortest_path)[3] <- "Betweeness Iterative"
names(inv_shortest_path)[4] <- "Normalized Betweeness Iterative"
names(inv_shortest_path)[5] <- "Betweeness AAO"
names(inv_shortest_path)[6] <- "Normalized Betweeness AAO"
  # gcc df
names(gcc)[2] <- "% of Nodes Removed"
names(gcc)[3] <- "Betweeness Iterative"
names(gcc)[4] <- "Betweeness AAO"








# plot results

library(ggplot2)
library(reshape2)

df_plot <- melt(inv_shortest_path,  id.vars = 'Nodes', variable.name = 'series')


# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df_plot, aes(Nodes,value)) + 
  geom_line(aes(colour = series))


# TO GET Largest Connected Component

# getgcc() extracts the component
# gorder() gets no of nodes/vertices
# gorder(getgcc(graph)) ---> no of nodes in largest gcc
