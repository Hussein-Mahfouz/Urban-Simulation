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
  gcc[i,3] = (gorder(getgcc(g_london_bet)))/(gorder(g_london))
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

for (i in 1:gorder(g_london)){
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
  gcc[i,4] = (gorder(getgcc(g_london_bet)))/(gorder(g_london))
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
for (i in 1:gorder(g_london)){
  # get closeness
  close_london=closeness(g_london_close)  
  # normalize
  normalised_close_london=(close_london-min(close_london))/(max(close_london)-min(close_london))
  # get inverse shortest path and save value to dataframe
  # 1.get distance matrix
  dist_mat <- distances(g_london_close, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,7] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,8] <- inv_shortest_path[i,7] / inv_shortest_path[1,7]
  # gcc score
  gcc[i,5] = (gorder(getgcc(g_london_close)))/(gorder(g_london))
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

for (i in 1:gorder(g_london)){
  # 1.get distance matrix
  dist_mat <- distances(g_london_close, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,9] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,10] <- inv_shortest_path[i,9] / inv_shortest_path[1,9]
  # gcc score
  gcc[i,6] = (gorder(getgcc(g_london_close)))/(gorder(g_london))
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
for (i in 1:gorder(g_london)){
  # get closeness
  deg_london=degree(g_london_deg)  
  # 1.get distance matrix
  dist_mat <- distances(g_london_deg, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,11] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,12] <- inv_shortest_path[i,11] / inv_shortest_path[1,11]
  # gcc score
  gcc[i,7] = (gorder(getgcc(g_london_deg)))/(gorder(g_london))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_deg = sort(deg_london, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
  g_london_deg = delete.vertices(g_london_deg, c(row.names(sorted_deg)[1]))
}

# ALL AT ONCE

g_london_deg <- g_london
# get degree
deg_london=degree(g_london_deg) 
#sort and convert to dataframe so as to extract names in next step
sorted_deg = sort(deg_london, decreasing = TRUE) %>%
  as.data.frame()

for (i in 1:gorder(g_london)){
  # get inverse shortest path and save value to dataframe
  # 1.get distance matrix
  dist_mat <- distances(g_london_deg, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,13] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,14] <- inv_shortest_path[i,13] / inv_shortest_path[1,13]
  # gcc score
  gcc[i,8] = (gorder(getgcc(g_london_deg)))/(gorder(g_london))
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_deg = delete.vertices(g_london_deg, c(row.names(sorted_deg)[i]))
}



# EIGENVECTOR  

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_eigen <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:gorder(g_london)){
  # get closeness
  eig_london=eigen_centrality(g_london_eigen)  
  # 1.get distance matrix
  dist_mat <- distances(g_london_eigen, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,15] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,16] <- inv_shortest_path[i,15] / inv_shortest_path[1,15]
  # gcc score
  gcc[i,9] = (gorder(getgcc(g_london_eigen)))/(gorder(g_london))
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_eigen = sort(eig_london[[1]], decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
  g_london_eigen = delete.vertices(g_london_eigen, c(row.names(sorted_eigen)[1]))
}

# 2) All at Once

g_london_eigen <- g_london
# get degree
eig_london=eigen_centrality(g_london_eigen) 
#sort and convert to dataframe so as to extract names in next step
sorted_eigen = sort(eig_london[[1]], decreasing = TRUE) %>%
  as.data.frame()

for (i in 1:gorder(g_london)){
  # get inverse shortest path and save value to dataframe
  # 1.get distance matrix
  dist_mat <- distances(g_london_eigen, algorithm = "dijkstra")
  # 2. get inverse of each shortest path
  dist_inv <- 1/dist_mat
  # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
  dist_inv[is.infinite(dist_inv)] <- 0
  # 4. get number of vertex pairs (demonitaor = N(N-1))
  ver_pair <- ((n_ver_init - i) * (n_ver_init - (i+1)))
  # 5. store in results df mean(dist_inv) is the mean inverse shortest path
  inv_shortest_path[i,17] <- sum(dist_inv) / ver_pair
  # 6. normalize the results (divide by result obtained from first iteration)
  inv_shortest_path[i,18] <- inv_shortest_path[i,17] / inv_shortest_path[1,17]
  # gcc score
  gcc[i,10] = (gorder(getgcc(g_london_eigen))/(gorder(g_london)))
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_eigen = delete.vertices(g_london_eigen, c(row.names(sorted_eigen)[i]))
}





# RENAME DF COLUMNS

  # inv_shortest_path df
names(inv_shortest_path)[2] <- "Perc_of_Nodes_Removed"
names(inv_shortest_path)[3] <- "Betweeness_Iterative"
names(inv_shortest_path)[4] <- "Normalized_Betweeness_Iterative"
names(inv_shortest_path)[5] <- "Betweeness_AAO"
names(inv_shortest_path)[6] <- "Normalized_Betweeness_AAO"
names(inv_shortest_path)[7] <- "Closeness_Iterative"
names(inv_shortest_path)[8] <- "Normalized_Closeness_Iterative"
names(inv_shortest_path)[9] <- "Closeness_AAO"
names(inv_shortest_path)[10] <- "Normalized_Closeness_AAO"
names(inv_shortest_path)[11] <- "Degree_Iterative"
names(inv_shortest_path)[12] <- "Normalized_Degree_Iterative"
names(inv_shortest_path)[13] <- "Degree_AAO"
names(inv_shortest_path)[14] <- "Normalized_Degree_AAO"
names(inv_shortest_path)[15] <- "Eigenvector_Iterative"
names(inv_shortest_path)[16] <- "Normalized_Eigenvector_Iterative"
names(inv_shortest_path)[17] <- "Eigenvector_AAO"
names(inv_shortest_path)[18] <- "Normalized_Eigenvector_AAO"

  # gcc df
names(gcc)[2] <- "Perc_of_Nodes_Removed"
names(gcc)[3] <- "Betweeness_Iterative"
names(gcc)[4] <- "Betweeness_AAO"
names(gcc)[5] <- "Closeness_Iterative"
names(gcc)[6] <- "Closeness_AAO"
names(gcc)[7] <- "Degree_Iterative"
names(gcc)[8] <- "Degree_AAO"
names(gcc)[9] <- "Eigenvector_Iterative"
names(gcc)[10] <- "Eigenvector_AAO"

