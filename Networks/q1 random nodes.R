## ------- run this after q1 ------- ##

# create the dataframes
df_random_inv <- data.frame("Nodes Removed" = 0:gorder(g_london))
df_random_gcc <- data.frame("Nodes Removed" = 0:gorder(g_london))


# clone the graph so as not to ruin the original one
g_london_rand <- g_london

# Just add the first column values
for (i in 1:gorder(g_london)){
  #get the fraction of nodes removed (this is the x - axis)
  df_random_inv[i,2] <- (1 - ((gorder(g_london) - (i-1)) / gorder(g_london))) * 100 
  df_random_gcc[i,2] <- (1 - ((gorder(g_london) - (i-1)) / gorder(g_london))) * 100
}

# create a dataframe of station names that can be accessed for deleting nodes
# hacky way of doing it (just any centrality measure then sort)
sorted_rand = degree(g_london_rand) %>% sort(decreasing = TRUE) %>%
  as.data.frame()
for (i in 1:100){
  # create a dataframe numbers sorted in random order
  nodes <- sample(1:gorder(g_london), gorder(g_london), replace=F)
  g_london_rand <- g_london
  for (j in 1:gorder(g_london)){
    # get inverse shortest path and save value to dataframe
    # 1.get distance matrix
    dist_mat <- distances(g_london_rand, algorithm = "dijkstra")
    # 2. get inverse of each shortest path
    dist_inv <- 1/dist_mat
    # 3. turn INF at the diagonal to 0s (if we do is.finite instead then diagonals aren't counted in denominator)
    dist_inv[is.infinite(dist_inv)] <- 0
    # 4. get number of vertex pairs (demonitaor = N(N-1))
    ver_pair <- ((n_ver_init - j) * (n_ver_init - (j+1)))
    # 5. store in results df mean(dist_inv) is the mean inverse shortest path
    df_random_inv[j,i+2] <- sum(dist_inv) / ver_pair
    # 6. normalize the results (divide by result obtained from first iteration)
      #6.1. variable x is the 1st (largest) row value. It is the denominator in the next step of normalization
    if(j == 1){
      denom <- df_random_inv[1,i+2]
    }
      #6.2. normalize
    df_random_inv[j,i+2] <- df_random_inv[j,i+2] / denom
    # get gcc ratio score (this score is normalized)
    df_random_gcc[j,i+2] = (gorder(getgcc(g_london_rand)))/(gorder(g_london))
    # remove highest scoring vertex (row.names to get the tube station name)
    # nodes[j] gets the number from the row df created above
    g_london_rand = delete.vertices(g_london_rand, row.names(sorted_rand)[nodes[j]])
  }
}

# create a dataframe with the average of each row
df_random_inv <- data.frame(ID=df_random_inv[,2], Means=rowMeans(df_random_inv[,-c(1:2)]))
df_random_gcc <- data.frame(ID=df_random_gcc[,2], Means=rowMeans(df_random_gcc[,-c(1:2)]))

# rename columns so you can merge them in the next step
names(df_random_inv)[1] <- "Perc_of_Nodes_Removed"
names(df_random_inv)[2] <- "Random_Removal"

names(df_random_gcc)[1] <- "Perc_of_Nodes_Removed"
names(df_random_gcc)[2] <- "Random_Removal"

# merge dataframes with ones containing results based on centrality measures (for plotting)
inv_shortest_path <- merge(inv_shortest_path, df_random_inv, by = "Perc_of_Nodes_Removed")
gcc               <- merge(gcc, df_random_gcc, by = "Perc_of_Nodes_Removed")