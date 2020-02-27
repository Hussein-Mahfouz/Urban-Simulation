# run the LondonTube script to have the network ready
source('LondonTube/london_tube.R')

# LOGIC: 
# calculate measure
# save in dataframe
# sort nodes
# remove highest one

# dataframe to store values in (initially created with 1 column)
df_results <- data.frame("Nodes Removed" = 0:10)
# clone the graph so as not to ruin the original one
g_london_bet <- g_london

# BETWEENESS CENTRALITY 

# iterative deleting (delete and recalculate at each step)
for (i in 1:11){
  # get betweeness
  bet_london=betweenness(g_london_bet, v=V(g_london_bet), directed = F, normalized = FALSE)
  #save average value to dataframe
  df_results[i,2] <- mean(bet_london)
  # sort tube stations by centrality measure value - descending order
  # convert to dataframe so as to extract names in next step
  sorted_between = sort(bet_london, decreasing = TRUE) %>%
    as.data.frame()
  # remove highest scoring vertex
  # row.names to get the tube station name
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

for (i in 1:11){
  #save average value to third column of dataframe
  df_results[i,3] <- mean(bet_london)
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_bet = delete.vertices(g_london_bet, c(row.names(sorted_between)[i]))
  # calculate betweeness
  bet_london=betweenness(g_london_bet, v=V(g_london_bet), directed = F, normalized = FALSE)
}

# CLOSENESS CENTRALITY
# this doesn't work well when we have more than one connected component (cannot travel between different components)
# https://stackoverflow.com/questions/55876664/warning-message-when-using-closness-in-igraph

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_close <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:11){
  # get closeness
  close_london=closeness(g_london_close)  
  # normalize
  normalised_close_london=(close_london-min(close_london))/(max(close_london)-min(close_london))
  #save average value to dataframe
  df_results[i,4] <- mean(normalised_close_london)
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

for (i in 1:11){
  #save average value to third column of dataframe
  df_results[i,5] <- mean(normalised_close_london)
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_close = delete.vertices(g_london_close, c(row.names(sorted_close)[i]))
  # calculate betweeness
  close_london=closeness(g_london_close) 
  # normalize
  normalised_close_london=(close_london-min(close_london))/(max(close_london)-min(close_london))
}


# DEGREE CENTRALITY

# 1) iteratively 

# clone the graph so as not to ruin the original one
g_london_deg <- g_london
# iterative deleting (delete and recalculate at each step)
for (i in 1:11){
  # get closeness
  deg_london=degree(g_london_deg)  
  #save average value to dataframe
  df_results[i,6] <- mean(deg_london)
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

# # clone the graph so as not to ruin the original one
# g_london_eigen <- g_london
# # iterative deleting (delete and recalculate at each step)
# for (i in 1:11){
#   # get closeness
#   eigen_london=eigen_centrality(g_london_eigen)  
#   #save average value to dataframe
#   df_results[i,7] <- mean(eigen_london$vector)
#   # sort tube stations by centrality measure value - descending order
#   # convert to dataframe so as to extract names in next step
#   sorted_eigen = sort(eigen_london$vector, decreasing = TRUE) %>%
#     as.data.frame()
#   # remove highest scoring vertex
#   # row.names to get the tube station name
#   g_london_eigen = delete.vertices(g_london_eigen, c(row.names(sorted_eigen)[1]))
# }


# rename dataframe columns
names(df_results)[2] <- "Betweeness Iterative"
names(df_results)[3] <- "Betweeness AAO"
names(df_results)[4] <- "Closeness Iterative"
names(df_results)[5] <- "Closeness AAO"








# plot results

library(ggplot2)
library(reshape2)

df_plot <- melt(df_results,  id.vars = 'Nodes', variable.name = 'series')


# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df_plot, aes(Nodes,value)) + 
  geom_line(aes(colour = series))



