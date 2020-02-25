# run the LondonTube script to have the network ready
source('LondonTube/london_tube.R')

# LOGIC: 
# calculate measure
# save in dataframe
# sort nodes
# remove highest one

# dataframe to store values in (initially created with 1 column)
df_results <- data.frame("Nodes" = 1:10)
# clone the graph so as not to ruin the original one
g_london_bet <- g_london

# iterative deleting (delete and recalculate at each step)
for (i in 1:10){
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

for (i in 1:10){
  #save average value to third column of dataframe
  df_results[i,3] <- mean(bet_london)
  # remove highest scoring vertex
  # row.names to get the tube station name
  # use i instead of 1: we are not resorting so 1st tube station has been deleted and will raise an error in loop
  g_london_bet = delete.vertices(g_london_bet, c(row.names(sorted_between)[i]))
  # calculate betweeness
  bet_london=betweenness(g_london_bet, v=V(g_london_bet), directed = F, normalized = FALSE)
}


# rename dataframe columns
names(df_results)[2] <- "betweeness Iterative"
names(df_results)[3] <- "betweeness AAO"

# plot results

library(ggplot2)
library(reshape2)

df_plot <- melt(df_results,  id.vars = 'Nodes', variable.name = 'series')


# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df_plot, aes(Nodes,value)) + 
  geom_line(aes(colour = series))



