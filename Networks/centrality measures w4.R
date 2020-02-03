library(igraph)

#file constructed by Yaan
file_network <- "centrality_practical/network_dad.csv"

#read and get network
dat=read.csv(file_network,header=TRUE)
head(dat)


g_dad=graph.data.frame(dat,directed=FALSE)
#let us plot it
plot(g_dad)

# DDEGREE CENTRALITY

# degree of a node refers to the number of links connected to it. Let us plot the 
# network again including this new piece of information:

#Let us compute the degree centrality
deg_dad=degree(g_dad, v=V(g_dad), loops = F, normalized = FALSE)

print(deg_dad)

#we can plot the graph using the measure of degree
plot(g_dad,vertex.size=deg_dad/max(deg_dad)*14,vertex.color=deg_dad,vertex.label.cex=0.5)
title(main = "Social network of dad seen by Yaan")