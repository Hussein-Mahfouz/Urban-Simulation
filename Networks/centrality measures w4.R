library(igraph)

#file constructed by Yaan
file_network <- "centrality_practical/network_dad.csv"

#read and get network
dat=read.csv(file_network,header=TRUE)
head(dat)


g_dad=graph.data.frame(dat,directed=FALSE)
#let us plot it
plot(g_dad)

# DEGREE CENTRALITY

# degree of a node refers to the number of links connected to it. Let us plot the 
# network again including this new piece of information:

#Let us compute the degree centrality
deg_dad=degree(g_dad, v=V(g_dad), loops = F, normalized = FALSE)

print(deg_dad)

#we can plot the graph using the measure of degree
plot(g_dad,vertex.size=deg_dad/max(deg_dad)*14,vertex.color=deg_dad,vertex.label.cex=0.5)
title(main = "Social network of dad seen by Yaan")

# CLOSENESS CENTRALITY

# Average distance of node to all other nodes

# A node that is very close to most nodes, and has hence low mean geodesic, 
# will be more influential than a node which is far away. 

# given that the network is weighted let us introduce the weights.
v_weights=1/(E(g_dad)$weight)

# Note that we inverted the weights, since they are meant to represent a distance.
# the higher the value to closer they are
clos_dad=closeness(g_dad,weights = v_weights)

# the following two commands are just to choose colours for the nodes, a palette
normalised_clos_dad=(clos_dad-min(clos_dad))/(max(clos_dad)-min(clos_dad))
palette=hsv(h=1-((normalised_clos_dad*2/3)+1/3),s = 1,v=1)

# Not we can plot it
plot(g_dad,vertex.color=palette,vertex.size=normalised_clos_dad*15,vertex.label.cex=.5)
title(main = "Closeness centrality of dad's network according to Yaan")


#TOPOLOGICAL CLOSENESS

# this is basically unweighted closeness centrality. It only corresponds to 
# the shortest path computed according to the number of links between them.

clos_top_dad=closeness(g_dad, weights = NA)
normalised_clos_top_dad=(clos_top_dad-min(clos_top_dad))/(max(clos_top_dad)-min(clos_top_dad))
palette_top=hsv(h=1-((normalised_clos_top_dad*2/3)+1/3),s = 1,v=1)
plot(g_dad,vertex.color=palette_top,vertex.size=normalised_clos_top_dad*15,vertex.label.cex=.5)
title(main = "Topological closeness centrality")

# BETWEENNESS CENTRALITY 

# The betweenness centrality of a vertex corresponds to the number of shortest paths 
# passing through it among all pairs. Edge betweennness is defined in a similar, where the 
# edge is within the shortest path.

#Let us compute the betweenness centrality for the network 
bet_dad=betweenness(g_dad, v=V(g_dad), directed = F, normalized = FALSE, weights = (E(g_dad)$weight))
palette=hsv(h=1-((bet_dad/max(bet_dad)*2/3)+1/3),s = 1,v=1)
plot(g_dad,vertex.color=palette,vertex.size=bet_dad/max(bet_dad)*18,vertex.label.cex=.5)
title(main = "Broker of dad's network seen by Yaan")

# Topological (Unweighted) Betweenness

bet_dad_top=betweenness(g_dad, v=V(g_dad), directed = F, normalized = FALSE, weights = NA)
palette_top=hsv(h=1-((bet_dad_top/max(bet_dad_top)*2/3)+1/3),s = 1,v=1)
plot(g_dad,vertex.color=palette_top,vertex.size=bet_dad_top/max(bet_dad_top)*18,vertex.label.cex=.5)
title(main = "Broker (topological) of dad's network seen by Yaan")


