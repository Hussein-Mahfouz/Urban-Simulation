library(igraph)
library(rgdal)
library(rgeos)
library(ggplot2)
library(geosphere)

# read shapefile downloaded from OSM
folderSpatialData="Practical_OSM/Central_London_Shapefile/shape/"
shapeFile <- readOGR(dsn = folderSpatialData, "roads")

# To look at the first lines of your data in R
head(shapeFile@lines)

# To get an idea of the sort of data stored
head(shapeFile@data)

# convert it into a data frame
shapeFile.f=fortify(shapeFile)
head(shapeFile.f)

# construct the list of connecting nodes for the network
# why 2? This removes the first row which has data #
shapeFile.f.postOrder=shapeFile.f[2:nrow(shapeFile.f),c("long","lat","id")]
shapeFile.f.postOrder=rbind(shapeFile.f.postOrder,shapeFile.f[1,c("long","lat","id")])
head(shapeFile.f.postOrder)

# this puts connected node pairs in the same row (like an OD matrix...)
shapeFile.f.segments=data.frame(idA=shapeFile.f$id,idB=shapeFile.f.postOrder$id,longA=shapeFile.f$long,latA=shapeFile.f$lat,longB=shapeFile.f.postOrder$long,latB=shapeFile.f.postOrder$lat)

dim(shapeFile.f.segments)

shapeFile.f.segments[1:20,]

# remove the links between nodes that are not part of the same segment. That is, that have different idA and idB
shapeFile.f.segments=shapeFile.f.segments[shapeFile.f.segments$idA==shapeFile.f.segments$idB,]
dim(shapeFile.f.segments)

shapeFile.f.segments[1:20,]

# Rename columns and do some manipulations simply to assign unique ids to the points
coordinatesSegments=shapeFile.f.segments[,c("longA","latA","longB","latB")]
colnames(coordinatesSegments)[colnames(coordinatesSegments)=="longA"]="x1"
colnames(coordinatesSegments)[colnames(coordinatesSegments)=="latA"]="y1"
colnames(coordinatesSegments)[colnames(coordinatesSegments)=="longB"]="x2"
colnames(coordinatesSegments)[colnames(coordinatesSegments)=="latB"]="y2"

head(coordinatesSegments)


pointsA=coordinatesSegments[,c("x1","y1")]
pointsB=coordinatesSegments[,c("x2","y2")]

colnames(pointsA)[1]="x"
colnames(pointsA)[2]="y"

colnames(pointsB)=colnames(pointsA)

# assign an individual id to each of the points
points=rbind(pointsA,pointsB)
dim(points)

#many are repeated
points=unique(points)
dim(points)
# to add id
points=data.frame(id=1:nrow(points),points)
head(points)

# Now that we have the ids for the points we can create a list for the network 
# in the usual format: Node1 Node2. We start by assigning the id to all 
# Nodes 1:

segments=merge(coordinatesSegments,points,by.x=c("x1","y1"),by.y=c("x","y"))
head(segments)

colnames(segments)[colnames(segments)=="id"]="idNode1"
head(segments)

# Nodes 2: 
segments=merge(segments,points,by.x=c("x2","y2"),by.y=c("x","y"))
head(segments)

colnames(segments)[colnames(segments)=="id"]="idNode2"
head(segments)

# If we want the distance between the points, that is, in order to compute the length of the street, 
# we need to either project the points which are in lat long or we need to compute the distance 
# in the ellipsoid. Let us do the latter using a function constructed for this: distGeo

p1=data.frame(segments$x1,segments$y1)
p2=data.frame(segments$x2,segments$y2)
head(p2)

distance=distGeo(p1,p2)

# assign a new field to the segments: the distance
segments=data.frame(segments,distance=distance)
head(segments)

segments=segments[distance>0,]

# CONSTRUCT THE NETWORK

ncol=data.frame(idNode1=segments$idNode1,idNode2=segments$idNode2,weight=segments$distance)

head(ncol)

# Now that we have created a network, let us save it as a list
dir.create("Practical_OSM/data")

file_out <- paste0('Practical_OSM/data/London_network.txt')
write.table(ncol, file = file_out, row.names=FALSE,col.names=T,sep=" ")

# COMPUTATIONS ON THE NETWORK

# define the graph using the format above
G_all=graph.data.frame(ncol,directed=F)

#look at the properties of the graph
summary(G_all)
# unweighted graph with 10548 vertices or nodes and 12449 edges or links.

# To look at the edges and vertices, simply type E(G) and V(G). To obtain an edge list type:
head(get.edgelist(G_all))

# plot it
plot(G_all,vertex.label=NA,vertex.size=.01)

# This doesn’t look like London!!! –> let us assign coordinates to the nodes

coordsPoints=points[points$id%in%as.numeric(V(G_all)$name),]

matched=match(as.numeric(V(G_all)$name),coordsPoints[,1])
V(G_all)$x=coordsPoints[matched,2]
V(G_all)$y=coordsPoints[matched,3]

# Now if we plot it we see the streets! Can you recongise any of them? ;-)

plot(G_all,vertex.label=NA,vertex.size=.01)

# let us see if the network is disconnected:
components(G_all)$no

summary(components(G_all))

# There are 55 disconnected components. This operation gives you the membership of each point, the cluster 
# size of each component is obtained by typing $csize and the number of clusters $no.

dist_sizes=components(G_all)$csize
dist_sizes[1:20]

#Let us compute the size of largest component
largest_cpt=max(dist_sizes)
tot_nodes=length(V(G_all))
largest_cpt/tot_nodes

# ~96% of the nodes belong to the largest component.

# Some disconnected components are the outcome of the way we selected the area of interest, while 
# some others might be a problem in the construction of the road networks: segments of streets disconnected, etc.

# Let us take only the largest connected component

gclust<-components(G_all, mode='weak')
largestConnectedComponent<-induced.subgraph(G_all, V(G_all)[which(gclust$membership == which.max(gclust$csize))])
G=largestConnectedComponent
all_weights=(E(G)$weight)
size_net=length(V(G))

# And if we plot it again

plot(G,vertex.label=NA,vertex.size=.01)

# CENTRALITY MEASURES

# closeness centrality
closeness_G=closeness.estimate(G,cutoff=1000)

# Note that we use “closeness.estimate” instead of “closeness”. In the documentation igraph.org: 
# “estimate_closeness only considers paths of length cutoff or smaller, this can be run for larger graphs, 
# as the running time is not quadratic (if cutoff is small). If cutoff is zero or negative then the function 
# calculates the exact closeness scores.”

# Let us define some nice colours for our nodes in the map

getHSVColors=function(values,s=1,v=1){
  values=(values[values!=-1])
  vnorm=(values-min(values))/(max(values)-min(values))
  h=-2/3*vnorm+2/3
  colors=values
  colors[values!=-1]= hsv(h,s,v)
  colors[values==-1]= hsv(0,0,1)
  return (colors)
}

# And let us plot it now

plot(G,vertex.size=2,vertex.color=getHSVColors(closeness_G),vertex.label=NA)

# degree distribution
degree_G=degree(G)
head(degree_G)

freq_degree=table(degree_G)
plot(freq_degree)

# CLEANING THE NETWORK
delta_x=0.01
delta_y=0.008
T_West=min(V(G)$x)+delta_x
T_North= max(V(G)$y)-delta_y
T_East=max(V(G)$x)-delta_x
T_South=min(V(G)$y)+delta_y

bb_box<-c(T_West, T_North, T_East, T_South)
# subset graph based on coordinates

nodes_to_remove<-V(G)[which((V(G)$x < bb_box[1] | V(G)$x > bb_box[3]) | (V(G)$y > bb_box[2] | V(G)$y < bb_box[4]))]$name
g_s<-delete.vertices(G, nodes_to_remove)

# This must have generated several disconnected components

components(g_s)$no

# get largest connected component

gclust<-components(g_s, mode='weak')
largestConnectedComponent<-induced.subgraph(g_s, V(g_s)[which(gclust$membership == which.max(gclust$csize))])
G_s=largestConnectedComponent
plot(G_s,vertex.label=NA,vertex.size=.01)

# get degree distribution of vertex

V(G_s)$deg<-degree(G_s)
deg<-as.data.frame(cbind(V(G_s)$name,V(G_s)$deg))
deg2<-as.character(deg[which(deg$V2==2),1])

# create for loop to remove degree 2 nodes and reconnect edges

g_clean<-G_s
for(vrtx in c(1:length(deg2))){
  #print(deg2[vrtx])
  #get edge list to find which vertex degree two connect
  edge_list<-data.frame(cbind(get.edgelist(g_clean),E(g_clean)$weight))
  edge_list_2<-edge_list[which(edge_list$X1 %in% deg2[vrtx]),]
  edge_list_3<-edge_list[which(edge_list$X2 %in% deg2[vrtx]),]
  edge_list_3[,c("X1","X2")]<-edge_list_3[,c("X2","X1")]
  edge_list_2<-rbind(edge_list_2, edge_list_3)
  
  #add distances
  distance_list<-cbind(V=deg2[vrtx],D=sum(as.numeric(as.character(edge_list_2[,3]))))
  
  #edges
  edg<-c(as.character(edge_list_2[1,2]),as.character(edge_list_2[2,2]))
  
  #remove vertex
  g_clean<-delete.vertices(g_clean,deg2[vrtx])
  
  #add edges and distance
  g_clean<-add.edges(g_clean, edg, weight=sum(as.numeric(as.character(edge_list_2[,3]))))
}

# compute a closeness centrality and plot the network
closeness_g_clean=closeness.estimate(g_clean,cutoff=1000)

plot(G_s, vertex.shape = 'none', vertex.label=NA, vertex.size = 1)
plot(g_clean,vertex.size=2,vertex.color=getHSVColors(closeness_g_clean),vertex.label=NA, add=TRUE, edge.color = NA)

# Note that we cheated in the plot, in the sense that when removing nodes of degree 2 we also removed the streets 
# that form “elbows”. In order to have then the right morphology, you will see that we plotted the network before 
# cleaning without the vertices “G_s”, and then we put on top the vertices of the clean one “g_clean”.


# check that cleaning was done as expected
degree_g_clean=degree(g_clean)
head(degree_g_clean)

freq_degree=table(degree_g_clean)
plot(freq_degree)

# As expected, all the nodes of degree 2 have been removed.


