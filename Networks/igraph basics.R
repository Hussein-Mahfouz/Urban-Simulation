library(igraph)
library(lattice)
library(Matrix)

# 1. Construct Network
g1 <- graph( c( 1,2, 2,3, 2,4, 3,4, 3,5, 1,5, 3,6), directed=FALSE)
plot(g1)

# 2. Extract edgelist from graph

#get proper edge list
get.edgelist(g1)

#get the vertex list in a specifc order
get.vertex.attribute(g1) #You can also extract the nodes of network.

#get adjacency matrix (which verices are connected)
adj_A1=get.adjacency(g1)
adj_A1

# Visualize

# Here we can also see how a network can be represented in different visulisations styles

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# -1 used to include first element 'layout'. Probably this is at index 0

#grep, grepl, regexpr, gregexpr and regexec search for matches to argument pattern within each element of a character vector: they differ in the format of and amount of detail in the results.
# you can use View(layouts) to check how many styles igraph has been embeded here.
# ls and objects return a vector of character strings giving the names of the objects in the specified environment. 
# think about why we add [-1] in the end of this line.
layouts <- layouts[!grepl("bipartite|sugiyama", layouts)]
#grepl returns TRUE if a string contains the pattern, otherwise FALSE; if the parameter is a string vector, returns a logical vector (match or not for each element of the vector;
# ! indicates logical negation (NOT). see other logical operators here: https://stat.ethz.ch/R-manual/R-devel/library/base/html/Logic.html
par(mfrow=c(5,4), mai=c(.1,.1,.1,.1)) #set graphic parameters_5 rows and 3 columns

# print layout names and plot them as facet plot
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(g1)) 
  plot(g1, edge.arrow.mode=0, layout=l, vertex.size=20, vertex.colour="orange") }

# 4. Calculate Basic Network Characteristics

# degree is the number of connections for each vertex
degree(g1, v= V(g1))

# The diameter of a graph is the lgreatest distance between two vertices
diameter (g1) 

# use the size of the node to indicate their degree?
plot(g1, vertex.size=10*degree(g1,V(g1)), vertex.colour="orange")


# Convert graph to dataframe
g <- as_data_frame(g1)


