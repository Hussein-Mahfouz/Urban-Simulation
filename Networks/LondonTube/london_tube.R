#This code will enable you to convert the tube network in the form of a shapefile into a network. 
#It will also show you how to download a network in a csv or txt format.
#before getting started, you need to install the following packages
# install.packages("maptools")
#install.packages("ggplot2")
#install.packages("broom")
# install.packages("rgeos")
# If this causes trouble go to 
# http://tlocoh.r-forge.r-project.org/mac_rgeos_rgdal.html
# and follow the instructions
#-------------------------

library(igraph)
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(broom)

#if you open R or RStudio from the applications set your working directory to where you have your data
#setwd("/Users/myUserName/directory/dataFolder")

#We will use the files later on, but so that we know what we need, let us name all our files now
file_network <- "LondonTube/london_network.csv"
file_stations <- "LondonTube/london_stations.csv"

#---------------
#OK, leet us start with the shape file for London's underground
dir_tube <- "LondonTube/underground"
shapeFile <- readOGR(dsn = dir_tube, "underground")
data.shapeFile=shapeFile@data
head(shapeFile@lines)
head(shapeFile@data)

#rename it
dat_sF=data.shapeFile

#if want to know what is the maximum distance between stations
max(dat_sF$distance)

#if want to know which are the stations further away from each other
dat_sF[which.max(dat_sF$distance),]

#Sherlock, I observe in the data pairs of nodes appearing multiple times, why?
#e.g. node 11
dat_sF[which(dat_sF$station_1==11),]
#--> Oh, two stations can be connected through different lines in the underground

#What if instead I wanted to find which node is Baker Street, Sherlock?
dat_sF[which(dat_sF$station_1_=='Baker Street'),]

#But Sherlock, we are not sure there's always a space in the way people write it!!!!
dat_sF[grep("^Baker",dat_sF$station_1_),]

shapeFile.f=fortify(shapeFile)
head(shapeFile.f)
#what is "fortify" Sherlock?
# Oh Watson, ask R instead of me, don't you see I'm trying to sleep!
# and Watson typed: 
?fortify    
#in the console, only to discover it's an old trick from Sherlock to cover the dirt!
# No Sherlock, Mrs Hudson will be upset, I will use the broom package instead!!!
shapeFile.b=tidy(shapeFile)
head(shapeFile.b)

#let us now get the information from the shapefile
#First let us merge the two datasets: the connectivity between stations and the spatial information
data.shapeFile=data.frame(dat_sF,id=as.numeric(as.character(dat_sF$toid_seq))-1)
mergedSegments=merge(shapeFile.f,data.shapeFile,by.x="id",by.y="id")

#we can then obtain an id for a station, its name and coordinates
firstStation= mergedSegments[mergedSegments$order==1,]
firstStation=data.frame(id=firstStation$station_1,name=firstStation$station_1_,long=firstStation$long,lat=firstStation$lat)
secondStation=mergedSegments[mergedSegments$order==2,]
secondStation =data.frame(id= secondStation $station_2,name= secondStation $station_2_,long= secondStation $long,lat= secondStation $lat)

stations=rbind(firstStation, secondStation)
stations=stations[!duplicated(stations$id),]

#let us construct the network 
node1=dat_sF$station_1
node2=dat_sF$station_2
weight=dat_sF$distance
df=data.frame(node1,node2,weight)

g_tube=graph.data.frame(df,directed=FALSE)

#we can create the files containing the network as follows
write.csv(df,"LondonTube/tube_network.csv",row.names = FALSE)

#-----------------------------
#Let us now see what happens if we are given a network

#in order to import data, have a look at the following link:
#http://www.r-tutor.com/r-introduction/data-frame/data-import

#read and get network
dat=read.csv(file_network,header=TRUE)
g_london=graph.data.frame(dat,directed=FALSE)

#are the two networks identitcal?
identical_graphs(g_london,g_tube)

#did we do something wrong? -->names of columns different
station_1=node1
station_2=node2
distance=weight
df2=data.frame(station_1,station_2,distance)

g_tube2=graph.data.frame(df2,directed=FALSE)

identical_graphs(g_london,g_tube2)
#now it works
#let us plot it
plot(g_tube)

#can't see anything 
plot(g_tube,vertex.size=3,vertex.label.cex=.5,vertex.color="white")
#better but still...

#need to assign distance to weight
E(g_london)$weight=df2$distance

#number of stations
n_stations=length(V(g_london))
n_stations
# No of edges
m= length(E(g_london))
m

# Number of islands or disconnected components
clusters(g_london)$no

#diameter
d_max <- diameter(g_london) #this gives me the max n. if nt weighted
d_max
d_max_unw <- diameter(g_london,weights=NA)
d_max_unw

#Compute betweenness
bet_london=betweenness(g_london, v=V(g_london), directed = F, normalized = FALSE)
# or just betweenness(g_london)

palette=hsv(h=1-((bet_london/max(bet_london)*2/3)+1/3),s = 1,v=1)
radius_vertex=3
width_lines=6

#we can plot the graph using the measure of betweenness
plot(g_london,vertex.size=bet_london/max(bet_london)*radius_vertex+.01,vertex.color=palette,vertex.label.cex=.1)


#read the name of the stations
info_names=dat=read.csv(file_stations,header=TRUE)
#first match names of nodes to id field
vect_p=match(V(g_london)$name,info_names$id)
#get real names for stands
v_names=info_names$station_name[vect_p]
#assign names to network
g_london <- set.vertex.attribute(g_london, "name", value=as.character(v_names))

vect_pos=match(V(g_london)$name,as.character(stations$name))
#get real names for stands
v_pos=stations[vect_pos,]

g_london <- set.vertex.attribute(g_london, "x", value=v_pos$long)
g_london <- set.vertex.attribute(g_london, "y", value=v_pos$lat)
plot(g_london,vertex.size=3,vertex.label.cex=.5,vertex.color="white")

#and using all the information:
plot(g_london,vertex.size=bet_london/max(bet_london)*radius_vertex+.01,vertex.color=palette,vertex.label.cex=.1)


#when there are many lines connecting pairs, we get multiple entries.
#We can simplify the network as follows:

g_london=simplify(g_london,remove.loops = T,remove.multiple = T,edge.attr.comb = "min")

plot(g_london,vertex.color=palette,vertex.size=bet_london/max(bet_london)*radius_vertex+.01,vertex.label.cex=.1)

#if we want to calculate topological betweenness (not using the weights) we set the weights to NA
bet_london=betweenness(g_london, v=V(g_london), directed = F, normalized = FALSE, weights = NA)
palette=hsv(h=1-((bet_london/max(bet_london)*2/3)+1/3),s = 1,v=1)
plot(g_london,vertex.color=palette,vertex.size=bet_london/max(bet_london)*radius_vertex+.01,vertex.label.cex=.1)

#edge betweenness

edge_bet_london=edge.betweenness(g_london)

palette_edges=hsv(h=1-((edge_bet_london/max(edge_bet_london)*2/3)+1/3),s = 1,v=1)

plot(g_london,vertex.size=.1,vertex.label.cex=.1,edge.width=edge_bet_london/max(edge_bet_london)*width_lines,edge.color=palette_edges)


#closeness

clos_london=closeness(g_london)
normalised_clos_london=(clos_london-min(clos_london))/(max(clos_london)-min(clos_london))
palette=hsv(h=1-((normalised_clos_london*2/3)+1/3),s = 1,v=1)
plot(g_london,vertex.color=palette,vertex.size=normalised_clos_london*radius_vertex+.01,vertex.label.cex=.1)

#topological closeness
clos_london=closeness(g_london,weights = NA)
normalised_clos_london=(clos_london-min(clos_london))/(max(clos_london)-min(clos_london))
palette=hsv(h=1-((normalised_clos_london*2/3)+1/3),s = 1,v=1)
plot(g_london,vertex.color=palette,vertex.size=normalised_clos_london*radius_vertex+.01,vertex.label.cex=.1)


# Exercise:
#   
# 1) Remove nodes according to a selected property: e.g. degree, closeness centrality and betweenness centrality
# 
# 2) think about the effects on the network of removing such nodes. Which have more impact on the network?

#----------------
#to remove nodes
g1=delete.vertices(g_london, c("Baker Street","Embankment"))
V(g_london)
V(g1)
