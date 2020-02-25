# Supporting material for Lecture on community detection in networks
# This code runs through some community detection algorithms
# Use Zachary network on friendships in a Karate club

library(igraph)

#----------------------- Karate club example --------------
#This example is the most widely used for community detection
#the network: friendship between members of a karate club, collected by Zachary in 1977
dir_karate <- "CommunityDetection/karate/"
file_data <- paste(dir_karate,'karate.gml',sep='')

#this graph is also part of the library of the famous networks
  #the graph can be obtained directly from iGraph as follows
  #g_k=graph.famous("Zachary")

#let us get the graph from the directory
g_karate=read.graph(file_data, format = "gml")
#let us plot it
plot(g_karate,vertex.label=NA)
#let us now produce a plot such that the nodes in the plot have a size related to their degree
V(g_karate)$size=degree(g_karate)
plot(g_karate,vertex.label=NA)
#title('Karate club friendship network \nZachary 1977')
#if want title fonts different in different lines
title('Karate club friendship network', cex.main = 1.25,line=2)
title('Zachary 1977', cex.main = 1, line=1)

#---------------Community detection -------------
#------------
#Girvan-Newman algorithm
#------------
	alg_name='Girvan-Newman'
	GN_comm <- edge.betweenness.community(g_karate)
	#look at result of algorithm
	print(GN_comm)
	#the process aims at maximising the modularity
	modularity(GN_comm)
	# number of communities and their sizes
	nc=length(GN_comm)
	sizes(GN_comm)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(GN_comm)
	#visualise the results
	plot(GN_comm,g_karate,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	#The division depends on where in the hierarchical tree we put the cut.
	#This methodology relies on cutting the tree in order to maximise modularity
	#Let us look at this through the dendrogram
	dendPlot(GN_comm,use.modularity = TRUE)
	title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))
	#---------------
# !!!!!! This method is based on computing betweenness in an iterative way, and hence it is extremely slow for large networks. Try to see how long it takes for the bikes example!

#------------
#Fast greedy modularity optimisation: Clauset-Newman-Moore
#------------
	alg_name='Clauset-Newman-Moore'
	greedy_c <- fastgreedy.community(g_karate)
	#look at result of algorithm
	print(greedy_c)
	#the process aims at maximising the modularity
	modularity(greedy_c)
	# number of communities and their sizes
	nc=length(greedy_c)
	sizes(greedy_c)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(greedy_c)
	#visualise the results
	plot(greedy_c,g_karate)#,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	#The division depends on where in the hierarchical tree we put the cut.
	#This methodology relies on cutting the tree in order to maximise modularity
	#Let us look at this through the dendrogram
	dendPlot(greedy_c,use.modularity = TRUE)
	title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

#------------
#Community strucure via short random walks: Pons&Latapy
#------------
	alg_name='Random walks'
	wtrap_c <- walktrap.community(g_karate)
	comm=wtrap_c
	#look at result of algorithm
	print(comm)
	#the process aims at maximising the modularity
	modularity(comm)
	# number of communities and their sizes
	nc=length(comm)
	sizes(comm)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(comm)
	#visualise the results
	plot(comm,g_karate)#,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	#The division depends on where in the hierarchical tree we put the cut.
	#This methodology relies on cutting the tree in order to maximise modularity
	#Let us look at this through the dendrogram
	dendPlot(comm,use.modularity = TRUE)
	title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

#------------
#Leading eigenvector: Newman spectral approach
#------------
	alg_name='Spectral'
	spectral_c <- leading.eigenvector.community(g_karate)
	comm=spectral_c
	#look at result of algorithm
	print(comm)
	#the process aims at maximising the modularity
	modularity(comm)
	# number of communities and their sizes
	nc=length(comm)
	sizes(comm)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(comm)
	#visualise the results
	plot(comm,g_karate)#,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	#Doesn't get a dendrogram
#------------
#Louvain method: Blondel et al, modularity optimization
#------------
	alg_name='Louvain'
	louv_c <- multilevel.community(g_karate)
	comm=louv_c
	#look at result of algorithm
	print(comm)
	#the process aims at maximising the modularity
	modularity(comm)
	# number of communities and their sizes
	nc=length(comm)
	sizes(comm)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(comm)
	#visualise the results
	plot(comm,g_karate)#,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	
#------------
#Infomap method: Rosvall and Bergstrom
#------------
	alg_name='Infomap'
	info_c <- infomap.community(g_karate)
	comm=info_c
	#look at result of algorithm
	print(comm)
	#the process aims at maximising the modularity
	modularity(comm)
	# number of communities and their sizes
	nc=length(comm)
	sizes(comm)
	
	#if you need the membership vector only
	V(g_karate)
	mem_vec=membership(comm)
	#visualise the results
	plot(comm,g_karate)#,layout=layout.fruchterman.reingold)
	title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))
	
	