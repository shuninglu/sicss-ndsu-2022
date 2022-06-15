#######################################
############SICSS-NDSU 2022############
#######################################

##      Day 4 Network analysis       ##
##  Part 1: Network analysis basics  ## 
##        Author: Shuning Lu       ####


#install and load packages::igraph, igraphdata

#load package
library(igraph)
library(igraphdata)
library(tidyverse)

#0. Create your first ten network graphs from scratch
#an undirected network with three animals
g1 <- graph( edges=c("Cat","Dog", "Cat","Rabbit", "Rabbit","Dog"),  directed=F ) 
plot(g1)

#a directed network with three animals
g2 <- graph( c("Cat","Dog", "Cat","Rabbit", "Rabbit","Dog"))
plot(g2)

#a directed network with four animals
g3 <- graph( edges=c("Cat","Dog", "Cat","Rabbit", "Rabbit","Dog", "Dog","Lahma", "Cat","Lahma"),  directed=T ) 
plot(g3)

#a directed network with four animals and four isolates 
g4 <- graph( edges=c("Cat","Dog", "Cat","Rabbit", "Rabbit","Dog", "Dog","Lahma", "Cat","Lahma"), 
             isolates=c("Lion", "Bear", "Tiger", "Wolf"), directed=T ) 
plot(g4)

#an empty graph with 10 nodes
g5 <- make_empty_graph(10)
plot(g5)

#a full graph with 10 nodes
g6 <- make_full_graph(10)
plot(g6)

#a star graph with 10 nodes
g7 <- make_star(10)
plot(g7, edge.size = 1, vertex.label = NA) 

#a tree graph with 10 nodes
g8 <- make_tree(10, children = 2)
plot(g8, edge.size = 1, vertex.label = NA) 

#a ring graph with 10 nodes
g9 <- make_ring(10)
plot(g9, edge.size = 1, vertex.label = NA) 

#a random graph with 10 nodes
g10 <- sample_gnm(n = 10, m = 10) 
plot(g10, vertex.size = 6, vertex.label = NA)  


#1. Load buit-in data, Zachary’s Karate Club network
data(karate)

#insepct data
class(karate) #data format
vcount(karate) #how many nodes?
ecount(karate) #how many edges?
V(karate) #a list of nodes
V(karate)[1:5] #first five nodes
E(karate) #a list of edges
E(karate)[5] #the fifth edge
vertex.attributes(karate) #node attributes
edge.attributes(karate) #edge attributes

#Descriptive statistics
k_degree <- degree(karate) #degree centrality
k_degree
hist(k_degree)
which.max(k_degree)
which.min(k_degree)

k_close <- closeness(karate) #closeness centrality
k_close <- round(k_close, 3) #round up into 3rd decimal place
k_close
sort(k_close) %>% #get top 5 nodes with closeness
  tail(5)

betweenness(karate)#betweenness centrality
eigen_centrality(karate) #eigenvector centrality

mean_distance(karate) #average path length
distances(karate) #all shortest paths between nodes w/ edge weights
distances(karate, weights = NA) ##all shortest paths between nodes w/o edge weights
neighbors(karate, "Actor 33") #Actor 33's neighbors

graph.density(karate,loop=FALSE) #network density
diameter(karate, weights = NA) #netowrk size
reciprocity(karate) # ratio of reciprocated edges *karate is an undirected network
transitivity(karate, type="global") #ratio of triad

#take a glance
plot(karate)

#finding cliques
clique_num(karate)
largest_cliques(karate)

#community dectection
karate.betw <- cluster_edge_betweenness(karate)
?cluster_edge_betweenness
karate.betw$modularity
dendPlot(karate.betw, mode="hclust")
plot(karate.betw,
     karate)

karate.cfg <- cluster_fast_greedy(karate)
?cluster_fast_greedy
karate.cfg$modularity
dendPlot(karate.cfg, mode="hclust")
plot(karate.cfg,
     karate)

V(karate)$community <- karate.cfg$membership
colrs <- adjustcolor( c("gray50", "blue", "green"), alpha=.6)
plot(karate, vertex.color=colrs[V(karate)$community])

#k-core decomposition
karate.kc <- coreness(karate, mode="all")
colrs <- adjustcolor( c("gray50", "tomato", "gold", "blue"), alpha=.6)
plot(karate, 
     vertex.size = karate.kc*6, 
     vertex.label = karate.kc, 
     vertex.color = colrs[karate.kc])


#2. Read data from files
#import gml data
polblog <- read.graph("polblogs.gml",format = c("gml"))

#for edgelist and nodelist, you can read them as follows
#nodes <- read.csv("nodelist filename", header=T, as.is=T)
#links <- read.csv("edgelist filename", header=T, as.is=T)
#polblog <- graph_from_data_frame(d=links, vertices=nodes, directed=T) #convert it into network object

#inspect data 
class(polblog)  
vcount(polblog)  
ecount(polblog)  
V(polblog) 
V(polblog)$label  
E(polblog)  

#destriptive statistics
#for directed network, you need to know indegre, outdegree, and total degree 
p_outdegree <- degree(polblog, mode = "out") #outdegree centrality
p_indegree <- degree(polblog, mode = "in") #indegree centrality
p_alldegree <- degree(polblog, mode = "all") #all degree centrality
#identify the top 5 political blogs with inward links
#identify the bottom 5 political blogs with inward links

#explore other descriptive statistis using code from #1

#plotting and fine-tuning
#plot the graph
plot(polblog)

#clean up plot
dev.off()

#remove self-loops
polblog <- simplify(polblog, remove.multiple = F, remove.loops = T) 

#remove isolates
Isolated <- which(degree(polblog) == 0)
polblog2 <- delete.vertices(polblog, Isolated)

#assign colors to blogs based on political leanings
V(polblog2)$color <- ifelse(V(polblog2)$value == 1, "red", "blue")

#plog again
plot(polblog2, 
     main = paste("Political Blog Network"), 
     usearrows = TRUE, edge.col = "grey50", edge.width = 0.25, 
     vertex.size = 3,vertex.col = colors,edge.arrow.mode = 0, 
     vertex.label = NA, layout = layout_with_kk)

#change layout algorithms
l1 <- layout_with_graphopt(polblog2, charge=0.02)
plot(polblog2, 
     main = paste("Political Blog Network"), 
     usearrows = TRUE, edge.col = "grey50", edge.width = 0.25, 
     vertex.size = 3,vertex.col = colors,edge.arrow.mode = 0, 
     vertex.label = NA, layout = l1)

#set node size based on indgree 
indeg <- degree(polblog2, mode="in")
plot(polblog2, 
     main = paste("Political Blog Network"), 
     usearrows = TRUE, edge.col = "grey50", edge.width = 0.25, 
     vertex.size = indeg*0.03,vertex.col = colors,edge.arrow.mode = 0, 
     vertex.label = NA, layout = l1)

#label top blogs
sort(indeg) %>% #get top 2 nodes  
  tail(2)
plot(polblog2, 
     main = paste("Political Blog Network"), 
     usearrows = TRUE, edge.col = "grey50", edge.width = 0.25, 
     vertex.size = indeg*0.03,vertex.col = colors,edge.arrow.mode = 0, 
     vertex.label = ifelse(degree(polblog2, mode = "in") > 270, V(polblog2)$label, NA),
     vertex.label.dist = 2, vertex.label.color = "yellow", vertex.label.degree = -pi/2,
     layout = l1)

#subset networkgraph
p_lib <- induced.subgraph(polblog2, 
                          V(polblog2)[value %in% c("0") ])
plot(p_lib, 
     main = paste("Liberal Political Blog Network"), 
     usearrows = TRUE, edge.col = "grey50", edge.width = 0.25, 
     vertex.size = 3, vertex.col = "blue", edge.arrow.mode = 0, 
     vertex.label = NA)

#how to subset conservative blog network?
#compare the two subnetworks in terms of network attributes
