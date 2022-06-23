#######################################
############SICSS-NDSU 2022############
#######################################

##      Day 4 Network analysis       ##
##      Part 2: Twitter network      ## 
##       Standard API, rtweet        ## 
##        Author: Shuning Lu       ####

setwd("~/Desktop")

library(rtweet)
library(igraph)
library(readr)

#set up your Twitter bearer
bearer_token <- read_file("bearer_token1.txt")


#1. retweet and mention network
#build your query
sicss <- search_tweets(q = "sicss2022", #keyword
                       n = 500,         #number of tweets
                       include_rts = TRUE) #include reweets

#inspect your collected data
head(sicss)
dim(sicss)
colnames(sicss)

#create an igraph network object
sicss.net <- network_graph(sicss, c("mention, retweet, reply")) #using all kinds of ties
sicss.net

#try a retweet network, net <- network_graph(sicss, c("retweet"))

#who are in the network?
V(sicss.net)$name
V(sicss.net)$name[2]

#edge type in the network
E(sicss.net)$type 

#remove self-loops from the network
sicss.net <- simplify(sicss.net, remove.multiple = FALSE, remove.loops = TRUE) 

#descriptive statistics
vcount(sicss.net) #number of nodes
ecount(sicss.net) #number of edges
degree(sicss.net) #edges each node has


library(tidyverse) #please load the package only after removing the self-loops
#use detach("package:tidyverse", unload=TRUE) if you want to go back to simplify the network

#get the top 10 users in terms of degree
degree<-degree(sicss.net)
as.data.frame(degree)%>%arrange(desc(degree))%>%head(10)

#top 10 nodes that receive mentions, replies and retweet
in_degree <- degree(sicss.net, mode = "in")
as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%head(10)

#top 10 nodes that mention, reply and retweet others
out_degree <- degree(sicss.net, mode = "out")
as.data.frame(out_degree)%>%arrange(desc(out_degree))%>%head(10)

#top 10 nodes with closeness - spread information quickly
cl_degree <- closeness(sicss.net, mode = "out")
as.data.frame(cl_degree)%>%arrange(desc(cl_degree))%>%head(10)

#top 10 nodes with betweenness - bridge that links different communities
bt_degree <- betweenness(sicss.net, directed = TRUE)
as.data.frame(bt_degree)%>%arrange(desc(bt_degree))%>%head(10)

#top 10 nodes with eigenvector centrality - influence over others
eigen_degree <- eigen_centrality(sicss.net, directed = TRUE)$vector
as.data.frame(eigen_degree)%>%arrange(desc(eigen_degree))%>%head(10)

#comparing centrality scores
centralities <- cbind(in_degree, out_degree, cl_degree, bt_degree, eigen_degree)
round(cor(centralities), 3)


#network attributes
mean_distance(sicss.net) #average path length
graph.density(sicss.net) #how connected this network is
diameter(sicss.net, weights = NA) #netowrk size
reciprocity(sicss.net) #ratio of reciprocated edges  
transitivity(sicss.net, type="global") #ratio of triad

#finding cliques (only for undirected network)
clique_num(sicss.net)
largest_cliques(sicss.net)


#plot
plot(sicss.net)


#let's plot, again!
plot(sicss.net, 
     edge.color = "grey", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = 4, vertex.shape = "circle",    #vertex attributes
     vertex.color = "darkblue", vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA, #label
     layout=layout_with_fr) #force-directed layout algorithm

plot(sicss.net, 
     edge.color = "grey", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.2,#arrow attributes
     vertex.size = 4, vertex.shape = "circle",    #vertex attributes
     vertex.color = "tomato", vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA, #label
     layout = layout_with_kk) #stress-minimization layout algorithm

#adding node size based on indegree
V(sicss.net)$screen_name <- sicss.net$screen_name
plot(sicss.net, 
     edge.color = "grey", edge.width = 0.6, edge.curved = 0.4, #edge attributes
     edge.arrow.mode=1, edge.arrow.size = 0.1,#arrow attributes
     vertex.size = log(in_degree+8)*1.5, vertex.shape = "circle",    #vertex attributes
     vertex.color = "skyblue", vertex.frame.color = "white",     #vertex attributes
     vertex.label = ifelse(in_degree > 30, sicss$screen_name, NA), #only show top nodes
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, vertex.label.degree = -pi/2,
     layout=layout_with_fr) #stress-minimization layout algorithm

#adding node size and label based on betweenness centrality, try that out!

#community detection
Isolated <- which(degree(sicss.net) == 0) #find isolates
sicss.net <- delete.vertices(sicss.net, Isolated) #remove isolates
cluster_walktrap(sicss.net) #walk for 4 edges by default, 18 groups, mod:0.79
cluster_walktrap(sicss.net)[1]
cluster_walktrap(sicss.net)[2]

#try different step of walk, edge = 11
cluster_walktrap(sicss.net, steps = 11) #15 groups, mod: 0.79
sicss.comm<-cluster_walktrap(sicss.net, steps = 11)
plot(sicss.comm, sicss.net, vertex.size=4, vertex.label=NA, 
     edge.arrow.mode=0, layout=layout_with_fr) 

sicss.net$community <- sicss.comm$membership #store membership 

#use the membership to replot
plot(sicss.net, 
     edge.color = "grey", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.2,#arrow attributes
     vertex.size = log(10+bt_degree), vertex.shape = "circle",    #vertex attributes
     vertex.color = membership(sicss.comm), vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA, #label
     layout = layout_with_kk) #stress-minimization layout algorithm


#other community detection algorithms for undirected network
#https://igraph.org/r/doc/cluster_edge_betweenness.html
sicss.comm1 <- cluster_edge_betweenness(as.undirected(sicss.net)) 
plot(sicss.comm1, sicss.net, vertex.size=4, vertex.label=NA, 
     edge.arrow.mode=0, layout=layout_with_fr) 

#https://igraph.org/r/doc/cluster_fast_greedy.html
sicss.comm2 <- cluster_edge_betweenness(as.undirected(sicss.net)) 
plot(sicss.comm2, sicss.net, vertex.size=4, vertex.label=NA, 
     edge.arrow.mode=0, layout=layout_with_fr) 


#subsetting network
#subset networkgraph
ndsu <- c("sicss_ndsu", "zoltanmajdik", "shuning_lu", "NDSUCOMMDept", "rossfcollins")
sicss.ndsu <- induced.subgraph(sicss.net, 
                               vids = ndsu)
plot(sicss.ndsu, 
     edge.color = "grey", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = 4, vertex.shape = "circle",    #vertex attributes
     vertex.color = "gold", vertex.frame.color = "white",     #vertex attributes
     layout=layout_with_fr) #force-directed layout 

