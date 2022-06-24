#######################################
############SICSS-NDSU 2022############
#######################################

##      Day 4 Network analysis       ##
##      Part 2: Twitter network      ## 
##        Author: Shuning Lu       ####

library(academictwitteR)
library(tidyverse)
library(igraph)
library(readr)


#set up your Twitter bearer
bearer_token <- read_file("bearer_token.txt")


#CONSTRUCT A RETWEET NETWORK BASED ON DATA COLLECTED ON DAY 2 

#1.Bind your data in the path
#data<-bind_tweets(data_path = "data", output_format = "tidy")

#2.find user names in retweets
#re_user <- data$sourcetweet_author_id
#re_user<-na.omit(re_user)
#re_profile <- get_user_profile(re_user, bearer_token)
#names(re_profile) #take a look at the features

#3.create vertex list for retweet network
#re_profile2 <- re_profile %>% select(name, id)
#ori_profile <- data %>% select(user_name, author_id)
#ori_profile <- rename(ori_profile,name = user_name)
#ori_profile <- rename(ori_profile,id = author_id)
#node <- unique(rbind(re_profile2, ori_profile))

#4.create edge list for retweet network
#data$is_retweet <- ifelse(grepl("RT @", data$text, ignore.case = T), "1","0" )
#sp <- split(data, data$is_retweet)
#rt <- mutate(sp[["1"]], from = sourcetweet_author_id)
#edge <- as.data.frame(cbind(from = rt$from, to = rt$author_id))

#5.create network object
#retweet_graph <- graph.data.frame(d = edge,  directed = T)
#retweet_graph<-set_vertex_attr(retweet_graph, "username", index = V(retweet_graph), node$name)


#READ EXISTING RETWEET DATA COLLECTED BY THE INSTRUCTOR 
#retweet_graph <- readRDS("retweet_graph.rds")


#Let's start!
#remove self-loops
retweet_graph <- simplify(retweet_graph, remove.multiple = F, remove.loops = T) 

#descriptive statistics
vcount(retweet_graph)
ecount(retweet_graph)
degree(retweet_graph, mode = "out")
degree(retweet_graph, mode = "in")

#finding top nodes
# get the top 10 users in terms of degree
degree<-degree(retweet_graph)
V(retweet_graph)$username%>%as.data.frame(degree)%>%arrange(desc(degree))%>%head(10)
# get the top 10 users in terms of indegree
in_degree<-degree(retweet_graph, mode=c("in"))
V(retweet_graph)$username%>%as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%head(10)

#top users with highest outdegrees?

#Please explore other network attributes.


#plot the retweet network
plot(retweet_graph, edge.color="gray70", edge.width=1, edge.curved=0, 
     edge.arrow.mode=0, vertex.size=4, vertex.shape="circle",
     vertex.color="blue", vertex.frame.color="white",
     vertex.label=NA, layout=layout_with_kk) 

# Community detection
retweet_comm <- cluster_walktrap(retweet_graph) 

#modularity
modularity(retweet_comm) #>0: edge number within groups exceeds the number expected based on chance

#plot community
plot(retweet_comm, retweet_graph, vertex.size=4, vertex.label=NA, 
     edge.arrow.mode=0, layout=layout_with_kk) 

unique(retweet_comm$membership) #way too many communities in this network 
