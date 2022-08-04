#Context
#We collected tweets pertaining to COVID-19 vaccines from 1/21/20 to 1/28/21. We used Botometer to assign each account a score from 0 (a human-like account) to 1(a bot-like account). -----------------------Social Network Analysis (Combined)\ -----------------------------
#We used 0.5 as the threshold. a score < 0.5, human-like; a score >= 0.5: bot-like

#-------------social network analysis (combined)------------------------------------------------
# The dataset we used, named "covid_retweet_cleaned.csv," includes both human-like and bot-like tweets. 

library(rtweet)
library(igraph)
library(readr)
library(tidyverse)
library(dplyr)
library(tidygraph)
library(lubridate) #for extracting dates
library(quanteda)
library(stm)
library(wordcloud)

covid_other = read.csv("covid_retweet_cleaned.csv")

edge_df <-
  covid_other %>% 
  select(user_screen_name, retweet_or_quote_screen_name) %>% #Step 1
  filter(retweet_or_quote_screen_name != "") %>% #step 3
  rename(from = user_screen_name,
         to = retweet_or_quote_screen_name) #Step 4


nodes_df <- data.frame(name = unique(c(edge_df$from,edge_df$to)),
                       stringsAsFactors = F)
node_overall = covid_other %>%
  select(user_screen_name, overall) %>% 
  mutate(name = user_screen_name, overall_score = overall) %>%
  select(name, overall_score) %>%
  distinct(name, .keep_all = TRUE)

graph_covid <- tbl_graph(nodes = nodes_df,
                         edges = edge_df,
                         directed = T)

graph_covid <- graph_covid %>% 
  activate(nodes) %>% 
  #mutate(community = group_louvain()) %>% # clustering
  activate(edges) %>% 
  filter(!edge_is_loop())
graph_covid = simplify_and_colorize(graph_covid)

#in_degree <- degree(graph_covid, mode = "in")
#out_degree <- degree(graph_covid, mode = "out")
#bt_degree <- betweenness(graph_covid, directed = TRUE)
#nodes_df$bt = bt_degree
#as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%tail(100)

# data about graphs ==================
degree<-degree(graph_covid)
as.data.frame(degree)%>%arrange(desc(degree))%>%head(10)

#top 10 nodes that receive mentions, replies and retweet
in_degree <- degree(graph_covid, mode = "in") #in
as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%head(10)

#top 10 nodes that mention, reply and retweet others
out_degree <- degree(graph_covid, mode = "out")
as.data.frame(out_degree)%>%arrange(desc(out_degree))%>%head(10)

#top 10 nodes with closeness - spread information quickly
cl_degree <- closeness(graph_covid, mode = "out") # out: mentioning others
as.data.frame(cl_degree)%>%arrange(desc(cl_degree))%>%head(10)

#top 10 nodes with betweenness - bridge that links different communities
bt_degree <- betweenness(graph_covid, directed = TRUE)
as.data.frame(bt_degree)%>%arrange(desc(bt_degree))%>%head(10)

#top 10 nodes with eigenvector centrality - influence over others
eigen_degree <- eigen_centrality(graph_covid, directed = TRUE)$vector #eigen degree is a list, so add $vector!
as.data.frame(eigen_degree)%>%arrange(desc(eigen_degree))%>%head(10)

mean_distance(graph_covid) #average path length, mean steps to take to get to any person in the network
graph.density(graph_covid) #how connected this network is, meaningful if compared with random graph
diameter(graph_covid, weights = NA) #network size, degrees of separation
reciprocity(graph_covid) #ratio of reciprocated edges, do ppl mention each other back etc?
transitivity(graph_covid, type="global") #ratio of triad

nodes_df = nodes_df %>%
  cbind(as.data.frame(in_degree), as.data.frame(out_degree), 
        as.data.frame(cl_degree), as.data.frame(bt_degree)) 

mean(bt_degree)
max(bt_degree)

nodes_df = nodes_df %>% select(1, 3:6)
nodes_df1 = nodes_df %>% select(1, 3:5)
nodes_df2 = nodes_df1 %>% select(1, 4)
x = merge(nodes_df, node_overall)

nodes_df2 %>% arrange(desc(nodes_df2$bt_degree)) %>% head(10)
x %>% arrange(desc(x$in_degree)) %>% head(10)

#==SNA visualization 

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(in_degree >= 10, log(in_degree)*2, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(out_degree >= 10, log(out_degree)*3, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(bt_degree >= 10, log(bt_degree)*3, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",#vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

#------------------------social network analysis (separate)----------------------------------
#Next, we split "covid_retweet_cleaned.csv" into two datasets: One is bot-like dataset(cleaned_bot.csv), the other is human-like dataset (cleaned_human.csv). 
#we did SNAs of these two datasets separately.
#We did these two SNAs on Gephi. 

#convert csv file to Gephi format==========
#=bot file=
cleaned_bot <- read.csv("cleaned_bot.csv")
tweets.df <- cleaned_bot
filter(tweets.df, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> net
summary(net)
write_graph(simplify(net),  "cleaned_bot.gml", format = "gml")
#=human file=
cleaned_human <- read.csv("cleaned_human.csv")
tweets.df <- cleaned_human
filter(tweets.df, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> net
summary(net)
write_graph(simplify(net),  "cleaned_human.gml", format = "gml")
#Find Gephi files in the folder

#---------------------------------topic modeling----------------------------------------------
library(lubridate) #for extracting dates
library(tidyverse)
library(quanteda)
library(stm)
library(RColorBrewer)
library(wordcloud)

covid <- read.csv("covid_retweet_cleaned.csv")

covid$tag[covid$overall > 0.5] <- 1
covid$tag[covid$overall <= 0.5] <- 0

covid <- covid%>%filter(lang == "en")

#clean tweets, you can tweak the codes below based on your research
clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove punctuation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove emoji
    str_remove_all('[:emoji:]')%>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}
covid$text<-covid$text %>% clean_tweets

stopWords2<-c("covid","vaccine","vaccines","vaccination","coronavirus","t","w","+","b","im")

#tokenization
covidCorpus <- corpus(covid$text)
covidCorpus <- covidCorpus %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english"), stopWords2))


#pass covariates to corpus, you may select other features
docvars(covidCorpus, field = "tag") <- covid$tag # covariance field "tag: bot or human"
#docvars(covidCorpus, field = "text") <- covid$text # covariance field "text"

#converting corpus to document frequency matrix
covidDfm <- dfm(covidCorpus) 
topfeatures(covidDfm, n = 100, scheme = "docfreq")

#prepare stm data
covid.stm <- convert(covidDfm, to = "stm")
out_covid <- prepDocuments(covid.stm$documents, 
                           covid.stm$vocab, 
                           covid.stm$meta, 
                           lower.thresh = 63) 

#Topic modeling
#How do we decide how many topics?
#1.Statistical fit
#2.Interpretability and relevance of topics

#Let's find the optimal K, this will take a long time, be prepared k = c(3:15)means choosing three to ten topics
set.seed(1234)
storage_covid <- searchK(out_covid$documents, out_covid$vocab, K = c(3:15),
                         data=out_covid$meta)
storage_covid

head(storage_covid)

#covid results
#    K   exclus    semcoh   heldout  residual     bound    lbound em.its
#1   3 8.258455  -166.187 -3.330766  0.978712 -328096.1 -328094.3     85
#2   4 8.479115 -166.9572 -3.322814 0.9487244   -327676 -327672.8     94
#3   5 8.490525 -164.3975 -3.309472 0.9002082 -327056.1 -327051.3     84
#4   6 8.492228 -148.2822 -3.286114 0.8540512 -326427.2 -326420.6     88
#5   7 8.493301 -143.3116 -3.245171 0.6774111 -324735.6 -324727.1     28
#6   8 8.494247 -143.6712 -3.256089 0.8662076 -325257.9 -325247.3    143
#7   9 8.497743 -142.6335 -3.239547 0.8664151 -324293.7 -324280.9    152
#8  10 8.496136 -141.2843 -3.230039 0.8769002 -324069.1   -324054    169
#9  11 8.499024 -143.9184 -3.231187 0.9144445 -324139.7 -324122.2    189
#10 12 8.499124 -146.0283 -3.225337 0.9356447 -323739.3 -323719.3    188
#11 13 8.499239 -144.0831 -3.224736 0.9627611 -323706.6 -323684.1    219
#12 14 8.497184 -147.6639 -3.273889 0.7722756 -324118.4 -324093.2     36
#13 15 8.498994 -148.6727 -3.256521 0.7990148 -324210.8 -324182.9     37

#The results indicate 10-topic solution could work, because
#both exclusivity coefficient and semantic coherence coefficient are high

#Now, let's run the 10-topic model  
model_covid <- stm(out_covid$documents, out_covid$vocab,
                   K = 10, 
                   max.em.its = 188, #its number should follow searchK output
                   data = out_covid$meta, 
                   init.type = "Spectral") 

#words used in topics
labelTopics(model_covid, c(1:14), frexweight = 0.5,n = 15) 

#Let's focus on FREX
#It shows words that are comparatively common and exclusive for a topic compared to others
labels <- labelTopics(model_covid, n = 20)
topwords <- data.frame("features" = t(labels$frex))
colnames(topwords) <- paste("Topics", c(1:14))
topwords[1:14]

#plot topics
par(bty="n",col="grey40",lwd=6)
plot(model_covid, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: ???????",
                     "Topic 2: ???????",
                     "Topic 3: ???????",
                     "Topic 4: ???????",
                     "Topic 5: ???????",
                     "Topic 6: ???????",
                     "Topic 7: ???????",
                     "Topic 8: ???????",
                     "Topic 9: ???????",
                     "Topic 10: ???????",
                     "Topic 11: ???????",
                     "Topic 12: ???????",
                     "Topic 13: ???????",
                     "Topic 14: ???????"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

#wordcloud
cloud(model_covid, topic = 1, min.freq=3)
cloud(model_covid, topic = 2, min.freq=3)

#plot two topics and compare
plot(model_covid, 
     type="perspectives", 
     topics=c(3,5), 
     plabels = c("Topic 3","Topic 5"))

#Structural topic modeling
#introducing document-level data into a regression model to predict topic

#tag as covariate to know whether human-like and bot-like tweets discussed about topics differently
result1 <- estimateEffect(1:5 ~ tag, model_covid,
                          meta = out_covid$meta, uncertainty = "Global")
topiceffect1<-summary(result1, topics = c(1:5))
topiceffect1

#you can save the result: write.table(topiceffect1$tables, 'output.csv', sep=",")



