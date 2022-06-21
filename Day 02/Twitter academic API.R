#######################################
############SICSS-NDSU 2022############
#######################################

##Day 2 Collecting Digital Trace Data##
##       Part 2: Twitter API         ## 
##       For academic API            ##
##       Author: Shuning Lu        ####


#install and load packages::academictwitteR, tidyverse, dbplyr, ggplot2, readr

library(academictwitteR)
library(readr)
library(tidyverse)
library(dbplyr)
library(ggplot2)

#alternative strategy to install academictwitteR
#install.packages("devtools", dependencies = TRUE)
#library(devtools)
#devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE) 


#set up your bearer 
bearer_token <- read_file("bearer_token.txt")

#create your query based on hashtags
tweets <- get_all_tweets(query = "#GunControl OR #GunViolence", 
                          start_tweets = "2022-06-01T00:00:00Z", #start date 
                          end_tweets = "2022-06-11T00:00:00Z", #end date
                          bearer_token = bearer_token, #bearer token to access Twitter API 
                          file = "gunissue1", #name of the RDS file 
                          data_path = "data/", #data path to store the RDS file
                          n = 5000)  #set an upper limit of tweets, 100 is the default limit, n = Inf allows you to get all tweets matching the criteria  

class(tweets1)#file format
names(tweets1)#column names
head(tweets1)#take a look at the data
head(tweets1$text) #first several tweets
tail(tweets1$text) #last several tweets

#create your query based on keywords or hashtag
tweets2 <- get_all_tweets(query = '"gun control" OR #GunControl', 
                          start_tweets = "2022-06-01T00:00:00Z", 
                          end_tweets = "2022-06-03T00:00:00Z",  
                          bearer_token = bearer_token, 
                          file = "gunissue2", 
                          data_path = "data/",
                          n = 500)
head(tweets2) 
head(tweets2$text) 

list.files("data")#list files in the data path, 4 json files and 1 query text file


#create your query based on keywords and hashtag
tweets_texas <- get_all_tweets(query = '"Texas" #GunControl OR #GunViolence', 
                          start_tweets = "2022-06-01T00:00:00Z",   
                          end_tweets = "2022-06-03T00:00:00Z",  
                          bearer_token = bearer_token, 
                          file = "gun_texas", 
                          data_path = "data/",
                          n = 500)

#creat your query based on a specific country and original tweet
us_tweet_gun <- get_all_tweets(query = '#guncontrol',
                               country = 'US',
                               lang = 'en', 
                               is_retweet = FALSE, #filter out retweet
                               start_tweets = "2022-06-01T00:00:00Z",
                               end_tweets = "2022-06-03T00:00:00Z",
                               bearer_token = bearer_token,
                               file = "us_tweet",
                               data_path = "data/",
                               n = 500)


#bind the data we've collected
gunissue_data <- bind_tweet_jsons(data_path = "data")
head(gunissue_data)

#remove duplicated tweets
gunissue_data<-distinct(gunissue_data, text, .keep_all = TRUE)

#group data by dates
gunissue_data$date <- as.Date(gunissue_data$created_at)
dat_collapsed_daily <- gunissue_data %>% group_by(date) %>% summarize(n())
dat_collapsed_daily


#create your query based on users
gov_tweet <- get_all_tweets(users = c("GregAbbott_TX", "GovKathyHochul"), #users object 
                start_tweets = "2022-06-01T00:00:00Z", #start date
                end_tweets = "2022-06-07T00:00:00Z", #end date
                bearer_token = bearer_token, #bearer token 
                file = "govtweets", #RDS file name
                data_path = "governor/", #data path stores RDS file
                bind_tweets = FALSE, #This mitigates potential data loss in cases of query interruption. 
                n = 500) #set an upper limit of tweets 
head(gov_tweet)


#create your query based on hashtag or keywords from specific users
gov_gun <- get_all_tweets(users = c("GregAbbott_TX", "GovKathyHochul", "GavinNewsom", "GovTimWalz"), #users 
                                query = c("gun","shooting", "shooter", "firearm"), #keywords
                            start_tweets = "2021-06-01T00:00:00Z",  
                            end_tweets = "2022-06-01T00:00:00Z",  
                            bearer_token = bearer_token,  
                            file = "gov_gun",
                            data_path = "governor_gun/", 
                            bind_tweets = FALSE,  
                            n = Inf)
head(gov_gun)

#get user profile data
gov <- gov_gun$author_id
gov_profile <- get_user_profile(gov, bearer_token)
names(gov_profile) #take a look at the features

#combine profile and tweet data
gov_profile2 <- gov_profile %>% select(name)
gov_data <- cbind(gov_gun, gov_profile2)
table(gov_data$name)


#group data by governor per month
gov_data$date <- as.Date(gov_data$created_at)

gov_group<-gov_data %>% 
  mutate(ym = format(date, '%Y-%m')) %>% 
  group_by(name, ym) %>% 
  summarize(n())
gov_group <- gov_group %>% mutate(frequency = `n()`)


#plot data
gov_group %>% 
  ggplot(aes(x = ym, y = frequency, group = name, color = name)) + geom_line() +  
  ggtitle("Tweet Frequency on Gun Issues from Four Governors",subtitle = "Tweet counts aggregated monthly") +
  labs(color='Governors') +
  scale_color_manual(labels = c("Abbott", "Hochul", "Newsom", "Walz"), values = c("aquamarine3", "red", "orange",
                                                                                   "purple")) + 
  theme(plot.title = element_text(face = "bold")) + ylab("Frequency") + xlab("Months") 
  theme_bw()


