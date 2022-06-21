#######################################
############SICSS-NDSU 2022############
#######################################

##Day 2 Collecting Digital Trace Data##
##       Part 3: Twitter API         ## 
##       For standard API            ##
##       Author: Shuning Lu        ####

#Read package documentation: https://cran.r-project.org/web/packages/rtweet/rtweet.pdf


#clean up the space
rm(list = ls())


#0. Set up the R environment
#load library, use install.packages to install them
library(rtweet)
library(readr)
library(ggplot2)
library(wordcloud)


#information on getting tokens
vignette("auth", package = "rtweet") 

#set up your bearer 
bearer_token <- read_file("bearer_token1.txt")

# Get tweets from search: returns tweets from the last ~7 days
# Rate limit: ~18,000 tweets every 15 minutes.

#1. make your first query on SICSS2022
#search tweets on SICSS2022
sicss <- search_tweets("#sicss2022",n = 100)

#inspect the data
colnames(sicss) #variables of your dataframe
dim(sicss) #dimension of the dataframe, # case x # variable
head(sicss, n = 3) #take a look at the first three observations
tail(sicss, n = 5) #the last five observations
sicss$text[1] #the first tweet in the dataframe
users_data(sicss)#user data 

#plot the data by hours
ts_plot(sicss, by = 'hours')


#2. make additional queries 
#search tweets on SICSS2022 with additional hashtags
sicss <- search_tweets(q = '"#sicss2022" OR "#sicss"',n = 100)
sicss <- search_tweets(q = '"#sicss2022" AND "#sicssndsu"',n = 100)

#search original tweets on SICSS2022 in USA
sicss_usa <- search_tweets("#sicss2022", n = 100,
                           geocode = lookup_coords("usa"),#geo boundary
                           include_rts = FALSE) #original

#find out who is tweeting original tweets in USA
head(sicss_usa$screen_name)

#tweets from specific users
sicss_ndsu <- search_tweets("from:SICSS_NDSU", n = 100)
sicss_ndsu$text[1]

#tweets at somebody, you can also use "@CDCgov"
cdc_replies <- search_tweets("to:CDCgov",n = 100)
cdc_replies$text[1]

#tweets based on locations of users
ndsu20_tweets <- search_tweets("",  
                             geocode= "46.8978,-96.8024,20mi", #within 20 miles of NDSU
                             n = 100, 
                             include_rts = FALSE,
                             language = "en")

head(ndsu20_tweets) 
head(ndsu20_tweets$text)


#3. collecting tweets on mass shooting 
#what are some of the keywords?
#tweets with links to news - filter:news
gun_tweets <- search_tweets('"gun violence" filter:news min_faves:100',              
                             n = 100, 
                             include_rts = FALSE, #alternatively, filter:retweets
                             language = "en")

#Let's also try - filer:links, what does it do?
#set min_retweets, min_replies. what do they achieve? 
#how about - filter:verified?
#and - filter:media?

#get all URL of the news sites
gun_tweets$urls_expanded_url

#plotting
wordcloud::wordcloud(gun_tweets$text, min.freq=3) #take a glance of the text
hist(gun_tweets$favorite_count) #distribution of favorite counts
hist(gun_tweets$retweet_count)  #distribution of retweets counts

#tweets with over 1,000 retweets:
gun_tweets[gun_tweets$retweet_count>1000, c("screen_name", "text" )] 

?search_tweets #learn more about the query based on tweets


# The Twitter API lets us get the most recent tweets from a user's timeline
# The limit is a maximum of 3,200 tweets.
# Parameter home=FALSE gets user timeline; TRUE gets their home feed.

#4. user related
#get tweets from specific users
nyt_tweets <- get_timeline(user="nytimes",
                            n=100,
                            home=FALSE)

nyt_tweets$text
wordcloud::wordcloud(nyt_tweets$text, min.freq=3)

#get tweets from multiple users
newspaper_tweets <- get_timeline(user= c("nytimes", "WSJ"),
                             n=100,
                             home=FALSE)
newspaper_tweets$text

#get users followed by nytimes
nyt_fo <- get_friends(users="nytimes", n=100)
nyt_fo
nyt_fo_acc <- lookup_users(nyt_fo$user_id) #using id to look up screennames
nyt_fo_acc$screen_name 

#get users users who follow nytimes
nyt_fol <- get_followers(user="nytimes", n=100)
nyt_fol_acc  <- lookup_users(nyt_fol$user_id)
nyt_fol_acc
nyt_fol_acc$screen_name 

#If you want to get a large number of followers, include parameter retryonratelimit = TRUE


# The streaming API randomly samples approx. 1% of all live tweets
# The following code will get live tweets over a period of 30 seconds,
# looking for ones with the that contain the term "happy" in them.

#5.Twitter streaming API
live_happy <- stream_tweets("happy", 
                            timeout=30, #timeout = 60
                            language = "en")

stream_tweets("happy", 
              timeout=30, #timeout = 60
              file_name="happy.json",
              data_path = "dataset/",
              parse = FALSE,
              language = "en")

#Read the file back and parse it to get a nice format:
live_happy1 <- parse_stream("happy.json")
live_happy1$text[1]

