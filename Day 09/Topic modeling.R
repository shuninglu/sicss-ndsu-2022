#######################################
############SICSS-NDSU 2022############
#######################################

##      Day 8 Topic Modeling         ##
##      Author: Shuning Lu         ####


#library(rtweet) for collecting tweets
library(lubridate) #for extracting dates
library(tidyverse)
library(quanteda)
library(stm)
library(wordcloud)


#data collection#
#bearer_token <- read_file("bearer_token1.txt")
#tweets <- get_timeline(user= c("NDSU", "UofNorthDakota"),
#                                 n=1000,
#                                 home=FALSE)


#read file
tweets <- readRDS(file = "tweets.rds")

#prepare covariates - month
elements <- str_split(tweets$created_at, fixed(" "), n=2)
tweets$Date <- map_chr(elements, 1)
tweets$month <- month(as.POSIXlt(tweets$Date, format="%Y-%m-%d"))

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
tweets$text<-tweets$text %>% clean_tweets

#tokenization
tweetCorpus <- corpus(tweets$text)
tweetCorpus <- tweetCorpus %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english")))

#pass covariates to corpus, you may select other features
docvars(tweetCorpus, field = "month") <- tweets$month # covariance field month
docvars(tweetCorpus, field = "university") <- tweets$screen_name # covariance field university

#converting corpus to document frequency matrix
tweetDfm <- dfm(tweetCorpus) 
topfeatures(tweetDfm, n = 100, scheme = "docfreq")

#prepare stm data
tweets.stm <- convert(tweetDfm, to = "stm")
out <- prepDocuments(tweets.stm$documents, 
                     tweets.stm$vocab, 
                     tweets.stm$meta, 
                     lower.thresh = 10) 

#Topic modeling
#How do we decide how many topics?
#1.Statistical fit
#2.Interpretability and relevance of topics

#Let's find the optimal K, this will take a long time, be prepared
#storage <- searchK(out$documents, out$vocab, K = c(3:10),
#                 data=out$meta)

## results
#  K   exclus    semcoh   heldout residual     bound    lbound em.its
#  3 8.731013  -125.735 -5.730929 1.953435 -109462.8   -109461    285
#  4 9.126157 -125.3244 -5.717498 1.900893 -108629.4 -108626.2    239
#  5 9.105678 -123.9671 -5.634661 1.823361 -107745.3 -107740.5    161
#  6 9.314834 -125.3053 -5.611538 1.799504 -107151.3 -107144.7    162
#  7 9.403309  -132.258   -5.6152 1.733148 -106679.6 -106671.1     48
#  8 9.475087 -136.3502 -5.637102 1.722141 -106388.9 -106378.3     29
#  9 9.506327 -129.2937 -5.580419 1.702409 -105671.1 -105658.3     36
# 10 9.533249 -141.4362 -5.580491 1.657298 -105399.6 -105384.5     17
#plot(storage)

#The results indicate 5-topic solution could work, because
#both exclusivity coefficient and semantic coherence coefficient are high

#Now, let's run the 5-topic model  
model <- stm(out$documents, out$vocab,
            K = 5, 
            max.em.its = 100, #its number should follow searchK output
            data = out$meta, 
            init.type = "Spectral")

#words used in topics
labelTopics(model, c(1:5), frexweight = 0.5,n = 15) 

#Let's focus on FREX
#It shows words that are comparatively common and exclusive for a topic compared to others
labels <- labelTopics(model, n = 20)
topwords <- data.frame("features" = t(labels$frex))
colnames(topwords) <- paste("Topics", c(1:5))
topwords[1:5]

#plot topics
par(bty="n",col="grey40",lwd=6)
plot(model, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: ????",
                     "Topic 2: ????",
                     "Topic 3: ????",
                     "Topic 4: ????",
                     "Topic 5: ????"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

#wordcloud
cloud(model, topic = 1, min.freq=3)
cloud(model, topic = 2, min.freq=3)

#plot two topics and compare
plot(model, 
     type="perspectives", 
     topics=c(3,5), 
     plabels = c("Topic 3","Topic 5"))

#compare topic 1 and 2?

#Please tweak the code and run the six-topic model
#How would you label them? Which makes more sense, five- or six- topic solution?


#Structural topic modeling
#introducing document-level data into a regression model to predict topic

#university as covariate
result1 <- estimateEffect(1:5 ~ university, model,
                       meta = out$meta, uncertainty = "Global")
topiceffect1<-summary(result1, topics = c(1:5))
topiceffect1

#you can save the result: write.table(topiceffect$tables, 'output.csv', sep=",")

#distribution of specific topic by university
plot(x = result1, 
     covariate = "university", 
     topic = c(5), 
     model = model, 
     method = "pointestimate",
     xlim = c(0.1, 0.5))

#month as covariate
result2 <- estimateEffect(1:5 ~ month, model,
                         meta = out$meta, uncertainty = "Global")
topiceffect2<-summary(result2, topics = c(1:5))
topiceffect2

#distribution of specific topic by month
plot(x = result2, 
     covariate = "month", 
     topic = c(2), 
     model = model, 
     method = "continuous",
     xlim = c(1, 12))

#fine-tuning
plot(x = result2, 
     covariate = "month", 
     topic = c(2), 
     model = model, 
     method = "continuous",
     xlim = c(0, 12),
     ylim = c(0.15, 0.3),
     linecol = "skyblue", width = 20, lwd=2)

#month and university as covariates
result3 <- estimateEffect(1:5 ~ month + university + month*university, model,
                          meta = out$meta, uncertainty = "Global")
topiceffect3<-summary(result3, topics = c(1:5))
topiceffect3

