library(tidyverse)
library(RedditExtractoR)
library(tidytext) 
library(sentimentr)
library(quanteda)
library(stm)
library(wordcloud)
library(rtweet) 
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(rvest) 
library(usmap)
library(jtools) #summ and export_summs 
library(readxl) 

#####Reddit#####
#####COFFEE#####
# Collect URLs of comment threads that contain a specific keyword
#reddit_urls_coffee <- find_thread_urls(
#  keywords="coffee", 
#  subreddit="HydroHomies", 
#  period = "all")

# Iterate through URLs to pull content
#content_coffee <- get_thread_content(reddit_urls_coffee$url[1-100])

#Save Data
#save(content_coffee, file = "~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/redditcoffee.Rdata") 

#Clean Reddit Posts, you can tweak the codes below based on your research
clean_reddit <- function(x) {
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

#Load Data
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/redditcoffee.Rdata") 
  #Uncomment the code above to collect your own data on this topic 

# Get comment text data. Metadata available under content$threads
comments_coffee <- content_coffee$comments

# Group comments by author: who posts most often about keyword?
by_author_coffee <- comments_coffee %>% group_by(author)
#View(by_author_coffee)
#View(by_author_coffee %>% tally(sort=TRUE))

# Which commenters are most popular?
by_upvote_coffee <- by_author_coffee %>% summarise(sum(upvotes))


## Number of negative words
reddit_hate_coffee <- comments_coffee %>%
  unnest_tokens("word", comment)

## Count number of negative, positive, NA words in tweet
reddit_hate_sentiment_coffee <- reddit_hate_coffee %>%
  left_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) 

## Add negative count to original dataframe
reddit_hate_coffee$nnegative <- reddit_hate_sentiment_coffee$negative


#' Number of profane words
#'   - Note: this function may generate a warning message, which you can ignore for this application.
reddit_hate_profanity_coffee <- profanity_by(comments_coffee$comment)

## Add profanity count to original dataframe
comments_coffee$nprofanity <- reddit_hate_profanity_coffee$profanity_count


redditstuff_coffee <- get_sentences(comments_coffee$comment) 

emotion(redditstuff_coffee)

emo_words_coffee <- extract_emotion_terms(redditstuff_coffee)
emo_words_coffee
emo_words_coffee$sentence
emo_words_coffee[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_coffee)$counts
attributes(emo_words_coffee)$elements


(emo_coffee <- emotion(redditstuff_coffee))
emotion(redditstuff_coffee, drop.unused.emotions = TRUE)

plot(emo_coffee)
plot(emo_coffee, drop.unused.emotions = FALSE)
plot(emo_coffee, facet = FALSE)
plot(emo_coffee, facet = 'negated')

#Same as above, but in one entry 
plot(
  emo_coffee,
  transformation.function = syuzhet::get_dct_transform,
  drop.unused.emotions = TRUE,
  facet = FALSE
)

#Reddit Tea Talk Topic Modeling 
comments_coffee$comment <- comments_coffee$comment %>% clean_reddit

#tokenization
redditCorpus_coffee <- corpus(comments_coffee$comment)
redditCorpus_coffee <- redditCorpus_coffee %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english")))

#converting corpus to document frequency matrix
redditDfm_coffee <- dfm(redditCorpus_coffee) 
topfeatures(redditDfm_coffee, n = 100, scheme = "docfreq")

#prepare stm data
reddit_coffee.stm <- convert(redditDfm_coffee, to = "stm")
out_reddit_coffee <- prepDocuments(reddit_coffee.stm$documents, 
                            reddit_coffee.stm$vocab, 
                            reddit_coffee.stm$meta, 
                            lower.thresh = 10) 

#Let's find the optimal K, this will take a long time, be prepared
storage_reddit_coffee <- searchK(out_reddit_coffee$documents, out_reddit_coffee$vocab, K = c(3:10),
                          data=out_reddit_coffee$meta)

storage_reddit_coffee

#Now, let's run the 4-topic model  
reddit_model_coffee <- stm(out_reddit_coffee$documents, out_reddit_coffee$vocab,
                    K = 4, 
                    max.em.its = 100, #its number should follow searchK output
                    data = out_reddit_coffee$meta, 
                    init.type = "Spectral")

#words used in topics
labelTopics(reddit_model_coffee, c(1:4), frexweight = 0.5,n = 15) 

#Let's focus on FREX
#It shows words that are comparatively common and exclusive for a topic compared to others
reddit_labels_coffee <- labelTopics(reddit_model_coffee, n = 20)
reddit_topwords_coffee <- data.frame("features" = t(reddit_labels_coffee$frex))
colnames(reddit_topwords_coffee) <- paste("Topics", c(1:4))
reddit_topwords_coffee[1:4]

#plot topics
par(bty="n",col="grey40",lwd=6)
plot(reddit_model_coffee, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: Cold, Ice, Hydration",
                     "Topic 2: Drink, Thirsty, Enough",
                     "Topic 3: Generic Post Stuff",
                     "Topic 4: Caffeine, Drink, great, Coffee"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

#wordcloud
cloud(reddit_model_coffee, topic = 4, min.freq=3)
cloud(reddit_model_coffee, topic = 2, min.freq=3)

#plot two topics and compare
plot(reddit_model_coffee, 
     type="perspectives", 
     topics=c(2,4), 
     plabels = c("Topic 2","Topic 4"))




#####TEA#####
# Collect URLs of comment threads that contain a specific keyword
#reddit_urls_tea <- find_thread_urls(
#  keywords="tea", 
#  subreddit="HydroHomies", 
#  period = "all")

# Iterate through URLs to pull content
#content_tea <- get_thread_content(reddit_urls_tea$url[1-100])

#Save Data
#save(content_tea, file = "~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/reddittea.Rdata") 

#Load Data
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/reddittea.Rdata") 

# Get comment text data. Metadata availble under content$threads
comments_tea <- content_tea$comments

# Group comments by author: who posts most often about keyword?
# Q: what does the weird %>% symbol do?
by_author_tea <- comments_tea %>% group_by(author)
#View(by_author_tea)
#View(by_author_tea %>% tally(sort=TRUE))

# Which commenters are most popular?
by_upvote_tea <- by_author_tea %>% summarise(sum(upvotes))


## Number of negative words
reddit_hate_tea <- comments_tea %>%
  unnest_tokens("word", comment)

## Count number of negative, positive, NA words in tweet
reddit_hate_sentiment_tea <- reddit_hate_tea %>%
  left_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) 

## Add negative count to original dataframe
reddit_hate_tea$nnegative <- reddit_hate_sentiment_tea$negative


#' Number of profane words
#'   - Note: this function may generate a warning message, which you can ignore for this application.
reddit_hate_profanity_tea <- profanity_by(comments_tea$comment)

## Add profanity count to original dataframe
comments_tea$nprofanity <- reddit_hate_profanity_tea$profanity_count


redditstuff_tea <- get_sentences(comments_tea$comment) 

emotion(redditstuff_tea)

emo_words_tea <- extract_emotion_terms(redditstuff_tea)
emo_words_tea
emo_words_tea$sentence
emo_words_tea[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_tea)$counts
attributes(emo_words_tea)$elements


## preferred method avoiding paying the cost

(emo_tea <- emotion(redditstuff_tea))
emotion(redditstuff_tea, drop.unused.emotions = TRUE)
## Not run:
plot(emo_tea)
plot(emo_tea, drop.unused.emotions = FALSE)
plot(emo_tea, facet = FALSE)
plot(emo_tea, facet = 'negated')

#Same as above, but in one entry 
plot(
  emo_tea,
  transformation.function = syuzhet::get_dct_transform,
  drop.unused.emotions = TRUE,
  facet = FALSE
)


#Reddit Tea Talk Topic Modeling 
comments_tea$comment <- comments_tea$comment %>% clean_reddit

#tokenization
redditCorpus_tea <- corpus(comments_tea$comment)
redditCorpus_tea <- redditCorpus_tea %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english")))

#converting corpus to document frequency matrix
redditDfm_tea <- dfm(redditCorpus_tea) 
topfeatures(redditDfm_tea, n = 100, scheme = "docfreq")

#prepare stm data
reddit_tea.stm <- convert(redditDfm_tea, to = "stm")
out_reddit_tea <- prepDocuments(reddit_tea.stm$documents, 
                     reddit_tea.stm$vocab, 
                     reddit_tea.stm$meta, 
                     lower.thresh = 10) 

#Let's find the optimal K, this will take a long time, be prepared
storage_reddit_tea <- searchK(out_reddit_tea$documents, out_reddit_tea$vocab, K = c(3:10),
                 data=out_reddit_tea$meta)

storage_reddit_tea

#Now, let's run the 5-topic model  
reddit_model_tea <- stm(out_reddit_tea$documents, out_reddit_tea$vocab,
             K = 6, 
             max.em.its = 100, #its number should follow searchK output
             data = out_reddit_tea$meta, 
             init.type = "Spectral")

#words used in topics
labelTopics(reddit_model_tea, c(1:6), frexweight = 0.5,n = 15) 

#Let's focus on FREX
#It shows words that are comparatively common and exclusive for a topic compared to others
reddit_labels_tea <- labelTopics(reddit_model_tea, n = 20)
reddit_topwords_tea <- data.frame("features" = t(reddit_labels_tea$frex))
colnames(reddit_topwords_tea) <- paste("Topics", c(1:6))
reddit_topwords_tea[1:6]

#plot topics
par(bty="n",col="grey40",lwd=6)
plot(reddit_model_tea, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: Generic subreddit words",
                     "Topic 2: ????",
                     "Topic 3: Tea - Water",
                     "Topic 4: Soda, bad, hydration",
                     "Topic 5: Tea - iced, cold, unsweetened", 
                     "Topic 6: Drink liquids - health"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

#wordcloud
cloud(reddit_model_tea, topic = 3, min.freq=3)
cloud(reddit_model_tea, topic = 5, min.freq=3)

#plot two topics and compare
plot(reddit_model_tea, 
     type="perspectives", 
     topics=c(3,5), 
     plabels = c("Topic 3","Topic 5"))



#####Twitter#####
#set up your bearer - You need to put in your own Bearer Token... 
#bearer_token <- read_file("bearer_token1.txt")

#search tweets on coffeebreak
#coffeebreak_twitter_usa <- search_tweets("#coffeebreak", n = 500,
#                                 geocode = lookup_coords("usa"),
#                                 include_rts = FALSE)

#teatime_twitter_usa <- search_tweets("#teatime", n = 500,
#                             geocode = lookup_coords("usa"),
#                             include_rts = FALSE)

load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/teatime_usa.Rdata")
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/coffeebreak_usa.Rdata")

teatime_twitter_usa <- teatime_usa
coffeebreak_twitter_usa <- coffeebreak_usa
#save(coffeebreak_twitter_usa, file="~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/coffeebreak_twitter_usa.Rdata")

#save(teatime_twitter_usa, file="~/desktop/SICSS-NDSU/Group Projects/Coffee Projects/Data/teatime_twitter_usa.Rdata")

text_coffee_twitter <- coffeebreak_twitter_usa$text
text_tea_twitter <- teatime_twitter_usa$text

teatime_twitter_usa$nohash <- lengths(teatime_usa$hashtags)
coffeebreak_twitter_usa$nohash <- lengths(coffeebreak_usa$hashtags)


emo_words_coffee_twitter <- extract_emotion_terms(text_coffee_twitter)
emo_words_coffee_twitter
emo_words_coffee_twitter$sentence
emo_words_coffee_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_coffee_twitter)$counts
attributes(emo_words_coffee_twitter)$elements

(emo_coffee_twitter <- emotion(text_coffee_twitter))
emotion(text_coffee_twitter, drop.unused.emotions = TRUE)

plot(emo_coffee_twitter)
plot(emo_coffee_twitter, drop.unused.emotions = FALSE)
plot(emo_coffee_twitter, facet = FALSE)
plot(emo_coffee_twitter, facet = 'negated')

#tea emotion
emo_words_tea_twitter <- extract_emotion_terms(text_tea_twitter)
emo_words_tea_twitter
emo_words_tea_twitter$sentence
emo_words_tea_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_words_tea_twitter)$counts
attributes(emo_words_tea_twitter)$elements

(emo_tea_twitter <- emotion(text_tea_twitter))
emotion(text_tea_twitter, drop.unused.emotions = TRUE)

plot(emo_tea_twitter)
plot(emo_tea_twitter, drop.unused.emotions = FALSE)
plot(emo_tea_twitter, facet = FALSE)
plot(emo_tea_twitter, facet = 'negated')

indcoffee_twitter <- subset(coffeebreak_twitter_usa, nohash<10)
buscoffee_twitter <- subset(coffeebreak_twitter_usa, nohash>9)

text_indcoffee_twitter <- indcoffee_twitter$text
text_buscoffee_twitter <- buscoffee_twitter$text

indtea_twitter <- subset(teatime_twitter_usa, nohash<10)
bustea_twitter <- subset(teatime_twitter_usa, nohash>9)

text_indtea_twitter <- indtea_twitter$text
text_bustea_twitter <- bustea_twitter$text

wordcloud::wordcloud(indcoffee_twitter$text, min.freq=3) #take a glance of the text
wordcloud::wordcloud(buscoffee_twitter$text, min.freq=3) #take a glance of the text

wordcloud::wordcloud(indtea_twitter$text, min.freq=3) #take a glance of the text
wordcloud::wordcloud(bustea_twitter$text, min.freq=3) #take a glance of the text

emo_indcoffee_twitter <- extract_emotion_terms(text_indcoffee_twitter)
emo_indcoffee_twitter
emo_indcoffee_twitter$sentence
emo_indcoffee_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_indcoffee_twitter)$counts
attributes(emo_indcoffee_twitter)$elements

(emo_icoffee_twitter <- emotion(text_indcoffee_twitter))
emotion(text_indcoffee_twitter, drop.unused.emotions = TRUE)

plot(emo_icoffee_twitter)
plot(emo_icoffee_twitter, drop.unused.emotions = FALSE)
plot(emo_icoffee_twitter, facet = FALSE)
plot(emo_icoffee_twitter, facet = 'negated')



emo_buscoffee_twitter <- extract_emotion_terms(text_buscoffee_twitter)
emo_buscoffee_twitter
emo_buscoffee_twitter$sentence
emo_buscoffee_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_buscoffee_twitter)$counts
attributes(emo_buscoffee_twitter)$elements

(emo_bcoffee_twitter <- emotion(text_buscoffee_twitter))
emotion(text_buscoffee_twitter, drop.unused.emotions = TRUE)

plot(emo_bcoffee_twitter)
plot(emo_bcoffee_twitter, drop.unused.emotions = FALSE)
plot(emo_bcoffee_twitter, facet = FALSE)
plot(emo_bcoffee_twitter, facet = 'negated')


emo_indtea_twitter <- extract_emotion_terms(text_indtea_twitter)
emo_indtea_twitter
emo_indtea_twitter$sentence
emo_indtea_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_indtea_twitter)$counts
attributes(emo_indtea_twitter)$elements

(emo_itea_twitter <- emotion(text_indtea_twitter))
emotion(text_indtea_twitter, drop.unused.emotions = TRUE)

plot(emo_itea_twitter)
plot(emo_itea_twitter, drop.unused.emotions = FALSE)
plot(emo_itea_twitter, facet = FALSE)
plot(emo_itea_twitter, facet = 'negated')


emo_bustea_twitter <- extract_emotion_terms(text_bustea_twitter)
emo_bustea_twitter
emo_bustea_twitter$sentence
emo_bustea_twitter[, c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')]
attributes(emo_bustea_twitter)$counts
attributes(emo_bustea_twitter)$elements

(emo_btea_twitter <- emotion(text_bustea_twitter))
emotion(text_bustea_twitter, drop.unused.emotions = TRUE)

plot(emo_btea_twitter)
plot(emo_btea_twitter, drop.unused.emotions = FALSE)
plot(emo_btea_twitter, facet = FALSE)
plot(emo_btea_twitter, facet = 'negated')


#####Visuals and Regressions for Plotting#####

#####Data Importation and Collection#####
url <- "https://vividmaps.com/us-coffee-index/"
url %>%
  read_html() #read HTML into R
css_selector <- "#tablepress-153" #using inspection tool

table2 <- url %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

#Rename columns
table2 <- rename(table2, Shops = 'Coffe Shops per 100k')
table2 <- rename(table2, state = "State")
table2 <- rename(table2, Avg_Price = 'Avg Coffe Price')

#Recode States
table2$state <- tolower(table2$state)

#Load the Data 
load("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/acscoffee.Rdata") 
#To access the ACS data follow this link: https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/
#To get this dataset we limited our responses to those in the "Coffee" categories (OCCP == 4150)

#####Subsetting Full-Time##### 
fullcoffee <- subset(acscoffee, OCCP==4150 & WKHP>39) 

#Get mean Wage for FT people
meanwagefull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WAGP), list(meanwagefull = mean))

#Get mean Hours for FT people 
meanhoursfull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WKHP), list(meanhoursfull = mean))

#Create Mean Income Per State 
meanincomefull <- fullcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(PINCP), list(meanincomefull = mean))

#Add a field for State Name 
meanwagefull <- meanwagefull %>% 
  mutate(StateName = 
           case_when( 
             ST == '01' ~ "alabama",
             ST == '02' ~ "alaska", 
             ST == '04' ~ "arizona", 
             ST == '05' ~ "arkansas", 
             ST == '06' ~ "california", 
             ST == '08' ~ "colorado", 
             ST == '09' ~ "connecticut", 
             ST == '10' ~ "delaware", 
             ST == '12' ~ "florida", 
             ST == '13' ~ "georgia", 
             ST == '15' ~ "hawaii", 
             ST == '16' ~ "idaho", 
             ST == '17' ~ "illinois", 
             ST == '18' ~ "indiana", 
             ST == '19' ~ "iowa", 
             ST == '20' ~ "kansas", 
             ST == '21' ~ "kentucky", 
             ST == '22' ~ "louisiana", 
             ST == '23' ~ "maine", 
             ST == '24' ~ "maryland", 
             ST == '25' ~ "massachusetts", 
             ST == '26' ~ "michigan", 
             ST == '27' ~ "minnesota", 
             ST == '28' ~ "mississippi", 
             ST == '29' ~ "missouri", 
             ST == '30' ~ "montana", 
             ST == '31' ~ "nebraska", 
             ST == '32' ~ "nevada", 
             ST == '33' ~ "new hampshire", 
             ST == '34' ~ "new jersey", 
             ST == '35' ~ "new mexico",
             ST == '36' ~ "new york", 
             ST == '37' ~ "north carolina", 
             ST == '38' ~ "north dakota", 
             ST == '39' ~ "ohio", 
             ST == '40' ~ "oklahoma", 
             ST == '41' ~ "oregon", 
             ST == '42' ~ "pennsylvania", 
             ST == '44' ~ "rhode island", 
             ST == '45' ~ "south carolina", 
             ST == '46' ~ "south dakota", 
             ST == '47' ~ "tennessee", 
             ST == '48' ~ "texas", 
             ST == '49' ~ "utah", 
             ST == '50' ~ "vermont", 
             ST == '51' ~ "virginia", 
             ST == '53' ~ "washington", 
             ST == '54' ~ "west virginia", 
             ST == '55' ~ "wisconsin", 
             ST == '56' ~ "wyoming"))

#Join Together Averages
meanwagefull <- left_join(meanwagefull, meanhoursfull) 
meanwagefull <- left_join(meanwagefull, meanincomefull) 

#Join the table2 & meanwagefull
meanwagemergedfull <- left_join(table2, meanwagefull, by = c('state' = 'StateName')) 


#####Subsetting Part-Time##### 
partcoffee <- subset(acscoffee, OCCP==4150 & WKHP<40) 

#Get mean Wage for FT people
meanwagepart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WAGP), list(meanwagepart = mean))

#Get mean Hours for FT people 
meanhourspart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(WKHP), list(meanhourspart = mean))

#Create Mean Income Per State 
meanincomepart <- partcoffee %>%
  group_by(ST) %>%
  summarise_at(vars(PINCP), list(meanincomepart = mean))

#Add a field for State Name 
meanwagepart <- meanwagepart %>% 
  mutate(StateName = 
           case_when( 
             ST == '01' ~ "alabama",
             ST == '02' ~ "alaska", 
             ST == '04' ~ "arizona", 
             ST == '05' ~ "arkansas", 
             ST == '06' ~ "california", 
             ST == '08' ~ "colorado", 
             ST == '09' ~ "connecticut", 
             ST == '10' ~ "delaware", 
             ST == '12' ~ "florida", 
             ST == '13' ~ "georgia", 
             ST == '15' ~ "hawaii", 
             ST == '16' ~ "idaho", 
             ST == '17' ~ "illinois", 
             ST == '18' ~ "indiana", 
             ST == '19' ~ "iowa", 
             ST == '20' ~ "kansas", 
             ST == '21' ~ "kentucky", 
             ST == '22' ~ "louisiana", 
             ST == '23' ~ "maine", 
             ST == '24' ~ "maryland", 
             ST == '25' ~ "massachusetts", 
             ST == '26' ~ "michigan", 
             ST == '27' ~ "minnesota", 
             ST == '28' ~ "mississippi", 
             ST == '29' ~ "missouri", 
             ST == '30' ~ "montana", 
             ST == '31' ~ "nebraska", 
             ST == '32' ~ "nevada", 
             ST == '33' ~ "new hampshire", 
             ST == '34' ~ "new jersey", 
             ST == '35' ~ "new mexico",
             ST == '36' ~ "new york", 
             ST == '37' ~ "north carolina", 
             ST == '38' ~ "north dakota", 
             ST == '39' ~ "ohio", 
             ST == '40' ~ "oklahoma", 
             ST == '41' ~ "oregon", 
             ST == '42' ~ "pennsylvania", 
             ST == '44' ~ "rhode island", 
             ST == '45' ~ "south carolina", 
             ST == '46' ~ "south dakota", 
             ST == '47' ~ "tennessee", 
             ST == '48' ~ "texas", 
             ST == '49' ~ "utah", 
             ST == '50' ~ "vermont", 
             ST == '51' ~ "virginia", 
             ST == '53' ~ "washington", 
             ST == '54' ~ "west virginia", 
             ST == '55' ~ "wisconsin", 
             ST == '56' ~ "wyoming"))

#Join Together Averages
meanwagepart <- left_join(meanwagepart, meanhourspart) 
meanwagepart <- left_join(meanwagepart, meanincomepart) 

#Join the usa & meanwagefull
meanwagemergedpart <- left_join(table2, meanwagepart, by = c('state' = 'StateName')) 



#####Visualization#####
#create map for nr. of coffee shops per state
map1 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = table2,
  values = "Shops",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Number of Shops per 100k People", label = scales::comma) + 
  labs(title = "Number of Coffee Shops in the US")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map1

#create map for average price of coffee per state
map2 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = table2,
  values = "Avg_Price",
  color = "darkseagreen"
) + 
  scale_fill_continuous(low = "white", high = "darkseagreen4", name = "Price in Dollars", label = scales::comma) + 
  labs(title = "Average Price of Coffee in the US")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map2


#Plot Mean Wages - FT
map3 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanwagefull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Wage - FT", label = scales::comma) + 
  labs(title = "Mean Wage of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map3

#Plot Mean Income - FT
map4 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanincomefull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Income - FT", label = scales::comma) + 
  labs(title = "Mean Income of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map4

#Plot Mean Hours Worked Per Week - FT
map5 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedfull,
  values = "meanhoursfull",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Hours - FT", label = scales::comma) + 
  labs(title = "Mean Hours Worked Per Week of Full-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map5



#Plot Mean Wages - PT
map6 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanwagepart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Wage - PT", label = scales::comma) + 
  labs(title = "Mean Wage of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map6

#Plot Mean Income - PT
map7 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanincomepart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Income - PT", label = scales::comma) + 
  labs(title = "Mean Income of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map7

#Plot Mean Hours Worked Per Week - PT
map8 <- plot_usmap(
  regions = "state",
  labels = "TRUE",
  label_color = "black",
  data = meanwagemergedpart,
  values = "meanhourspart",
  color = "burlywood4"
) + 
  scale_fill_continuous(low = "white", high = "burlywood4", name = "Mean Hours - PT", label = scales::comma) + 
  labs(title = "Mean Hours Worked Per Week of Part-Time Coffee Workers")+ 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right")
map5



#####Regressions#####
#FULL TIME
#Wage ~ Shops
reg1a <- lm(meanwagefull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1a) 

reg1b <- lm(meanwagefull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1b) 

reg1c <- lm(meanwagefull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg1c) 

reg1d <- lm(meanwagefull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg1d) 

export_summs(reg1a, reg1b, reg1c, reg1d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Wage - FT.xlsx")  #Think outreg2 

#Income ~ Shops
reg2a <- lm(meanincomefull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2a) 

reg2b <- lm(meanincomefull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2b) 

reg2c <- lm(meanincomefull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg2c) 

reg2d <- lm(meanincomefull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg2d) 

export_summs(reg2a, reg2b, reg2c, reg2d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Income - FT.xlsx")  #Think outreg2 

#Hours ~ Shops
reg3a <- lm(meanhoursfull ~ Shops, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3a) 

reg3b <- lm(meanhoursfull ~ Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3b) 

reg3c <- lm(meanhoursfull ~ Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE) 
summary(reg3c) 

reg3d <- lm(meanhoursfull ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedfull, na.rm=TRUE)
summary(reg3d) 

export_summs(reg3a, reg3b, reg3c, reg3d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Hours - FT.xlsx")  #Think outreg2 


#PART TIME
#Wage ~ Shops
reg4a <- lm(meanwagepart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4a) 

reg4b <- lm(meanwagepart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4b) 

reg4c <- lm(meanwagepart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg4c) 

reg4d <- lm(meanwagepart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg4d) 

export_summs(reg4a, reg4b, reg4c, reg4d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Wage - PT.xlsx")  #Think outreg2 

#Income ~ Shops
reg5a <- lm(meanincomepart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5a) 

reg5b <- lm(meanincomepart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5b) 

reg5c <- lm(meanincomepart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg5c) 

reg5d <- lm(meanincomepart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg5d) 

export_summs(reg5a, reg5b, reg5c, reg5d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Income - PT.xlsx")  #Think outreg2 

#Hours ~ Shops
reg6a <- lm(meanhourspart ~ Shops, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6a) 

reg6b <- lm(meanhourspart ~ Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6b) 

reg6c <- lm(meanhourspart ~ Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE) 
summary(reg6c) 

reg6d <- lm(meanhourspart ~ Shops*Avg_Price + Shops + Avg_Price, data = meanwagemergedpart, na.rm=TRUE)
summary(reg6d) 

export_summs(reg6a, reg6b, reg6c, reg6d, coefs = c("Shops per 100k" = "Shops", "Average Price of Coffee" = "Avg_Price", "Shops * Avg Price" = "Shops:Avg_Price", "Intercept" = "(Intercept)"), to.file="xlsx", file.name = "~/desktop/Mean Hours - PT.xlsx")  #Think outreg2 

#####Coffee Over Time#####
#Data can be downloaded from: https://ico.org/new_historical.asp
#You will need to "clean" the excel files and remove extra rows and such... 
#Import and Clean the Data
coffee_prices <- read_excel("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/3b - Retail Prices.xlsx") 
coffee_imports <- read_excel("~/desktop/SICSS-NDSU/Group Projects/Coffee Project/Data/2b - Imports.xlsx")

coffee_prices <- rename(coffee_prices, country = '...1')
coffee_imports <- rename(coffee_imports, country = '...1')

coffee_prices_long <- coffee_prices %>% 
  gather(year, value, -c(country))

coffee_imports_long <- coffee_imports %>% 
  gather(year, value, -c(country))

coffee_prices_long <- rename(coffee_prices_long, prices = 'value') 

coffee_imports_long <- rename(coffee_imports_long, bags = 'value') 

#Merge the two Data Sets
coffee_long <- left_join(coffee_prices_long, coffee_imports_long) 

#Visualize
ggplot(filter(coffee_long, country %in% c("Japan", "United States of America", "Italy")), mapping=aes(x=year, y=prices, group=country, color=country)) + 
  geom_line() + 
  xlab("Year") + 
  ylab("Price of Raw Coffee Beans") + 
  scale_color_discrete(name = "Country")


ggplot(filter(coffee_long, country %in% c("Japan", "United States of America", "Italy")), mapping=aes(x=year, y=bags, group=country, color=country)) + 
  geom_line() + 
  xlab("Year") + 
  ylab("1000s of 60-pound bags imported") + 
  scale_color_discrete(name = "Country")