#'========================================================================
#'========================================================================
#' PROJECT:
#'   Analysis of public comments on the use of facial recognition
#'     and AI technologies by DHS
#' BY:
#'   Kurt Williams
#'   Sara Bano
#'   Mir Rabby
#'   Chetna Kuanr
#' DATE:
#'   SICSS-NDSU 2022, Jun 19-30, 2022
#'========================================================================
#'========================================================================

#' Workflow:
#' 1. Scrape the comments from regulations.gov
#' 2. Clean the comments data for text analysis
#' 3. Construct and summarize the corpus
#' 4. Topic modeling
#' 5. Aspect-based sentiment analysis

# ========================================================================

# Scrape the comments from regulations.gov ----

# Load required packages
library(tidyverse) # for general wrangling
library(httr) # for HTTP
library(tidyjson) # for parsing the JSON output

## Setting up initial query

api_key <- read_file("../reg-gov-api-key.txt") # Read in your API key: replace with the path to the file where your API key is saved
docketID <- "DHS-2021-0015" # this is the docketID for the comments on regulations.gov

reg_request <- GET(
  url = paste0("https://api.regulations.gov/v4/documents?", # the base url for the query
               "filter[docketId]=", docketID, # filtering by docketID
               "&api_key=", api_key),
  config = config(ssl_verifypeer = FALSE) # I needed to do this to get httr to work, don't know why
)

## Check that the request went through and check the url:

http_status(reg_request) # check status
reg_request$url # check url

## Get the objectId for each of the retrieved documents

stringi::stri_enc_detect(content(reg_request, "raw")) # detect encoding
reg_json <- content(reg_request, "text", encoding = "ISO-8859-1") %>% # extract content
  as.tbl_json() # store json as tbl

# The following code extracts the objectId from the JSON for each document
# I pieced this together and don't fully know what each part does
reg_docs <- spread_values(reg_json) %>% 
  enter_object("data") %>% 
  gather_array() %>% 
  enter_object("attributes") %>%
  spread_values(objectId = jstring("objectId"))

## Get comments for each document

reg_comments_request <- list() # Initialize the list to store the results from each document
for (i in 1:nrow(reg_docs)) { # For each document...
  reg_comments_request[[i]] <- GET( # make an API request...
    url = paste0("https://api.regulations.gov/v4/comments?", # base url
                 "filter[commentOnId]=", reg_docs$objectId[i], # filter by objectId
                 "&page[size]=220", # expand the number of results to 220 (to fit all results on 1 page)
                 "&api_key=", api_key), # add API key
    config = config(ssl_verifypeer = FALSE)
  )
  Sys.sleep(time = 5) # give the API a break between requests to be polite
}

reg_comments_request # check the output

## Get comments from request

reg_comments_request[[2]]$url # check the url
stringi::stri_enc_detect(content(reg_comments_request[[1]], "raw")) # detect encoding
reg_comments_json <- content(reg_comments_request[[2]], "text", encoding = "ISO-8859-1") %>% as.tbl_json() # Save JSON as tbl

## Get comment IDs from JSON
reg_commentIDs <- spread_values(reg_comments_json) %>%
  enter_object("data") %>%
  gather_array() %>%
  spread_values(commentID = jstring("id"))

## Request comment details to get comment text

reg_comments_details <- list() # Initialize list to store all the comments
for (i in 1:nrow(reg_commentIDs)) { # For each commentID...
  reg_comments_details[[i]] <- GET( # make an API request...
    url = paste0("https://api.regulations.gov/v4/comments/", # base url
                 reg_commentIDs$commentID[i],"?", # filtering by commentID
                 "include=attachments", # include attachments in case some commenters uploaded pdf or word documents
                 "&api_key=", api_key), # add API key
    config = config(ssl_verifypeer = FALSE)
  )
  Sys.sleep(time = 1.3) # Give the API a break between requests to be polite
}
beepr::beep("treasure") # Make a sound when this line is reached (the requests are done)


reg_comments_details[[220]]$url # check the request url
reg_comments_details[[1]]$url # check the request url

headers(reg_comments_details[[220]])$`x-ratelimit-remaining` # check the number of requests left this hour, given API limits

## Extract the text of the comment from the request

# Prototyping the function to extract the text on a single comment
# content(reg_comments_details[[1]], "text", encoding = "ISO-8859-1") %>%
#   as.tbl_json() %>%
#   spread_values(commentID = jstring("data","id"),
#                 commentTitle = jstring("data","attributes","title"),
#                 commentText = jstring("data","attributes","comment")) %>%
#   enter_object("included") %>%
#   gather_array(column.name = "included.index") %>%
#   spread_values(attachID = jstring("id"),
#                 attachments = jstring("type"))

# Extract the text from all the comments
comments_data <- map_dfr(reg_comments_details, function(x) { # For all the elements of `reg_comments_details`, do the function and bind the results into a dataframe by rows
  content(x, "text", encoding = "ISO-8859-1") %>%
    as.tbl_json() %>%
    spread_values(commentID = jstring("data","id"),
                  commentTitle = jstring("data","attributes","title"),
                  commentText = jstring("data","attributes","comment"))
})

comments_data$commentText # check that the text was successfully extracted

saveRDS(comments_data, file = "../data/comments_scraped.rds") # save the scraped comments as an RDS object for easy loading into R later
write_csv(comments_data, file = "../data/comments_scraped.csv") # save the scraped comments as a csv object for easy manual editing later

# Clean the comments data for text analysis ----
#' Comments including pdf attachments had to be cleaned manually to ensure complete extraction of the comment text
#' The cleaned comments were re-loaded into R from csv

## Required packages for corpus construction & cleaning
library(quanteda)

## Read in the data
comments_data <- read_csv("../data/pdfcomments_scraped.csv") # replace with your path to the file

## Remove problematic characters
comments_data$text_cleaned <- gsub(pattern="<.*?>", replacement="", x=comments_data$commentText) %>% # remove <br>
  gsub(pattern="&#39;", replacement="", x=.) %>% # remove special characters
  gsub(pattern="&quot;", replacement = "", x=.) %>%
  gsub(pattern="&rsquo;", replacement = "", x=.) %>%
  gsub(pattern="&ldquo;", replacement = "", x=.) %>%
  gsub(pattern="&rdquo;", replacement = "", x=.) %>%
  gsub(pattern = "\\n", replacement = " ", x=.) %>% # remove \n line breaks
  gsub(pattern = "[\x80-\xff]", replacement = "", x=.) %>% # remove hex strings (from pdfs?)
  gsub(pattern = "�", replacement = "", x=.) # remove special characters

write_csv(comments_data, "../data/comments_cleaned.csv") # save the cleaned comments for later use

# Construct corpus object ----
comments_corpus <- corpus(comments_data$text_cleaned, docnames = comments_data$commentID)

## Tokenize
comments_tokens <- tokens(
  x = comments_corpus,
  what = "word",
  remove_punct = TRUE
) %>%
  tokens_remove(stopwords("english"))

## Check the most common features
dfm(comments_tokens,
    tolower = TRUE) %>% 
  topfeatures(n = 50)

# Topic modeling ----
## Required packages
library(stm)

## Convert to dfm
comments_dfm <- dfm(comments_tokens,
                    tolower = TRUE)
## Prepare stm data
comments_stm <- convert(comments_dfm, to="stm")
out <- prepDocuments(comments_stm$documents,
                     comments_stm$vocab,
                     comments_stm$meta,
                     lower.thresh = 10) # filter out terms that occur less than 10 times

## Search K for number of topics
set.seed(20220628)
storage <- searchK(out$documents, out$vocab,
                   K = c(3:10),
                   data = out$meta)
beepr::beep() # Make a sound when `searchK()` ends
plot(storage)
storage

# Check the 7-topic solution
model <- stm(out$documents, out$vocab,
             K=7,
             max.em.its = 400,
             data = out$meta,
             init.type = "Spectral")

labels <- labelTopics(model, n = 20)
topwords <- data.frame("features" = t(labels$frex))
colnames(topwords) <- paste("Topics", c(1:7))
topwords[1:7]

## Topic comparison
plot(model, 
     type="perspectives", 
     topics=c(3,5), 
     plabels = c("Topic 3","Topic 5"))

topic_indices <- combn(x=1:7, m=2)
for (i in 1:ncol(topic_indices)) {
  plot(model,
       type = "perspectives",
       topics=topic_indices[,i],
       plabels=paste0("Topic ",topic_indices[,i]))
}

# Aspect-based sentiment analysis ----
## Required packages
library(udpipe)
library(tidytext)

?udpipe # Check: what does the `udpipe` function do?

## Read in the data !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
comments_data <- read_csv("data/comments_cleaned.csv")

## Dependency parsing using `udpipe`
#' Language options for english:
#' - 'english-ewt' based on web media (blogs, newsgroups, emails, reviews, Yahoo! answers)
#' - 'english-gum' based on Georgetown University Multilayer corpus, using sources found by Georgetown students for a class
#' - 'english-lines' "the English half of the LinES Parallel Treebank with UD annotations", from literature, online manuals, Europarl data
#' Because we are analyzing reviews from the general public, the 'english-ewt' treebank I think will give the best performance
comments_data$doc_id <- comments_data$commentID
comments_data$text <- comments_data$text_cleaned
comments_udpipe <- udpipe(x = comments_data,
                          object = "english-ewt")

## Let's use Bing sentiment dictionary
comments_sentiment <- comments_udpipe %>%
  mutate(word = token) %>%
  left_join(y = get_sentiments(lexicon="bing"), by="word") %>%
  rename(bing_sentiment = sentiment)

## Extract relationships and sentiment
reasons <- cbind_dependencies(comments_sentiment) %>%
  select(doc_id, 
         lemma, token, upos, bing_sentiment, 
         token_parent, lemma_parent, upos_parent, dep_rel) %>%
  filter(!is.na(bing_sentiment))

reasons_amod <- filter(reasons, dep_rel %in% "amod") # look only at modifiers
## Get edges
word_cooccurences <- reasons_amod %>%
  group_by(lemma, lemma_parent) %>%
  summarise(cooc = n()) %>%
  arrange(-cooc)

## Get vertices
vertices <- tibble(
  key = unique(c(word_cooccurences$lemma, word_cooccurences$lemma_parent)),
  in_dictionary = ifelse(key %in% get_sentiments("bing")$word, "in_dictionary","linked_to")
) %>%
  left_join(get_sentiments("bing") %>% rename(key=word)) %>%
  left_join(count(comments_udpipe, lemma) %>% rename(key=lemma))

## Visualization
library(ggraph)
library(igraph)

cooc <- head(word_cooccurences, 20)
set.seed(20220628)
cooc %>%
  graph_from_data_frame(vertices = filter(vertices, key %in% c(cooc$lemma, cooc$lemma_parent))) %>%
  ggraph(layout = "fr") +
  geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
  geom_node_point(aes(color = in_dictionary), size = 5) +
  geom_node_text(aes(label = name), vjust = 1, col = "darkgreen") +
  theme_void()

sentiment_graph <- word_cooccurences %>%
  graph_from_data_frame(vertices = vertices)

#plot(sentiment_graph)
sentiment_communities <- walktrap.community(sentiment_graph)
sentiment_communities

sentiment_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
  geom_node_point(aes(color = sentiment, size = n)) +
  #geom_node_text(aes(label = name), vjust = 1, col = "darkgreen") +
  coord_fixed() +
  theme_void()

# subgraph of degree > 1
sentiment_degree <- degree(sentiment_graph)
high_degree_nodes <- V(sentiment_graph)[sentiment_degree > 3]
high_degree_neighbors <- neighborhood(sentiment_graph, nodes = high_degree_nodes, order = 3) %>% 
  unlist() %>% 
  unique()

set.seed(20220629)
svg("absa-graph.svg")
induced_subgraph(sentiment_graph, vids = high_degree_neighbors) %>%
  ggraph(layout = "fr") +
  geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
  geom_node_point(aes(color = sentiment, size = n), alpha = 0.8) +
  #geom_node_text(aes(label = name), vjust = 1, col = "darkgreen") +
  #ggtitle("Which words are linked to the negative terms") +
  scale_edge_width(name = "Co-occurrence") +
  scale_edge_alpha(name = "Co-occurrence") +
  scale_size(name = "Term Frequency", range = c(3,20)) +
  scale_color_discrete(name = "Sentiment (Bing)", type = c("#b85741","#009384")) +
  coord_fixed() +
  theme_void()
dev.off()

sentiment_degree %>% quantile(probs=c(0.9, 0.8))
vertices$degree <- sentiment_degree
# High degree words
map(list("negative", "positive", NA),
    function(x) if(is.na(x)) {filter(vertices, is.na(sentiment)) %>% arrange(desc(degree))} else {filter(vertices, sentiment == x) %>% arrange(desc(degree))})
# negative: criminal [14], bad [9], false [8], negative [7], authoritarian [6], wrong [6], violation [6], invasive [5], biased [5], limited [5]
# positive: great [15], clear [13], free [9], better [9], accurate [7], reasonable [6], right [6], good [5], available [5], secure [5]
# neutral: rate [8], tool [8], idea [7], technology [7], information [6], result [6], person [5], people [5], regime [4], system [4]

# High frequency words
map(list("negative", "positive", NA),
    function(x) if(is.na(x)) {filter(vertices, is.na(sentiment)) %>% arrange(desc(n))} else {filter(vertices, sentiment == x) %>% arrange(desc(n))})
# negative: criminal, violation, risk
# positive: right, great, positive, better, accurate
# neutral: recognition, technology, data, government, privacy

# ego-centric networks
make_ego_graph(sentiment_graph, nodes = c("criminal","violation","risk","biased",
                                          "right","great","positive","better","accurate",
                                          "recognition","technology","data","government","privacy","dark"), order = 3) %>%
  map(function(x) {
    x %>%
      ggraph(layout = "fr") +
      geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
      geom_node_point(aes(color = sentiment, size = n), alpha = 0.8) +
      geom_node_text(aes(label = name), vjust = 1, col = "black") +
      scale_edge_width(name = "Co-occurrence") +
      scale_edge_alpha(name = "Co-occurrence") +
      scale_size(name = "Term Frequency", range = c(3,20)) +
      scale_color_discrete(name = "Sentiment (Bing)", type = c("#b85741","#009384")) +
      coord_fixed() +
      theme_void()
  })

# selected ego-centric networks
make_ego_graph(sentiment_graph, nodes = c("great","clear","government","recognition","criminal","violation","biased"), order = 3) %>%
  map(function(x) {
    set.seed(20220629)
    x %>%
      ggraph(layout = "fr") +
      geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
      geom_node_point(aes(color = sentiment, size = n), alpha = 0.8) +
      geom_node_text(aes(label = name), vjust = 1, col = "black") +
     scale_edge_width(name = "Co-occurrence", limits = c(1,10), breaks = c(5,10)) +
      scale_edge_alpha(name = "Co-occurrence", limits = c(1,10), breaks = c(5,10)) +
      scale_size(name = "Term Frequency", range = c(3,20)) +
      scale_color_discrete(name = "Sentiment (Bing)", type = c("#b85741","#009384")) +
      coord_fixed() +
      theme_void()
  })

comments_sentiment %>% count(bing_sentiment) # overall number of negative and positive tokens

# ========================================================================