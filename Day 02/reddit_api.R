# You can find API documentation here: https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf

library(tidyverse)
library(RedditExtractoR)


# Collect URLs of comment threads that contain a specific keyword
reddit_urls <- find_thread_urls(
  keywords="COVID", 
  subreddit="fargo", 
  period = "all")


# Iterate through URLs to pull content
content <- get_thread_content(reddit_urls$url)


# Get comment text data. Metadata availble under content$threads
comments <- content$comments


# Group comments by author: who posts most often about keyword?
# Q: what does the weird %>% symbol do?
by_author <- comments %>% group_by(author)
View(by_author)
View(by_author %>% tally(sort=TRUE))


# Which commenters are most popular?
by_upvote <- by_author %>% summarise(sum(upvotes))