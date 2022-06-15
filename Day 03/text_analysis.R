# Using Quanteda for text analysis
# https://github.com/quanteda/quanteda
# Excellent summary: https://quanteda.io/articles/pkgdown/quickstart.html

library(quanteda)
library(devtools)
devtools::install_github("quanteda/quanteda.corpora")
devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("quanteda/quanteda.textplots")
devtools::install_github("quanteda/quanteda.textstats")
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.sentiment)
library(spacyr)

# Quanteda can import a variety of text formats directly, from .txt files to JSON from the Twitter API, using the `corpus` command.
# Here, we'll use a built-in corpus of all inaugural addresses by U.S. presidents.

inaugural <- data_corpus_inaugural


# Let's take a look inside:
View(summary(inaugural))


# ... and create tokens:
# Q: what are tokens in NLP?
tokens <- tokens(
  subset)

clean_tokens <- tokens(
  inaugural,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

# Create word frequency matrix; try with and without stopwords
data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english'))


# Less sparse
data_freq_matrix <- dfm_trim(data_freq_matrix,
                             min_docfreq = 0.075,
                             max_docfreq = 0.90,
                             docfreq_type = "prop"
) 


# Visualize keyword in context
kwic <- kwic(clean_tokens, pattern = "war")

subset <- corpus_subset(inaugural, Year > 2000)
kwic(tokens(subset), pattern="war") %>% textplot_xray()


# Visualize frequency
# Let's go back and create a DFM from this subset

features <- textstat_frequency(data_freq_matrix, n=30)
View(features)

features$feature <- with(features, reorder(feature, -frequency))

ggplot(features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Keyness
t_o_subset <- corpus_subset(subset, President %in% c('Obama', 'Trump'))
t_o_dfm <- tokens(t_o_subset, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = President) %>%
  dfm()

keyness <- textstat_keyness(t_o_dfm, target='Trump')
textplot_keyness(keyness)
# Remember what precisely 'keyness' refers to?


# Feature co-occurence matrix
# For when we want to compare how often terms co-occur in a given document
# Look into the 'window' parameter
obama_subset <- corpus_subset(t_o_subset, President %in% 'Obama')
obama_dfm <- tokens(obama_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
obama_fcm <- fcm(obama_dfm)
obama_fcm

trump_subset <- corpus_subset(t_o_subset, President %in% 'Trump')
trump_dfm <- tokens(trump_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
trump_fcm <- fcm(trump_dfm)
trump_fcm

pro_tags <- dfm_select(trump_dfm, pattern="pro*")
toptag <- names(topfeatures(pro_tags, 50))
toptag

pro_tags_fcm <- fcm(pro_tags)
pro_tags_fcm

textplot_network(pro_tags_fcm)
#contrast Trump to Obama fcm network plots
#what could this dfm_select feature be really useful for?


# Cosine similarity
obama_simil <- textstat_simil(data_freq_matrix, data_freq_matrix[c("2009-Obama" , "2013-Obama"),], 
                              margin = "documents", method = "cosine")
obama_simil


# Sentiment, Basic
textstat_polarity(inaugural, dictionary=data_dictionary_LSD2015)
# What do you observe?


# For more advanced sentiment analysis (and POS/dependency tagging), consider using the spacyR package.
# It's a little tricky to install because of its dependency on python, but well worth it.