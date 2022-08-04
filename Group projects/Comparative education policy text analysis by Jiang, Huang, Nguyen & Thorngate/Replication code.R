

###Group Project
## Load packages
library(pdftools)
library(stringr)
library(dplyr)

### Two Acts

# for NCLB
Nochild1 <- pdf_text("PLAW-107publ110.pdf")
res<-""
NoChildFile<-file("Nochild.txt")
for (i in seq_along(Nochild1)){
  res<-paste0(res,Nochild1[i])
  writeLines(res,NoChildFile)
}
res<-str_replace_all(res,"\n","")
res<-str_replace_all(res,"\\s+", "")
close(NoChildFile)


#for ESSA
Nochild <- pdf_text("PLAW-114publ95.pdf")
res1<-""
NoChildFile<-file("Pub.txt")
for (i in seq_along(Nochild)){
  res1<-paste0(res1,Nochild[i])
  writeLines(res1,NoChildFile)
}
res1<-str_replace_all(res1,"\n","")
res1<-str_replace_all(res1,"\\s+", "")
close(NoChildFile)

library(tm)
library(tidytext)
library(ggplot2)
corpus<-Corpus(DirSource(mode="text"))
corpus
writeLines(head(strwrap(corpus[[1]]),5))
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
morestop<-readLines("morestop.txt")
#cleaning more corpus
mycorpus <- tm_map(corpus,content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removeWords, english_stopwords)
mycorpus <- tm_map(mycorpus, removeWords, morestop)
mycorpus <- tm_map(mycorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
mycorpus <- tm_map(mycorpus, removeNumbers)
mycorpus <- tm_map(mycorpus, stemDocument, language = "en")
mycorpus <- tm_map(mycorpus, stripWhitespace)

writeLines(head(strwrap(mycorpus[[1]]),5))

#Document Term Matrix
dtm<-DocumentTermMatrix(mycorpus)
inspect(dtm)

#Word Freqency
words_frequency<-colSums(as.matrix(dtm))
length(words_frequency)

ord<-order(words_frequency, decreasing=TRUE)

#get the top 10 words by fequency of appearance
words_frequency[head(ord,10)]%>%
  kable()
# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result)


#Return undordered frequent terms that appeared more than 200, but less than infinity
findFreqTerms(dtm, lowfreq = 200, highfreq = Inf)
#Topic model
library(topicmodels)
library(lda)
k<-8
set.seed(9161)
#compute the LDA model, inference via 1000 interations of Gibbs sampling
topicModel<-LDA(dtm, k, method="Gibbs", control=list(iter=500, verbose=25))
topicModel
#Look at the result
#posteria distributions
tmResult<-posterior(topicModel)

attributes(tmResult)
nTerms(dtm)
beta<-tmResult$terms
dim(beta)
library(clipr)
library(report)
terms(topicModel, 10)

writeClipboard(terms(topicModel,10))
top5termsPerTopic<-terms(topicModel,4)
top5termsPerTopic
#Give names for each topic using the five most likely terms of each topic to a string
topicNames<-apply(top5termsPerTopic,2, paste,collapse=" ")
topicNames
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nDocs(dtm)               # size of collection

###Creat TF:IDF Visual
df_corpus <- tidy(dtm)
DocumentTermMatrix(mycorpus, control = list(weighting = weightTfIdf)) -> dtm_tfidf

# View details of tfidf weighted dtm
dtm_tfidf
# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
bind_tf_idf(df_corpus, term, document, count) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(term, levels = rev(unique(term))),
         chapter = factor(document)) %>%  
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in public policies by the two documents",
       x = "Words", y = "tf-idf") +
  facet_wrap(~chapter, ncol = 2, scales = "free") +
  coord_flip()

# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10]     # rows in theta sum to 1

library(kableExtra)
library(pals)
library(wordcloud)

# visualize topics as word cloud
topicToViz <- 8 # change for your own topic of interest
topicToViz <- grep('equity', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

#Topic Distributions
exampleIds<-c(1,2)
lapply(mycorpus[exampleIds], as.character)
library(reshape2)
theta<-tmResult$topics
N<-length(exampleIds)
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)






### Media Coverage

library(quanteda)
library(devtools)
library(tidyverse)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.sentiment)
library(spacyr)
library(pdftools)
library(stm)

#Read text from files into lists - Note: files are not provided here beacuse they are subscription content
setwd("C:/SICCS2022/final/lexisnews/nclb")
nclb_files <- list.files(pattern = "pdf$")
nclb_list <- lapply(nclb_files, pdf_text)

setwd("C:/SICCS2022/final/lexisnews/essa")
essa_files <- list.files(pattern = "pdf$")
essa_list <- lapply(essa_files, pdf_text)


#create dataframes
nclb_text <- vector("character", length(nclb_list)) 


nclb_df <- data.frame(text = nclb_text)

nclb_df$bill <- vector("character", length(nclb_list))

essa_text <- vector("character", length(essa_list)) 

essa_df <- data.frame(text = essa_text)

essa_df$bill <- vector("character", length(essa_list))
#collapse paginated list of text to one character item per obs, add year variable


for (i in seq_along(nclb_list)) {          
  nclb_df$text[[i]] <- paste(nclb_list[i], collapse=', ' )
  nclb_df$bill[[i]] <- "NCLB"
}

for (i in seq_along(essa_list)) {          
  essa_df$text[[i]] <- paste(essa_list[i], collapse=', ' )
  essa_df$bill[[i]] <- "ESSA"
}

#merge the two dataframes

lnews_df <- rbind(nclb_df, essa_df)


#clean text, you can tweak the codes below based on your research
clean_text <- function(x) {
  x %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove bill names
    str_replace_all("Every Student Succeeds Act", "") %>%
    str_replace_all("NCLB", "") %>%
    # Remove punctuation, using a standard character class
    str_remove_all("[[\\p{P}][\\p{S}]]") %>%
    # Remove punctuation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}


lnews_df$text<-lnews_df$text %>% clean_text


#tokenization

#create a corpus
lnewsCorpus <- corpus(lnews_df, text_field = "text")


summary(lnewsCorpus)


lnewsCorpus <- lnewsCorpus %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english")))


lnewsCorpus <- tokens(
  lnewsTokens,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

lnewsCorpus <- tokens_select(lnewsCorpus, c("education", "student", "child"), selection = "remove", padding = FALSE)



#pass covariates to corpus, you may select other features
docvars(lnewsCorpus, field = "bill") <- lnews_df$bill # covariance field month

#converting corpus to document frequency matrix (worth doing so if you see weird ones you can remove them)
lnewsDfm <- dfm(lnewsCorpus,
                tolower = TRUE,
                stem = TRUE,
                remove = stopwords("english"))

topfeatures(lnewsDfm, n = 100, scheme = "docfreq")



#prepare stm data (this changes it to structural topic model, which includes those docvars)
lnews.stm <- convert(lnewsDfm, to = "stm")
out <- prepDocuments(lnews.stm$documents, 
                     lnews.stm$vocab, 
                     lnews.stm$meta, 
                     lower.thresh = 10) 

#Topic modeling
#How do we decide how many topics?
#1.Statistical fit
#2.Interpretability and relevance of topics

#Let's find the optimal K, this will take a long time, be prepared
storage <- searchK(out$documents, out$vocab, K = c(3:10),
                   data=out$meta)

storage

#choose your model and run it
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
frexwords <- data.frame("features" = t(labels$frex))
colnames(frexwords) <- paste("Topics", c(1:5))
view(frexwords[1:5])

#and top words
topwords <- data.frame("features" = t(labels$prob))
colnames(topwords) <- paste("Topics", c(1:5))
view(topwords[1:5])

#plot topics (and label them)
par(bty="n",col="gray20",lwd=16)
plot(model, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Research",
                     "Gobbledygook",
                     "Politics",
                     "Accountability Metrics",
                     "Guidance and Support"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

#bill as covariate

#Structural topic modeling
out$meta$bill <- as.factor(out$meta$bill)
result1 <- estimateEffect(1:5 ~ bill, model,
                          meta = out$meta, uncertainty = "Global")
topiceffect1<-summary(result1, topics = c(1:5))
topiceffect1

#you can save the result: write.table(topiceffect$tables, 'output.csv', sep=",")

#distribution of specific topic by bill
par(bty="n",col="gray60",lwd=1)
plot(x = result1, 
     covariate = "bill", cov.value1 = "ESSA", cov.value2 = "NCLB",
     topics = c(1, 3, 4, 5), 
     model = model, 
     method = "difference",
     xlim = c(-0.5, 0.5), 
     labeltype = "custom", 
     custom.labels = c("Evidence", "Politics", "Accountability", "Guidance"), 
     main = c("Relative topic distribution in news coverage of ESSA compared to NCLB"))