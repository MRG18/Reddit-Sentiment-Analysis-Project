# Author: Michael Gryncewicz
# Date: 4/21/2018
# Version: 1
#
# Project:
#   
# The goal of this project was to apply various natural language 
# processing techniques to a corpus of documents. These techniques 
# consisted of producing word clouds based on various n-grams, using a 
# lexicon to apply sentiment to the documents, and running an emotional 
# analysis on the corpus. 
# 
# Data:
#   
# The data used in this project was taken from reddit.com. The data 
# consisted of various features from reddit posts made in February of 2018. 
# The full data contained about 10 million rows but a subset was used. 
# The data used was a subset by one single section of reddit, the technology 
# subreddit. The sentiment analysis was then performed on the titles of all 
# of these posts. There were about 10,000 posts used in the analysis.
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
# imports
options("java.home"="/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib")
Sys.setenv(LD_LIBRARY_PATH='$JAVA_HOME/server')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
library(rJava)
library(tidyr)
library(tidytext)
library(qdap)
library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(radarchart)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
# read in and clean data
setwd('/Users/MikeGryncewicz/Desktop/')
tech <- read.csv('reddit_technology_subset.csv')
term_count <- freq_terms(tech$title, 20)

clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c('technology', 'tech')
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stemDocument, language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  removeNonAscii <- content_transformer(function(x) gsub("[^\x20-\x7E]", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeNonAscii)
  return(cleaned_corpus)
}

tech_corpus <- VCorpus(VectorSource(tech$title))
cleaned_tech_corpus <- clean_corpus(tech_corpus)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### unigram wordcloud ###
# make term document matrix
TDM_tech <- TermDocumentMatrix(cleaned_tech_corpus)
TDM_tech_m <- as.matrix(TDM_tech)

# get term frequencies
term_frequency <- rowSums(TDM_tech_m)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# get top 20 most common words
top10 <- term_frequency[1:20]
# reorder top 20 for horizontal barplot
top10_ordered <- top10[order(top10, decreasing = FALSE)]
# barchart of the 20 most common words
barplot(top10_ordered, col = 'blue', las = 2, horiz = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# unigram wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, 
          max.words = 500, colors = brewer.pal(8, "Paired"))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### bigram wordcloud ###
# tokenizer for bigrams
tokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# bigram term document matrix
bigram_tdm <- TermDocumentMatrix(cleaned_tech_corpus, 
                                 control = list(tokenize = tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)
# get term frequencies
term_frequency <- rowSums(bigram_tdm_m)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
word_freqs[1:30,]
# bigram wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, max.words = 300,
          colors = brewer.pal(8, "Paired"))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### trigram wordcloud ###
# tokenizer for bigrams
tokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 3, max = 3))

# bigram term document matrix
trigram_tdm <- TermDocumentMatrix(cleaned_tech_corpus, 
                                 control = list(tokenize = tokenizer))
trigram_tdm <- as.matrix(trigram_tdm)
# get term frequencies
term_frequency <- rowSums(trigram_tdm)
# sort frequencies 
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
word_freqs[1:30,]
# bigram wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5, max.words = 300,
          colors = brewer.pal(8, "Paired"))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### tfidf wordcloud ###
# tfidf term document matrix
tfidf_tdm <- TermDocumentMatrix(cleaned_tech_corpus, 
                                control=list(weighting = weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)
# term frequencies
term_frequency <- rowSums(tfidf_tdm_m)
# sort term_frequency in descending order
term_frequency <- sort(term_frequency, dec = TRUE)
# word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num, min.freq = 5,
          max.words = 1000, colors=brewer.pal(8, "Paired"))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### sentiment analysis ###
# tidy tdm
TDM_tech <- TermDocumentMatrix(cleaned_tech_corpus)
tech_tidy <- tidy(TDM_tech)

# bing lexicon
bing_lex <- get_sentiments("bing")

# apply sentiment analysis with bing lexicon
tech_bing_lex <- inner_join(tech_tidy, bing_lex, by = c("term" = "word"))
tech_bing_lex$sentiment_n <- ifelse(tech_bing_lex$sentiment == "negative", -1, 1)
tech_bing_lex$sentiment_value <- tech_bing_lex$sentiment_n * tech_bing_lex$count
bing_aggdata <- aggregate(tech_bing_lex$sentiment_value, 
                          list(index = tech_bing_lex$document), sum)
bing_aggdata$index <- as.numeric(bing_aggdata$index)
colnames(bing_aggdata) <- c("index", "bing_score")

# sentiment bar chart for corpus
barplot(bing_aggdata$bing_score, names.arg = bing_aggdata$index)

# how many documents are postive and how many are negative?
bing_aggdata$pos_or_neg <- bing_aggdata$bing_score
bing_aggdata$pos_or_neg[bing_aggdata$bing_score > 0] <- 1
bing_aggdata$pos_or_neg[bing_aggdata$bing_score < 0] <- -1
bing_aggdata$pos_or_neg[bing_aggdata$bing_score == 0] <- 0
pos_neg_counts <- table(bing_aggdata$pos_or_neg)
barplot(pos_neg_counts, main="Distribution of Sentiment", 
        xlab="Positive, Neutral, or Negative")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### comparison/contrast word clouds ###
# get polarity
polarity <- counts(polarity(tech$title))[, "polarity"]
tech$polarity <- polarity

# split by polarity - pos vs neg
pos_tech <- tech[tech$polarity > 0, "title"]
neg_tech <- tech[tech$polarity < 0, "title"]
pos_tech <- paste(pos_tech, collapse = " ")
neg_tech <- paste(neg_tech, collapse = " ")
tech_pos_and_neg <- c(pos_tech, neg_tech)

# clean and make tdm
tech_pos_and_neg_corpus <- VCorpus(VectorSource(tech_pos_and_neg))
tech_pos_and_neg_cleaned <- clean_corpus(tech_pos_and_neg_corpus)
TDM_pol <- TermDocumentMatrix(tech_pos_and_neg_cleaned)
TDM_pol_m <- as.matrix(TDM_pol)

# commonality cloud
commonality.cloud(TDM_pol_m, colors = brewer.pal(8, "Dark2"), max.words = 400)

# comparison cloud
comparison.cloud(TDM_pol_m, colors = brewer.pal(8, "Dark2"), max.words = 400)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- #
### emotion analysis with NRC lexicon ###
# tidy tdm
tidy_tech <- tidy(TermDocumentMatrix(cleaned_tech_corpus))

# load NRC lexicon
nrc_lex <- get_sentiments("nrc")

# join corpus and NRC
tech_nrc <- inner_join(tidy_tech, nrc_lex, by = c("term" = "word"))

aggdata_nrc <- aggregate(tech_nrc$count, list(index = tech_nrc$sentiment), sum)
chartJSRadar(aggdata_nrc)

