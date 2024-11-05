# Alvaro Sanchez , Worldclouds and comparison 

# R script for Topic Models (thanks to Achim Edelmann & Rudi Farys for script templates) 

#install packages
# install.packages(c("stringr", "dplyr", "readtext", "quanteda"))
# install.packages("bit64")
# install.packages("ggplot2")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("stm")

## load libraries
library(stringr)
library(dplyr)
library(readtext)
library(quanteda)
library(bit64)
library(ggplot2)
library(tibble)

setwd("C:/Users/vicsg/OneDrive/Desktop/Buddah vs. Bible Project")




buddah <- readtext("C:/Users/vicsg/OneDrive/Desktop/Buddah vs. Bible Project/The_Gospel_of_Buddha.txt") 

#texte <- readtext("The_Gospel_of_Buddha", text_field = "text", encoding = "UTF-8")


view(buddah)

#convert date
#texte$date <- as.Date(texte$date, format = "%d.%m.%Y");
#texte$year <- lubridate::year(texte$date)

buddah_corpus <- corpus(buddah, docid_field = "doc_id", text_field = "text")
view(buddah_corpus)


# summary
summary(buddah_corpus) %>% head

# inspect the first documents
buddah_corpus[1:0]

# document level variables
docvars(buddah_corpus) %>% head
#metadoc(buddah_corpus, "language") <- "German"
#metadoc(buddah_corpus, "origin") <- "Keaggle"
#metadoc(buddah_corpus) %>% head

# first steps
summary(buddah_corpus, showmeta = TRUE) %>% head


# Aufbereitung
toks <- tokens(buddah_corpus,
               what = c("word"),
               remove_separators = TRUE,
               include_docvars = TRUE,
               ngrams = 1L,
               remove_numbers = TRUE, #NO numbers 
               remove_punct = TRUE,
               remove_symbols = TRUE, #NO symbols
               remove_hyphens = FALSE,
               remove_url = TRUE #NO URLs
)



toks %>% head

## Cleaning
# Satzzeichen
# toks <- toks %>% 
#  tokens_remove(pattern = "^[[:punct:]]+$", # regex for toks consisting solely of punct class chars
#                valuetype = "regex",
#                padding = TRUE) 
# stopwords
toks <- toks %>% 
  tokens_remove(stopwords("German"), padding = TRUE) 
toks <- toks %>% 
  tokens_remove(stopwords("English"), padding = TRUE) 
toks[1:1]
# remove empty token
toks <- toks %>% 
  tokens_remove("")
toks[1:1]
# remove custom stopwords

#toks   # Ansicht der STOPWORDS

toks <- toks %>% 
  tokens_remove(c("thou","thy", "_skt", "said","thus","upon","must", "_p", "o","p","thee"))

toks[1:1]

# create document-term-matrix (quanteda: document-feature-matrix)
mydfm <- dfm(toks)
str(mydfm)

#

kwic(buddah_corpus, "one", 3) %>% head(20)


# quick overview
topfeatures(mydfm, 50)

top_words <-
  topfeatures(mydfm, 20) %>% 
  data.frame(word = names(.), freq = ., row.names = c())

library(ggplot2)
freq_tokens <-
  ggplot(top_words, aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens


plot(freq_tokens)




## topic models
library(topicmodels)
library(tm)



dtm <- convert(mydfm, to = "topicmodels")
mode(dtm)
class(dtm)
str(dtm)

# trimming of DTM
dtm <- 
  mydfm %>% 
  dfm_trim(., sparsity = 0.999) %>% 
  convert(., to = "topicmodels")

# check
as.matrix(dtm)[1:1,1:1]
as.matrix(mydfm)[1:1,1:1]
# freq words using tm-function
tm::findFreqTerms(dtm, 40)

# remove empty docs
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ] 
dtm


kwic(toks, "blessed", 3) %>% head(20)
kwic(toks, "truth", 3) %>% head(20)
kwic(toks, "buddha", 3) %>% head(20)
kwic(toks, "life", 3) %>% head(20)
kwic(toks, "people", 3) %>% head(20)

##  wordcould 
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)


#if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
#
#if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
#
install.packages("quanteda")
#

wordcloud(toks, max_words = 50, random.order = FALSE, colors = brewer.pal(6, "Dark2"), min_size = 1, max_size = 10 )## Warning: scale is deprecated; use min_size and max_size instead
#

wordcloud(toks, max_words = 30, random.order = FALSE, colors = brewer.pal(4, "Greens"), min_size = 1, max_size = 10, size = 0.7, shape = 'pentagon') ## Warning: scale is deprecated; use min_size and max_size instead
#

------------------------------------------------------------------------------------------------------
  
# Now some goes for the King James Bible
  
  
  
  KJ_bible <- readtext("C:/Users/vicsg/OneDrive/Desktop/Buddah vs. Bible Project/The_King_ James_Bible.txt") 

#texte <- readtext("The_Gospel_of_Buddha", text_field = "text", encoding = "UTF-8")


view(KJ_bible)

#convert date
#texte$date <- as.Date(texte$date, format = "%d.%m.%Y");
#texte$year <- lubridate::year(texte$date)

KJ_bible_corpus <- corpus(KJ_bible, docid_field = "doc_id", text_field = "text")
view(KJ_bible_corpus)


# summary
summary(KJ_bible_corpus) %>% head

# inspect the first documents
KJ_bible_corpus[1:0]

# document level variables
docvars(KJ_bible_corpus) %>% head
#metadoc(KJ_bible_corpus, "language") <- "German"
#metadoc(KJ_bible_corpus, "origin") <- "Keaggle"
#metadoc(KJ_bible_corpus) %>% head

# first steps
summary(KJ_bible_corpus, showmeta = TRUE) %>% head


# Aufbereitung
toks2 <- tokens(KJ_bible_corpus,
               what = c("word"),
               remove_separators = TRUE,
               include_docvars = TRUE,
               ngrams = 1L,
               remove_numbers = TRUE, #NO numbers 
               remove_punct = TRUE,
               remove_symbols = TRUE, #NO symbols
               remove_hyphens = FALSE,
               remove_url = TRUE #NO URLs
)



toks2 %>% head

## Cleaning
# Satzzeichen
# toks <- toks %>% 
#  tokens_remove(pattern = "^[[:punct:]]+$", # regex for toks consisting solely of punct class chars
#                valuetype = "regex",
#                padding = TRUE) 
# stopwords
toks2 <- toks2 %>% 
  tokens_remove(stopwords("German"), padding = TRUE) 
toks2 <- toks2 %>% 
  tokens_remove(stopwords("English"), padding = TRUE) 
toks2[1:1]
# remove empty token
toks2 <- toks2 %>% 
  tokens_remove("")
toks2[1:1]
# remove custom stopwords

#toks   # Ansicht der STOPWORDS

toks2 <- toks2 %>% 
  tokens_remove(c("thou","thy", "_skt", "said","thus","upon","must", "_p", "o","p","thee","ye","let","come","came","unto","shalt","shall"))

toks2[1:1]

# create document-term-matrix (quanteda: document-feature-matrix)
mydfm2 <- dfm(toks2)
str(mydfm2)

#

#kwic(buddah_corpus, "Buddah", 3) %>% head(20)


# quick overview
topfeatures(mydfm2, 50)

top_words2 <-
  topfeatures(mydfm2, 20) %>% 
  data.frame(word = names(.), freq = ., row.names = c())

library(ggplot2)
freq_tokens <-
  ggplot(top_words2, aes(x=reorder(word, freq), y=freq)) +
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Most frequent tokens") 
freq_tokens


plot(freq_tokens)




## topic models
library(topicmodels)
library(tm)



dtm2 <- convert(mydfm2, to = "topicmodels")
mode(dtm2)
class(dtm2)
str(dtm2)

# trimming of DTM
dtm2 <- 
  mydfm2 %>% 
  dfm_trim(., sparsity = 0.999) %>% 
  convert(., to = "topicmodels")

# check
as.matrix(dtm2)[1:1,1:1]
as.matrix(mydfm2)[1:1,1:1]
# freq words using tm-function
tm::findFreqTerms(dtm2, 40)

# remove empty docs
dtm2 <- dtm[rowSums(as.matrix(dtm2)) > 0, ] 
dtm2


kwic(toks2, "lord", 5) %>% head(20)
kwic(toks2, "god", 3) %>% head(20)
kwic(toks2, "israel", 3) %>% head(20)
kwic(toks2, "king", 3) %>% head(20)
kwic(toks2, "people", 3) %>% head(20)

##  wordcould 
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)


#if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
#
#if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
#
install.packages("quanteda")
#

wordcloud(toks2, max_words = 50, random.order = FALSE, colors = brewer.pal(6, "Accent"), min_size = 1, max_size = 10 )## Warning: scale is deprecated; use min_size and max_size instead
#

wordcloud(toks2, max_words = 30, random.order = FALSE, colors = brewer.pal(4, "Reds"), min_size = 1, max_size = 10, size = 0.7, shape = 'pentagon') ## Warning: scale is deprecated; use min_size and max_size instead
#
  
  