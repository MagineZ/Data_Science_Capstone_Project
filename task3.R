###########################
### Tasks to accomplish ###
###########################
# 1. Build basic n-gram model - using the exploratory analysis you performed, 
# build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# 2. Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. 
# Build a model to handle cases where a particular n-gram isn't observed.

#############################
### Questions to consider ###
#############################
# 1. How can you efficiently store an n-gram model (think Markov Chains)?
# 2. How can you use the knowledge about word frequencies to make your model smaller and more efficient?
# 3. How many parameters do you need (i.e. how big is n in your n-gram model)?
# 4. Can you think of simple ways to "smooth" the probabilities 
# (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
# 5. How do you evaluate whether your model is any good?
# 6. How can you use backoff models to estimate the probability of unobserved n-grams?

###############################
### Tips, tricks, and hints ###
###############################
# 1. Size: the amount of memory (physical RAM) required to run the model in R
# 2. Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input
# 3. Here are a few tools that may be of use to you as you work on their algorithm:
# * object.size(): this function reports the number of bytes that an R object occupies in memory
# * Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. 
# The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
# * gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.

##############
### Script ###
##############
setwd('C:/Users/Pei-Chun/Documents/R/Coursera-SwiftKey')

rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
require(data.table)
require(qdap);require(rJava);
require(scales); require(gridExtra);
suppressMessages(library(LaF)) # to read large files as ASCII & sample while reading. Enhanced performance over `readLines`
suppressMessages(library(tm)) # for text mining and corpus operations
suppressMessages(library(SnowballC)) # for stemming root words
suppressMessages(library(ggplot2)) # for plotting
suppressMessages(library(RWeka)) # for tokenization. Need 64 bit java installed in you have a 64 bit machine.
suppressMessages(library(quanteda)) # for word frequency and sparsity exploratory analysis
suppressMessages(library(wordcloud)) # for additional exploratory analysis
suppressMessages(library(tau)) # Quick identification of n-words that frequent together.
suppressMessages(library(dplyr)) # for text cleaning and other mutations if needed
library(RColorBrewer) 

options( java.parameters = c("-Xmx8g","-XX:-UseGCOverheadLimit") ) 

## get raw data
en_US <- file.path('.','final','en_US')

con <- file("./final/en_US/en_US.news.txt")
news <- readLines(con,encoding = "UTF-8")
news<- iconv(news, "UTF-8", "ascii", sub = " ")
writeLines(news, "./Corpus/news.txt")
close(con)

con <- file("./final/en_US/en_US.blogs.txt")
blogs <- readLines(con,encoding = "UTF-8")
blogs<- iconv(blogs, "UTF-8", "ascii", sub = " ")
writeLines(blogs, "./Corpus/blogs.txt")
close(con)

con <- file("./final/en_US/en_US.twitter.txt")
twitter <- readLines(con,encoding = "UTF-8",skipNul=TRUE)
twitter<- iconv(twitter, "UTF-8", "ascii", sub = " ")
writeLines(twitter, "./Corpus/twitter.txt")
close(con)

cname <- file.path(".", "Corpus")
docs <- Corpus(DirSource(cname))
rm(twitter,news,blogs)


###############################
### Tokenization & Stemmin###
###############################
# convert to lowercase
docs <- tm_map(docs, content_transformer(tolower))

# remove more transforms
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|@|\\|")

# remove punctuation
docs <- tm_map(docs, removePunctuation)

# remove numbers
docs <- tm_map(docs, removeNumbers)

# strip whitespace
docs <- tm_map(docs, stripWhitespace)

save(docs, file='data/docs.RData')

##############################
### Transfer to Data Frame ###
##############################
ngram_df <- data.frame(text=unlist(sapply(docs, '[',"content")),stringsAsFactors=F)
save(ngram_df, file='data/ngram_df.RData')

################################
## Chunks Spliting for Ngrams ##
################################
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
load('data/ngram_df.RData')
source('func/Task_4.5_ngram_split_func.R')

split_num <- 100
grams <- 4  # 1/2/3/4
ngram_pred4 <- ngramify(split_num, ngram_df, grams)
save(ngram_pred4, file='data/ngrams/ngram_pred4.RData')
rm(ngram_pred4)

grams <- 3  # 1/2/3/4
ngram_pred3 <- ngramify(split_num, ngram_df, grams)
save(ngram_pred3, file='data/ngrams/ngram_pred3.RData')
rm(ngram_pred3)

grams <- 2  # 1/2/3/4
ngram_pred2 <- ngramify(split_num, ngram_df, grams)
save(ngram_pred2, file='data/ngrams/ngram_pred2.RData')
rm(ngram_pred2)

grams <- 1  # 1/2/3/4
ngram_pred1 <- ngramify(split_num, ngram_df, grams)
save(ngram_pred1, file='data/ngrams/ngram_pred1.RData')
rm(ngram_pred1)

######################
## Ngrams Cleansing ##
######################

require(stringr)
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
load('data/ngrams/ngram_pred1.RData')
load('data/ngrams/ngram_pred2.RData')
load('data/ngrams/ngram_pred3.RData')
load('data/ngrams/ngram_pred4.RData')

Unigrams_all_unicode <- str_replace_all(ngram_pred1[,1], "[^a-z]", NA)
length(Unigrams_all_unicode[is.na(Unigrams_all_unicode)])
Unigrams_all_cleaned <- ngram_pred1[!is.na(Unigrams_all_unicode),]
Unigrams_all_cleaned <- Unigrams_all_cleaned[-which(Unigrams_all_cleaned[,2] == 1), ]
save(Unigrams_all_cleaned, file='data/ngrams/Unigrams_all_cleaned.RData')

Bigrams_all_unicode <- str_replace_all(ngram_pred2[,1], "[^a-z][:blank:][^a-z]", NA)
Bigrams_all_unicode <- str_replace_all(Bigrams_all_unicode, "[^a-z ]", NA)
length(Bigrams_all_unicode[is.na(Bigrams_all_unicode)])
Bigrams_all_cleaned <- ngram_pred2[!is.na(Bigrams_all_unicode),]
Bigrams_all_cleaned <- Bigrams_all_cleaned[-which(Bigrams_all_cleaned[,2] == 1), ]
save(Bigrams_all_cleaned, file='data/ngrams/Bigrams_all_cleaned.RData')

Trigrams_all_unicode <- str_replace_all(ngram_pred3[,1], "[^a-z][:blank:][^a-z][:blank:][^a-z]", NA)
Trigrams_all_unicode <- str_replace_all(Trigrams_all_unicode, "[^a-z ]", NA)
length(Trigrams_all_unicode[is.na(Trigrams_all_unicode)])
Trigrams_all_cleaned <- ngram_pred3[!is.na(Trigrams_all_unicode),]
Trigrams_all_cleaned <- Trigrams_all_cleaned[-which(Trigrams_all_cleaned[,2] == 1), ]
save(Trigrams_all_cleaned, file='data/ngrams/Trigrams_all_cleaned.RData')

Quatrgrams_all_unicode <- str_replace_all(ngram_pred4[,1], "[^a-z][:blank:][^a-z][:blank:][^a-z][:blank:][^a-z]", NA)
Quatrgrams_all_unicode <- str_replace_all(Quatrgrams_all_unicode, "[^a-z ]", NA)
length(Quatrgrams_all_unicode[is.na(Quatrgrams_all_unicode)])
Quatrgrams_all_cleaned <- ngram_pred4[!is.na(Quatrgrams_all_unicode),]
Quatrgrams_all_cleaned <- Quatrginprams_all_cleaned[-which(Quatrgrams_all_cleaned[,2] == 1), ]
save(Quatrgrams_all_cleaned, file='data/ngrams/Quatrgrams_all_cleaned.RData')