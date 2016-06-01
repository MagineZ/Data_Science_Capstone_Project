################
### Script 3 ###
################
setwd('C:/Users/Pei-Chun/Documents/R/Coursera-SwiftKey')

rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
require(tm); require(SnowballC); require(stringr);require(RWeka); 
require(qdap); require(scales); require(gridExtra); require(data.table)

load('data/ngrams/Quatrgrams_all_cleaned.RData') 
load('data/ngrams/Trigrams_all_cleaned.RData') 
load('data/ngrams/Bigrams_all_cleaned.RData') 
load('data/ngrams/Unigrams_all_cleaned.RData') 
Unigrams_all_cleaned<-as.data.table(Unigrams_all_cleaned)
Unigrams_all_cleaned[,ngram:=strsplit(terms, '\\s')]
Unigrams_all_cleaned[, `:=`(pred=sapply(ngram, function(s) s[1]),
                           terms=NULL, ngram=NULL)]
Unigrams_all<-Unigrams_all_cleaned
rm(Unigrams_all_cleaned)

Bigrams_all_cleaned<-as.data.table(Bigrams_all_cleaned)
Bigrams_all_cleaned[,ngram:=strsplit(terms, '\\s')]
Bigrams_all_cleaned[, `:=`(w1=sapply(ngram, function(s) s[1]),
                           pred=sapply(ngram, function(s) s[2]),
                           terms=NULL, ngram=NULL)]
Bigrams_all<-Bigrams_all_cleaned
rm(Bigrams_all_cleaned)

Trigrams_all_cleaned<-as.data.table(Trigrams_all_cleaned)
Trigrams_all_cleaned[,ngram:=strsplit(terms, '\\s')]
Trigrams_all_cleaned[, `:=`(w1=sapply(ngram, function(s) s[1]),
                            w2=sapply(ngram, function(s) s[2]),
                            pred=sapply(ngram, function(s) s[3]),
                            terms=NULL, ngram=NULL)]
Trigrams_all<-Trigrams_all_cleaned
rm(Trigrams_all_cleaned)


Quatrgrams_all_cleaned<-as.data.table(Quatrgrams_all_cleaned)
Quatrgrams_all_cleaned[,ngram:=strsplit(terms, '\\s')]
Quatrgrams_all_cleaned[, `:=`(w1=sapply(ngram, function(s) s[1]),
                            w2=sapply(ngram, function(s) s[2]),
                            w3=sapply(ngram, function(s) s[3]),
                            pred=sapply(ngram, function(s) s[4]),
                            terms=NULL, ngram=NULL)]
Quatrgrams_all<-Quatrgrams_all_cleaned
rm(Quatrgrams_all_cleaned)


setnames(Unigrams_all, c('freq', 'pred'))
setnames(Bigrams_all, c('freq', 'w1', 'pred'))
setnames(Trigrams_all, c('freq', 'pred', 'w1','w2'))
setnames(Quatrgrams_all, c('freq','w1','w2','w3', 'pred'))

object.size(Unigrams_all)
Unigrams_all <- Unigrams_all[1:10,]
save(Bigrams_all,Quatrgrams_all,Trigram_all,Unigrams_all,file='completed/ngrams_model.RData')
Quatrgrams_all <- as.data.table(Quatrgrams_all)


###########################
### Prediction Modeling ###
###########################
setkeyv(Bigrams_all, c('w1','freq','pred'))
setkeyv(Trigrams_all, c('w1','w2','freq','pred'))
setkeyv(Quatrgrams_all, c('w1','w2','w3', 'freq', 'pred'))

system.time(predict <- Quatrgrams_all[list('how', 'are', 'you')])
predict

require(stringr)
predictNgrams <- function(input){
    ## clean input text
    # remove numbers, punctuations
    word <- gsub("[^a-zA-Z\n\']", " ", input)
    # convert all words to lowercase
    word <- tolower(word)
    # remove extra spaces
    trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))
    word<-trim(word)      
    
    str <- unlist(str_split(word," "))
    len <- length(str)
    
    predict <- c()
    
    if (len>=3){
        ##trigram 
        W1 <- str[len-2]; W2 <- str[len-1]; W3 <- str[len]
        ngram <- Quatrgrams_all[list(W1, W2, W3)]
        predict <- head(ngram[order(ngram$freq, decreasing=T),]$pred)
        
        if(length(predict)<6){
            ##bigram
            ngram <- Trigrams_all[list(W2, W3)]
            predict <- c(predict,head(ngram[order(ngram$freq, decreasing=T),]$pred))    
        }
        
        if(length(predict)<6){
            ##unigram
            ngram <- Bigrams_all[list(W3)]
            predict <- c(predict,head(ngram[order(ngram$freq, decreasing=T),]$pred))    
        }
        
        predict <- predict[!is.na(predict)]
        
        if(length(predict)<5){
            predict <- c(predict, Unigrams_all$pred[1:5])
        }
    }    
    return(predict)
}

input <- 'tell me something about'

system.time(result <- predictNgrams(input))
result

