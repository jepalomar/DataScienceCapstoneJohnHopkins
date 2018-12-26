library(dplyr) # "data pliers"
library(tibble) # data tables
library(tm) # text mining
library(stringi) # convenience functions for string processing
library(ngram)
library(R.utils)
library(RWeka)
library(ggplot2)

rm(list=ls())
setwd("C:\\WORK\\Cursos\\Coursera\\BigData\\Capstone\\Data\\final\\en_US")


timeBlogs<-0
timeTwit<-0
timeNews<-0

st<-Sys.time()
fileBlogs <- file('en_US.blogs.txt', encoding="UTF-8") #if we don't declare the encoding explicitly, we get errors
blogs   <- readLines(fileBlogs)
close(fileBlogs)
sizeBlogs <- file.info('en_US.blogs.txt')$size
nWordsBlogs <- wordcount(blogs)
nLinesBlogs <- countLines('en_US.blogs.txt')
et<-Sys.time()
timeBlogs<-timeBlogs + et-st
filesummary <- tibble(name="blogs", size=sizeBlogs, words=nWordsBlogs, lines=nLinesBlogs, time = timeBlogs)

st<-Sys.time()
fileNews <- file('en_US.news.txt', encoding="UTF-8")
news   <- readLines(fileNews)
close(fileNews)
length(news)
sizeNews <- file.info('en_US.news.txt')$size
nWordsNews <- wordcount(news)
nLinesNews <- countLines('en_US.news.txt')
et<-Sys.time()
timeNews<-timeNews + et-st
filesummary <-  add_row(filesummary, name="news", size=sizeNews, words=nWordsNews, lines=nLinesNews, time = timeNews)

st<-Sys.time()
fileTwitter <- file('en_US.twitter.txt', encoding="UTF-8")
twitter   <- readLines(fileTwitter)
close(fileTwitter)
length(twitter)
sizeTwit <- file.info('en_US.twitter.txt')$size
nWordsTwit <- wordcount(twitter)
nLinesTwit <- countLines('en_US.twitter.txt')
et<-Sys.time()
timeTwit<-timeTwit + et-st
filesummary <- add_row(filesummary, name="twitter", size=sizeTwit, words=nWordsTwit, lines=nLinesTwit, time=timeTwit)


filesummary





# Prepare corpus

# We use a small sample to avoid exceeding my available memory.

CreateCorpus<-function(blogs, twitter, news, useSpecificWords,words,thisSeed, thisSampleRate)
{
  mySeed<-41234
  sampleRate<-0.005
  if(!missing(thisSeed)) 
  {
    mySeed<-thisSeed
  }    
   if(!missing(thisSampleRate)) 
  {
    sampleRate<-thisSampleRate
  }     
  
  sampleBlogs<-head(blogs)
  sampleTwitter<-head(twitter)
  sampleNews<-head(news)
  if(!useSpecificWords)
  {
    #now we take samples of the three sets
    sample_rate <- 0.005 
    set.seed(thisSeed)
    sampleBlogs <- blogs[sample(1:length(blogs), length(blogs) * sample_rate, replace=FALSE)]
    sampleTwitter <- twitter[sample(1:length(twitter), length(twitter) * sample_rate, replace=FALSE)]
    sampleNews <- news[sample(1:length(news), length(news) * sample_rate, replace=FALSE)]
    
  }
  else
  {
    input<-removePunctuation(input)
    blogIndex<-grepl(words, blogs, ignore.case=TRUE)
    sampleBlogs<-blogs[blogIndex]
    twitterIndex<-grepl(words, twitter, ignore.case=TRUE)
    sampleTwitter<-twitter[twitterIndex]
    newsIndex<-grepl(words, news, ignore.case=TRUE)
    sampleNews<-news[newsIndex]
  }
  
  
  # convert samples to ASCII from unicode
  sampleBlogs <- iconv(sampleBlogs,from="UTF-8",to="ASCII","")
  sampleTwitter <- iconv(sampleTwitter,from="UTF-8",to="ASCII","")
  sampleNews <- iconv(sampleNews,from="UTF-8",to="ASCII","")
  totalSample = rbind(sampleBlogs,sampleNews,sampleTwitter)
  vecTotalSample <- VectorSource(totalSample) # create the tm vector source for input. 
  corpus <- VCorpus(vecTotalSample) # Collections of documents containing text
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, content_transformer(removePunctuation))
  corpus = tm_map(corpus, content_transformer(removeNumbers))
  corpus = tm_map(corpus, content_transformer(stripWhitespace))
  rm(vecTotalSample)
  cleanCorpus <- data.frame(rawtext = sapply(corpus, as.character), stringsAsFactors=FALSE)
  cleanCorpus$textLines <- tolower(cleanCorpus$rawtext)
  cleanCorpus
  
  
}



  #Loading tockenizers package
  library(tokenizers)
  

CreateNgram<-function(corpus, numWords)
{
  if(exists("unigram")) thisNGram<-unigram
  if(length(corpus)>1)
  {
    #NGram 1
    tokens <- tokenize_ngrams(corpus$textLines,n=numWords,n_min = numWords)
    thisTable <- table(unlist(tokens))
    thisNgram <- data.frame(thisTable)
    thisNgram <- thisNgram[order(thisNgram$Freq,decreasing = TRUE),]
    if(numWords>1)
    {
      thisNgram<-addWordsCols(thisNgram,numWords)
    }
  }
  thisNgram
  
}

source("C:\\WORK\\Cursos\\Coursera\\BigData\\Capstone\\code\\AuxFunctions.R")
input<-"You love that"
corpus<-CreateCorpus(blogs,twitter,news,FALSE,input,41234)

unigram<-CreateNgram(corpus,1)
bigram<-CreateNgram(corpus,2)
trigram<-CreateNgram(corpus,3)
fourgram<-CreateNgram(corpus,4)
fivegram<-CreateNgram(corpus,5)

unigram<-NgramStats(unigram)
bigram<-NgramStats(bigram)
trigram<-NgramStats(trigram)
fourgram<-NgramStats(fourgram)
fivegram<-NgramStats(fivegram)
write.table(unigram,file="unigram.txt")
write.table(bigram,file="bigram.txt")
write.table(trigram,file="trigram.txt")
write.table(fourgram,file="fourgram.txt")
write.table(fivegram,file="fivegram.txt")



unigram<-read.table("unigram.txt")
bigram<-read.table("bigram.txt")
trigram<-read.table("trigram.txt")
fourgram<-read.table("fourgram.txt")
fivegram<-read.table("fivegram.txt")
source("C:\\WORK\\Cursos\\Coursera\\BigData\\Capstone\\code\\AuxFunctions.R")


input<-"you love that"
corpus<-CreateCorpus(blogs,twitter,news,TRUE,input)
unigram<-CreateNgram(corpus,1)
bigram<-CreateNgram(corpus,2)
trigram<-CreateNgram(corpus,3)
fourgram<-CreateNgram(corpus,4)
fivegram<-CreateNgram(corpus,5)
x<-predictNextWord(input)


sample_rate <- 0.001 
set.seed(55863)
testBlogs <- blogs[sample(1:length(blogs), length(blogs) * sample_rate, replace=FALSE)]
testTwitter <- twitter[sample(1:length(twitter), length(twitter) * sample_rate, replace=FALSE)]
testNews <- news[sample(1:length(news), length(news) * sample_rate, replace=FALSE)]

test<-c(testBlogs,testTwitter,testNews)
testBegin<-word(test,1,-2)
testEnd<-word(test,-1)

score0<-GetWeightsScore(testBegin,testEnd,0.99,0.01) # 373
score1<-GetWeightsScore(testBegin,testEnd,0.9,0.1) # 372
score2<-GetWeightsScore(testBegin,testEnd,0.8,0.2) # 372
score3<-GetWeightsScore(testBegin,testEnd,0.7,0.3) # 373
score4<-GetWeightsScore(testBegin,testEnd,0.6,0.4) # 370
score5<-GetWeightsScore(testBegin,testEnd,0.5,0.5) # 368


