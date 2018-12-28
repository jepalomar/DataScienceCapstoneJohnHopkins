


#Setting up Environment
library(stringr)
library(dplyr)
library(tm)
#library(stylo)

#Cleaning input to extract specific words
cleanInput <- function(text){
  textInput <- tolower(text)
  textInput <- removePunctuation(textInput)
  textInput <- removeNumbers(textInput)
  textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
  textInput <- stripWhitespace(textInput)
  aux<-strsplit(textInput," ")
  textInput<-aux[[1]]
  #textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
  return(textInput)
}





 # Function that given two arrays of N words builds N bigrams and returns the freq of every one of them
 getFreqBigram<-function(word1,word2,bigram)
 {
   freq<-c()
   for(i in 1:length(word1))
   {
     thisfreq<-0
     aux<-bigram$Freq[bigram$word1 == word1[i] & bigram$word2 == word2[i]]
     if(length(aux)>0)
     {
       thisfreq<-aux
     }
     freq<-c(freq,thisfreq)
   }
   z<-data.frame(word2,freq)
   names(z)<-c("word","freq")
   z
 }

 # Function that given two arrays of N words builds N bigrams and returns the freq of every one of them
 getFreqTrigram<-function(word1,word2,word3,trigram)
 {
   freq<-c()
   for(i in 1:length(word1))
   {
     thisfreq<-0
     aux<-trigram$Freq[trigram$word1 == word1[i] & trigram$word2 == word2[i]  & trigram$word3 == word3[i]]
     if(length(aux)>0)
     {
       thisfreq<-aux
     }
     freq<-c(freq,thisfreq)
   }
   z<-data.frame(word3,freq)
   names(z)<-c("word","freq")
   z

 }

 getFreqFourgram<-function(word1,word2,word3,word4,fourgram)
 {
   freq<-c()
   for(i in 1:length(word1))
   {
     thisfreq<-0
     aux<-fourgram$Freq[fourgram$word1 == word1[i] & fourgram$word2 == word2[i]  & fourgram$word3 == word3[i]  & fourgram$word4 == word4[i]]
     if(length(aux)>0)
     {
       thisfreq<-aux
     }
     freq<-c(freq,thisfreq)
   }
   z<-data.frame(word4,freq)
   names(z)<-c("word","freq")
   z

 }




#Match string in bigram and get probable word
matchBigram <- function(inputWord1,bigram)
{
  aux<-filter(bigram,( word1 == inputWord1 ))
  thisWord <- aux$word2
  thisWord<-as.character(thisWord)
  thisFreq<-aux$Freq
  
  thisPrediction<-data.frame(thisWord,thisFreq)
  names(thisPrediction)<-c("prediction","freq")
  thisPrediction
  
}


#Match string in Three Gram and get probable word
matchTrigram <- function(inputWord1,inputWord2,bigram,trigram)
{
  aux<-filter(trigram,( word1 == inputWord1 & word2 == inputWord2))
  if(length(aux$Freq)==0)
  {
    thisPrediction <- matchBigram(inputWord2,bigram)
  }
  else
  {
    thisWord <- aux$word3
    thisWord<-as.character(thisWord)
    thisFreq<-aux$Freq
    # we will sort the possible predictions first by their probability and then by the prob of the last (n-1)-gram
    z<-getFreqBigram(rep(inputWord2,length(thisWord)),thisWord,bigram)
    thisPrediction<-data.frame(thisWord,thisFreq,z$freq)
    names(thisPrediction)<-c("prediction","freq","freqLast")
    thisPrediction<-thisPrediction[with(thisPrediction,order(-freq,-freqLast)),]
  }
  
  thisPrediction
}





#Match string in Four Gram and get probable word
matchFourGram <- function (inputWord1,inputWord2,inputWord3,bigram,trigram,fourgram)
  
{
  aux <- filter(fourgram,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))
  if(length(aux$Freq) == 0)
  {
    thisPrediction <- matchTrigram(inputWord2,inputWord3,bigram,trigram)
  }
  else
  {
    thisWord <- aux$word4
    thisWord<-as.character(thisWord)
    thisFreq<-aux$Freq
    # we will sort the possible predictions first by their probability and then by the prob of the last (n-1)-gram
    z<-getFreqTrigram(rep(inputWord2,length(thisWord)),rep(inputWord3,length(thisWord)),thisWord,trigram)
    thisPrediction<-data.frame(thisWord,thisFreq,z$freq)
    names(thisPrediction)<-c("prediction","freq","freqLast")
    thisPrediction<-thisPrediction[with(thisPrediction,order(-freq,-freqLast)),]
  } 
  thisPrediction
  
}


#Predict next word Function takes in the input variable from user and predicts the next word
getWordProbs <- function(input,bigram,trigram,fourgram)
{
  
  #Clean input
  wordInput <- cleanInput(input)
  #count number of words
  wordCount <- length(wordInput)
  #Initializing response
  myPredictedWords <- c()
  
  #Trimming input to the last five words
  if(wordCount>3)
  {
    wordInput <- wordInput[(wordCount-2):wordCount]
    myPrediction <- matchFourGram(wordInput[1],wordInput[2],wordInput[3],bigram,trigram,fourgram)
  }
  
  
  #Four Gram Match
  if(wordCount ==3)
  {
    myPrediction <- matchFourGram(wordInput[1],wordInput[2],wordInput[3],bigram,trigram,fourgram)
  }
  
  #Three Gram Match
  if(wordCount ==2)
  {
    myPrediction <- matchTrigram(wordInput[1],wordInput[2],bigram,trigram)
  }
  #Two gram match
  if(wordCount ==1)
  {
    myPrediction <- matchBigram(wordInput[1],bigram)
  }
  
  #No word entered
  if(wordCount == 0)
  {
    myPredictedWords <- "Please enter words"
  }
  
  #Unknown words
  if(length(myPrediction$prediction)==0)
  {
    myPredictedWords <- "Unable to predict"
  }
  
  #Returning response
  # if(length(myPrediction$prediction) < 5)
  # {
  #   myPredictedWords<-myPrediction$prediction
  # }
  # else
  # {
  #   myPredictedWords<-myPrediction$prediction[1:5]
  # }
  myPredictedWords<-myPrediction
  myPredictedWords
  
  
}

predictNextWordWeights<-function(input,w1,w2,bigram,trigram,fourgram)
{
  x<-getWordProbs(input,bigram,trigram,fourgram)
  if("freqLast" %in% names(x))
  {
    x$probs<-w1*x$freq/sum(x$freq) + w2*x$freqLast/sum(x$freqLast)
  }
  else
  {
    x$probs<-x$freq/sum(x$freq)
  }
  thisPrediction<-x[with(x,order(-probs)),]
  thisPrediction
}

predictNextWord<-function(input)
{
  thisPrediction<-predictNextWordWeights(input,0.9,0.1,bigram,trigram,fourgram)
  thisPrediction
}

predictNextWordArray<-function(input,profanity,avoidProfanity,bigram,trigram,fourgram)
{
  thisPrediction<-predictNextWordWeights(input,0.9,0.1,bigram,trigram,fourgram)
  if(length(thisPrediction)==0)
  {
    myPred<-""
  }
  else
  {
    myPred<-head(thisPrediction$prediction,6)
    if(avoidProfanity) myPred<-setdiff(myPred,profanity)
  }
  myPred
}

predictedSentence<-function(input,profanity,avoidProfanity,bigram,trigram,fourgram)
{
  thisPrediction<-predictNextWordArray(input,profanity,avoidProfanity,bigram,trigram,fourgram)
  if(length(thisPrediction)==0)
  {
    myPred<-input
  }
  else
  {
    myPred<-paste(input,thisPrediction[1],sep=" ")
  }
  myPred
}

NgramStats<-function(ngram)
{
  ngram$Perc<-ngram$Freq/sum(ngram$Freq)
  ngram$Cumul<-cumsum(ngram$Perc)
  ngram
}

RelevantNgrams<-function(ngram)
{
  auxGram<-ngram[ngram$Freq>1,]
  auxGram
}


