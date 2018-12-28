library(shiny)
rm(list=ls())
library(dplyr) # "data pliers"

#setwd("C:\\WORK\\Cursos\\Coursera\\BigData\\Capstone\\Data\\final\\en_US")
unigram1<-read.table("unigram.txt")
bigram1<-read.table("bigram.txt")
trigram1<-read.table("trigram.txt")
fourgram1<-read.table("fourgram.txt")
#fivegram<-read.table("fivegram.txt") we finally do not use fivegrams since they give low benefit at a high cost
#source("C:\\WORK\\Cursos\\Coursera\\BigData\\Capstone\\code\\App\\appCode.R")
source("appCode.R")
profanity<-read.table("profanity.txt")
profanity<-as.character(profanity$V1)



shinyServer(function(input, output) {

  output$prediction <- renderPrint({
    avoidProfanity<-input$avoidProfanity
    result <- predictNextWordArray(input$inputText,profanity,avoidProfanity,bigram1,trigram1,fourgram1)
    predSentence<-predictedSentence(input$inputText,profanity,avoidProfanity,bigram1,trigram1,fourgram1)
    output$predSentence <- renderText({predSentence})
    result
  });
  
}
)

