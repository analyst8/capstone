blog <- readLines("en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news <- readLines("en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter <- readLines("en_US.twitter.txt",skipNul = TRUE, warn = TRUE)

library(ggplot2)
library(NLP)
library(tm)
library(RWeka)
library(data.table)
library(dplyr)

set.seed(100)
sample_size = 500

sample_blog <- blog[sample(1:length(blog),sample_size)]
sample_news <- news[sample(1:length(news),sample_size)]
sample_twitter <- twitter[sample(1:length(twitter),sample_size)]

sample_data<-rbind(sample_blog,sample_news,sample_twitter)
rm(blog,news,twitter)

mycorpus<-VCorpus(VectorSource(sample_data))
mycorpus <- tm_map(mycorpus, content_transformer(tolower)) # convert to lowercase
mycorpus <- tm_map(mycorpus, removePunctuation) # remove punctuation
mycorpus <- tm_map(mycorpus, removeNumbers) # remove numbers
mycorpus <- tm_map(mycorpus, stripWhitespace) # remove multiple whitespace
changetospace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
mycorpus <- tm_map(mycorpus, changetospace, "/|@|\\|")


uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
fourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
OneT <- NGramTokenizer(mycorpus, Weka_control(min = 1, max = 1))
oneGM <- TermDocumentMatrix(mycorpus, control = list(tokenize = uniGramTokenizer))
twoGM <- TermDocumentMatrix(mycorpus, control = list(tokenize = biGramTokenizer))
threeGM <- TermDocumentMatrix(mycorpus, control = list(tokenize = triGramTokenizer))
fourGM <- TermDocumentMatrix(mycorpus, control = list(tokenize = fourGramTokenizer))

freqTerms1 <- findFreqTerms(oneGM, lowfreq = 5)
termFreq1 <- rowSums(as.matrix(oneGM[freqTerms1,]))
termFreq1 <- data.frame(unigram=names(termFreq1), frequency=termFreq1)
termFreq1 <- termFreq1[order(-termFreq1$frequency),]
unigramlist <- setDT(termFreq1)
save(unigramlist,file="unigram.Rda")
freqTerms2 <- findFreqTerms(twoGM, lowfreq = 3)
termFreq2 <- rowSums(as.matrix(twoGM[freqTerms2,]))
termFreq2 <- data.frame(bigram=names(termFreq2), frequency=termFreq2)
termFreq2 <- termFreq2[order(-termFreq2$frequency),]
bigramlist <- setDT(termFreq2)
save(bigramlist,file="bigram.Rda")
freqTerms3 <- findFreqTerms(threeGM, lowfreq = 2)
termFreq3 <- rowSums(as.matrix(threeGM[freqTerms3,]))
termFreq3 <- data.frame(trigram=names(termFreq3), frequency=termFreq3)
trigramlist <- setDT(termFreq3)
save(trigramlist,file="trigram.Rda")
freqTerms4 <- findFreqTerms(fourGM, lowfreq = 1)
termFreq4 <- rowSums(as.matrix(fourGM[freqTerms4,]))
termFreq4 <- data.frame(fourgram=names(termFreq4), frequency=termFreq4)
fourgramlist <- setDT(termFreq4)
save(fourgramlist,file="fourgram.Rda")



load("unigram.Rda")
load("bigram.Rda")
load("trigram.Rda")
load("fourgram.Rda")

wordproc <- function(sentence){
  value = "next word is..."
  sen = unlist(strsplit(sentence,' '))
  if(length(sen)>=3){
    value = fourgram(sen[(length(sen)-2):length(sen)])
  }
  
  if(is.null(value)||length(sen)==2){
    value = trigram(sen[(length(sen)-1):length(sen)])
    
  }
  if(is.null(value)||length(sen)==1){
    value = bigram(sen[length(sen)])
    
  }
  if(is.null(value)){
    value = "the"
    #        k<-unigramlist$unigram
    #        value = as.String(k[1])
  }
  
  return(value)
}

fourgram <- function(fourg){
  four <- paste(fourg,collapse = ' ')
  foursum <- data.frame(fourgram="test",frequency=0)
  k <- trigramlist[trigram==four]
  m <- as.numeric(k$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in unigramlist$unigram){
    text = paste(four,string0)
    found <- fourgramlist[fourgram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      foursum <- rbind(foursum,found)
      
    }
  }
  if(nrow(foursum)==1) return(NULL)
  foursum <- foursum[order(-frequency)]
  sen <- unlist(strsplit(as.String(foursum[1,fourgram]),' '))
  return (sen[length(sen)])
}

trigram <- function(threeg){
  three <- paste(threeg,collapse = ' ')
  threesum <- data.frame(trigram="test",frequency=0)
  k <- bigramlist[bigram==three]
  m <- as.numeric(k$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in unigramlist$unigram){
    text = paste(three,string0)
    found <- trigramlist[trigram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      threesum <- rbind(threesum,found)
      
    }
  }
  if(nrow(threesum)==1) return(NULL)
  threesum <- threesum[order(-frequency)]
  sen <- unlist(strsplit(as.String(threesum[1,trigram]),' '))
  return (sen[length(sen)])
}

bigram <- function(twog){
  two <- paste(twog,collapse = ' ')
  twosum <- data.frame(bigram="test",frequency=0)
  k <- unigramlist[unigram==two]
  m <- as.numeric(k$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in unigramlist$unigram){
    text = paste(two,string0)
    found <- bigramlist[bigram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      twosum <- rbind(twosum,found)
      
    }
  }
  
  if(nrow(twosum)==1) return(NULL)
  twosum <- twosum[order(-frequency)]
  
  sen <- unlist(strsplit(as.String(twosum[1,bigram]),' '))
  return (sen[length(sen)])
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  mainPanel(
    h3("Introduction:"),
    h5("This application takes your string and predict the next world"),
    h3("Method:"),
    h5("Use MLE of n-gram algorithm"),
    
    textInput("Tcir",label=h3("Type your sentence here:")),
    submitButton('Submit'),
    h4('string you entered : '),
    verbatimTextOutput("inputValue"),
    h4('next word :'),
    verbatimTextOutput("prediction")
    
  )
)

