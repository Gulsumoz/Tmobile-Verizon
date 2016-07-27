library(twitteR)
tmobiletweets = searchTwitter("t-mobile", n=1000)
verizontweets<-searchTwitter("verizon", n=1000)

pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")
library("stringr")
library("plyr")

score.sentiment = function(sentences, pos.words, neg.words,
                           .progress='none')
{
  #Parameters
  #- sentences: vector of text to score
  #- pos.words: vector of words of postive sentiment
  #- neg.words: vector of words of negative sentiment
  #- .progress: passed to laply() to control of progress bar
  
  #- create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   #- remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   #- remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   #- remove digits
                   sentence = gsub('\\d+', '', sentence)
                   #- define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     #- create missing value
                     y = NA
                     #- tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     #- if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # - result
                     return(y)
                   }
                   #- use tryTolower with sapply
                   sentence = sapply(sentence, tryTolower)
                   
                   #- split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   #- compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   #- get the position of the matched term or NA
                   #- we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   #- final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                   
                 }
                 , pos.words, neg.words, .progress=.progress )
  
  #- data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#convert tmobiletweetslist to array by using sapply.
tmobile_txt = sapply(tmobiletweets, function(x) x$getText())
verizon_txt = sapply(verizontweets, function(x) x$getText())
#how many tweets of each phone carrier
nd = c(length(tmobile_txt), length(verizon_txt))
#join the text
phonecar = c(tmobile_txt, verizon_txt)
#Apply score.sentiment function  and calculate  results
scores = score.sentiment(phonecar, pos, neg, .progress='text')
#add variables to data frame
scores$phonecar = factor(rep(c("T-mobile", "Verizon"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

#boxplot 
boxplot(score~phonecar, data=scores)
#histogram
library("lattice")
histogram(data=scores, ~score|phonecar, main="Sentiment Analysis of 2
          phone carrier", xlab="", sub="Sentiment Score")
