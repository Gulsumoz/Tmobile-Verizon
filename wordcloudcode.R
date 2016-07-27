library(twitteR)
#searching from twitter

tmobiletweets = searchTwitter("t-mobile", n=1000)
verizontweets<-searchTwitter("verizon", n=1000)

#to fit paper widht
for (i in c(1:2, 1000)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(tmobile_df$text[i], 60))
}
library(tm)
# build a corpus
myCorpus <- Corpus(VectorSource(tmobile_df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
#remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'), "available", "via")
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy of corpus to use  as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:5])
# The code below is used for to make text fit for paper width
for (i in c(1:2, 1000)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
# count frequency of "t-mobile"
tmobileCases <- lapply(myCorpusCopy,
                      function(x) { grep(as.character(x), pattern = "\\<t-mobile")} )
sum(unlist(tmobileCases))
#form term document matrix

tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  +   xlab("Terms") + ylab("Count") + coord_flip()

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 5,
          random.order = F, colors = "red")







