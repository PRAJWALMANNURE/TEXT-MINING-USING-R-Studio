library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
install.packages('wordcloud2')
library(wordcloud2)
install.packages('syuzhet')
library(syuzhet)

library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

install.packages('devtools')
devtools::install_github("lchiffon/wordcloud2")


# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

getwd()
setwd("C:/Users/PJ/Documents")
write.table(IMDB_reviews,"Aquaman.txt",row.names = F)


Aquaman <- read.delim('Aquaman.txt')
str(Aquaman)

View(Aquaman)

# Build Corpus and DTM/TDM
library(tm)
corpus <- Aquaman[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)

cleanset<-tm_map(corpus,removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')

# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]


# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))


# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

#sentiment analysis
IMDB_reviews <- read.delim('Aquaman.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)

reviews[4]
get_nrc_sentiment('splendid')

# Splendid has one Joy and one positive 
get_nrc_sentiment('no words') #1 Anger and 1 Negative

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Aquaman')
