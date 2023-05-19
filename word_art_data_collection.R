install.packages("twitteR")
install.packages("tm")
install.packages("ROAuth")

library(twitteR)
library(tm)
library(ROAuth)

consumer_key <- 'INSERT_CONSUMER_KEY'
consumer_secret <- 'INSERT_CONSUMER_SECRET'
access_token <- 'INSERT_ACCESS_TOKEN'
access_secret <- 'INSERT_ACCESS_SECRET'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)





### Pulling Words From Tweets to Create Word Portraits Using Search Strings:
### @KingJames, #RallyTheValley, and #LakeShow





### Search String: @KingJames
lbj_tweets <- searchTwitter("@KingJames",n=1000)

## count number of tweets
n.tweets <- length(lbj_tweets)
n.tweets

## convert tweets into dataframe
lbj_tweets.df <- twListToDF(lbj_tweets)
View(lbj_tweets.df)

## cleaning the text
myCorpus <- Corpus(VectorSource(lbj_tweets.df$text))
myCorpus <- tm_map(myCorpus, removeWords, stopwords())

myStopWords <- "@KingJames"
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)

remove_url <- function(x) gsub("http[^[:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(remove_url))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument)

dtm <- TermDocumentMatrix(myCorpus)
dtm <- as.matrix(dtm)
dtm <- sort(rowSums(dtm),decreasing=TRUE)
dtm <- data.frame(word = names(dtm),freq=dtm)
View(dtm)

## writing the data frame to a CSV and saving to Downloads directory
write.csv(dtm, "C:\\Users\\INSERT_DIRECTORY_NAME\\
          Downloads\\lbj_tweet_words.csv", row.names = FALSE)





### Search String: #RallyTheValley
phx_tweets <- searchTwitter("#RallyTheValley",n=500)

## count number of tweets
n.tweets <- length(phx_tweets)
n.tweets

## convert tweets into dataframe
phx_tweets.df <- twListToDF(phx_tweets)
View(phx_tweets.df)

## cleaning the text
myCorpus <- Corpus(VectorSource(phx_tweets.df$text))
myCorpus <- tm_map(myCorpus, removeWords, stopwords())

myStopWords <- c("#RallyTheValley+rallythevalley+rally+the+valley+
                 Suns+suns+Sun+sun")
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)

remove_url <- function(x) gsub("http[^[:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(remove_url))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument)

dtm <- TermDocumentMatrix(myCorpus)
dtm <- as.matrix(dtm)
dtm <- sort(rowSums(dtm),decreasing=TRUE)
dtm <- data.frame(word = names(dtm),freq=dtm)
View(dtm)

## writing the data frame to a CSV
write.csv(dtm, "phx_tweet_words.csv", row.names = FALSE)





### Search String: #LakeShow
lal_tweets <- searchTwitter("#LakeShow",n=500)

## count number of tweets
n.tweets <- length(lal_tweets)
n.tweets

## convert tweets into dataframe
lal_tweets.df <- twListToDF(lal_tweets)
View(lal_tweets.df)

## cleaning the text
myCorpus <- Corpus(VectorSource(lal_tweets.df$text))
myCorpus <- tm_map(myCorpus, removeWords, stopwords())

myStopWords <- c("#LakeShow+lakeshow+lake+show+Lakers+lakers+Laker+laker")
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)

remove_url <- function(x) gsub("http[^[:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(remove_url))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument)

dtm <- TermDocumentMatrix(myCorpus)
dtm <- as.matrix(dtm)
dtm <- sort(rowSums(dtm),decreasing=TRUE)
dtm <- data.frame(word = names(dtm),freq=dtm)
View(dtm)

## writing the data frame to a CSV
write.csv(dtm, "lal_tweet_words.csv", row.names = FALSE)