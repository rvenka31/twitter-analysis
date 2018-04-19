install.packages("twitteR")
install.packages("ROAuth")
install.packages("httpuv")
library("twitteR")
library("ROAuth")
library("devtools")
library("httr")
library(twitteR)
library("RCurl")
library("base64enc")
library("tm")
library("wordcloud")
install.packages('wordcloud')

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")


consumer_key <- "XXXXX"
consumer_secret <- "AAAAAAAAAAAAAAAA"
access_token <- "CCCCCCCCCCCCC"
access_secret <- "DDDDDDDD"

install.packages("httpuv") httpuv

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#twitteR:::setup_twitter_oauth(consumer_key,consumer_secret, access_token, access_secret)

AlphaGo <- searchTwitteR("zuckerberg",n=100,lang = "en",geocode = '20.593684,78.96288,60mi')
AlphaGo[1:3]

AlphaGo_text <- sapply(AlphaGo, function(x) x$getText()) #get texts

Alph_corpus <-Corpus(VectorSource(AlphaGo_text))
inspect(Alph_corpus[1])

Alph_corpus[[1]]$content
Alph_clean=tm_map(Alph_corpus,removePunctuation) #remove punctuation etc.



retreived.tweets = searchTwitter("Avengers",n=500,lang = "en",geocode = '37.09024,-95.712891,600mi') 
df <- do.call("rbind", lapply(retreived.tweets, as.data.frame))

#remove odd characters
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
sample <- df$text

wordcloud(sample)
#sometimes, if twitter brings strange special characters, it might not work
