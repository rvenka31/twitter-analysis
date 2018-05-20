library("twitteR")
library("ROAuth")
library("devtools")
library("httr")
library("RCurl")
library("base64enc")
library("tm")
library("wordcloud")
library("slam")
library("topicmodels")

#setup twitter API
consumer_key <- "XXXXX"
consumer_secret <- "AAAAAAAAAAAAAAAA"
access_token <- "CCCCCCCCCCCCC"
access_secret <- "DDDDDDDD"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Load Tweet
retreived.tweets <- searchTwitter("Zuckerberg",n=500,lang = "en")

#convert tweets to dataframe
df <- do.call("rbind", lapply(retreived.tweets, as.data.frame))
#remove odd characters

                  
preprocessing <- function(tweets){
  tweets <- sapply(document,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
  tweets <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweets) #remove URL
  tweets <- gsub("(RT|via)((?:\b\\W*@\\w+)+)","",tweets)
  tweets <- gsub("http[^[:blank:]]+", "", tweets)
  tweets <- gsub("@\\w+", "", tweets)
  tweets <- gsub("[ t]{2,}", "", tweets)
  tweets <- gsub("^\\s+|\\s+$", "", tweets)
  tweets <- gsub("\\d+", "", tweets)
  corpus <- Corpus(VectorSource(tweets)) #term matrix
  corpus <- tm_map(corpus,removePunctuation) # removes punctuation
  corpus <- tm_map(corpus,stripWhitespace) # removes whitespace
  corpus <- tm_map(corpus,tolower) #makes lower
  #remove other words and stop words depends upon the topic we choose
  other_words <- c('ark','ate','ax','bay','ci','ct','dam','de','dew','dis','dot','dry','ebb','ed','em','ess','exp','ha','hat','hot','ich','ii','ing','inn','ins','ion','ism','iwo','khe','le','ll','mad','mai','mrs','nd','o','oar','odd','ome','onl','ose','pat','pra','pre','rel','rid','rio','rob','rod','sad','ses','som','son','sow','t','tap','wh','wi','wit','apt','boy','cup','din','dr','fed','icy','lit','m','non','par','red','row','sap','sit','sun','tie','vow','w','y','beg','bow','box','eve','fix','re','sky','ve','woe','won','arm','art','bar','buy','eye','fit','joy','low','n','owe','raw','six','thy','air','foe','key','lie','fly','st','ten','win','cut','job','aim','big','lay','sum','run','saw','ill','lot','bad','sea','she','try','yes','met','led','why','0','off','add','era','age','th','pay','put','set','due','mr','ago','aid','say','her','how','end','him','two','use','act','see','1','s','up','old','yet','way','nor','man','too','day','god','out','he','law','men','let','me','you','his','now','own','new','one','was','if','may','who','so','no','us','my','u','on','or','its','has','i','not','it','is','we','our','in','to','of','the')
  specific_words <- c('people','government','constitution')
  corpus = tm_map(corpus,removeWords,c(stopwords("SMART"),other_words,specific_words))
  return(corpus)
  
}  

# Creating a Term document Matrix after preprocessing
tdm = DocumentTermMatrix(preprocessing(df$text)) 
# create tf-idf matrix
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)
tdm <- tdm[,term_tfidf >= 0.1]
tdm <- tdm[row_sums(tdm) > 0,]
summary(col_sums(tdm))
#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(tdm, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
#calculating LDA
#number of topics
k <- 4;
SEED <- 500; # number of tweets used
CSC_TM <-list(VEM = LDA(tdm, k = k, control = list(seed = SEED)),VEM_fixed = LDA(tdm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),Gibbs = LDA(tdm, k = k, method = "Gibbs",control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000)),CTM = CTM(tdm, k = k,control = list(seed = SEED,var = list(tol = 10^-4), em = list(tol = 10^-3))))
#To compare the fitted models we first investigate the values of the models fitted with VEM 
#and estimated and with VEM and fixed 
sapply(CSC_TM[1:2], slot, "alpha")
sapply(CSC_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z)  sum(z * log(z)))))
Topic <- topics(CSC_TM[["VEM"]], 1)
Terms <- terms(CSC_TM[["VEM"]], 8)
Terms
