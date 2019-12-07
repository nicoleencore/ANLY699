install.packages("twitterR")
install.packages("wordcloud")
install.packages("tm")
install.packages("plyr")
install.packages("caret")
install.packages("psych")
install.packages("effects")
install.packages("car")
install.packages("doBy")
install.packages("multcomp")
install.packages("cluster")
install.packages("ggfortify")
install.packages("MASS")
install.packages("rrcov")
  
  

############

# TWITTER WORD CLOUD
library("twitteR")
library("wordcloud")
library("tm")
library("plyr")

consumer_key <-  'msmChN0bNpiMp0HovRZyeOCD7'
consumer_secret <- 'TsTYtHhb0p89CgIA4NKEYagy0kj2pU0ssY0VO80vdvgsOdVCh5'
access_token <- '3423530003-ec2kThW8iJ86n6xV25rAYPSdTAZtzAn1CmpEHuz'
access_secret <- 'plOPkkKMLgPPBw2uQgfgTfEhHFnhDn3M7jB5j7TdyGwfV'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

num_tweets <- 100
r_stats <- searchTwitter("#heart disease", n=num_tweets)

r_stats_text <- sapply(r_stats, function(x) x$getText())
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#thus some of the most commonly used Twitter words about "heart disease" are as follows: protect, risk, chronic and health.





############

#DATA PREPROCESSING
train=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/train_2v.csv')
str(train)

train [train =="Other"] <- NA
sum(is.na(train)==T)
train=na.omit(train) # Removed Rows With Missing Values (e.g., Gender=="Other" to make geneder-related results easier to interpret)
train=train[,-1]
names(train)[7] <- "residence_type"
str(train) #thus missing value rows removed, column renamed, and the number of observations is big enough


test=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/test_2v.csv')
str(test)

test [test =="Other"] <- NA
sum(is.na(test)==T)
test=na.omit(test) # Removed Rows With Missing Values (including Gender=="Other")
test=test[,-1]
names(test)[7] <- "residence_type"
str(test) #thus missing value rows removed, renamed, and the number of observations is big enough

