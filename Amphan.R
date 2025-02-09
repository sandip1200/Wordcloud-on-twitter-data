#Using Rtweet
install.packages(rtweet)

library(rtweet)

#whatever name you assigned to you created app

appname="Amphan"

#Your api key
api_key="D*******************R"
api_secret="f**********************************S"   #Your api secret key
api_key
#create twiter_token variable with help of app,consumer_key and consumer_secret

twitter_token = create_token(
  app = "Amphan",
  consumer_key = api_key,
  consumer_secret = api_secret)

#search tweet

tw=search_tweets("Amphan", n=4500, token = twitter_token, lang="en")

head(tw)

#converted to dataframe
tw1=as.data.frame(tw)
head(tw1)

View(tw1)


#Data preprocessing

install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RcolorBrewer")
install.packages("stringr")

library(RColorBrewer)
library(NLP)
library(SnowballC)
library(ggplot2)
library(tm)
library(wordcloud)
library(stringr)

#remove emoticons
tw1$text <- sapply(tw1$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#create corpus
tw1_corpus<-Corpus(VectorSource(as.vector(tw1$text)))
inspect(tw1_corpus)



#lowercase
tw1_corpus<-tm_map(tw1_corpus,content_transformer(tolower))

#remove punctuation
tw1_corpus<-tm_map(tw1_corpus, content_transformer(removePunctuation))

inspect(tw1_corpus)

#removing numbers
tw1_corpus<-tm_map(tw1_corpus,content_transformer(removeNumbers))
inspect(tw1_corpus)

#remove bad text
Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove Retweet
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}
tw2_corpus <- tm_map(tw1_corpus,Textprocessing)


#add stopwords

mywords<-c(stopwords("english"),"india","rt","��","get","like","just","yes","know","will","good","day","people","amphan")

#remove stopwords
tw2_corpus<-tm_map(tw2_corpus, removeWords, mywords)

inspect(tw7_corpus)


#stemming
tw3_corpus<-tm_map(tw2_corpus,content_transformer(stemDocument), language="english")

#term document

my_dtm <-TermDocumentMatrix(tw3_corpus)
inspect(my_dtm[1:5,3:8])

m<-as.matrix(my_dtm)
v<-sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)
tail(d,10)             
#generate wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(15, "Dark2"))
head(d)


# order bars
top<-head(d,50)
p1<- ggplot(top, aes(x = reorder(word,freq), y = freq)) +
  geom_col() +
  coord_flip() +
  ggtitle("Unique word count")
plot(p1)

