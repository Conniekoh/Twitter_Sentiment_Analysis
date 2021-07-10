#Tweets

#1. Gather Tweets
#Tweets were collected at 

#Tweets were collected at 10:06 am PST on June 26, 2021 on three celebrities
library(rtweet)
musk = search_tweets(q = 'elonmusk',n = 200, include_rts = F)
cook = search_tweets(q = 'tim_cook', n=200, include_rts=F)
harris = search_tweets(q = 'kamalaharris', n=200, include_rts=F)
gates = search_tweets(q = 'billgates', n=200, include_rts=F)

#Peek at a few tweets
musk$text[50]

#combine all tweets in dataframe
tweets=
  rbind(cbind(rep('musk', nrow(musk)),musk$text),
        cbind(rep('cook', nrow(cook)), cook$text),
        cbind(rep('harris', nrow(harris)), harris$text),
        cbind(rep('gates', nrow(gates)), gates$text))
tweets = data.frame(tweets)
colnames(tweets) = c('person', 'text')

#2.Clean and Tokenize
library(tm); library(SnowballC)
corpus = Corpus(VectorSource(tweets$text))
library(magrittr)

#convert to lowercase; remove urls; remove emojis; remove punctuation; remove stopwords; remove whitespace
library(tm); library(SnowballC)
corpus = Corpus(VectorSource(tweets$text))
library(magrittr)
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '@\\w+',replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '<.*>',replacement = ' ',x = x))) %>%  # remove emojis
  tm_map(content_transformer(function(x)iconv(x = x,from = "latin1", to = "ASCII", sub="")))%>%
  tm_map(removeWords, c(stopwords('english'),'narendramodi','elonmusk','realdonaldtrump','katyperry','&amp;','amp'))%>%
  tm_map(removePunctuation)

#create a dictionary of all words
dict = findFreqTerms(DocumentTermMatrix(corpus),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

#Use tm_map to stem words
# EXAMPLE --- . example: stemDocument(running,runs,ran) would return (run,run,ran) as the ouput.

corpus = 
  corpus %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)


#create a document term matrix to see Term frequency weighting for each term
xdtm = DocumentTermMatrix(corpus)
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),dictionary = dict_corpus,type = 'prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

#3. Topic Model
#Identify topics in tweets
library(topicmodels)
xdtm_topic = xdtm[which(rowSums(xdtm)!=0),]
set.seed(617)
topic2 = LDA(x = xdtm_topic,k = 4)

library(tidytext); library(dplyr); library(ggplot2); library(tidyr)
terms(topic2,20)

#Visualize topics in tweets
topic2 %>%
  tidy(matrix='beta')%>%
  group_by(topic)%>%
  top_n(n = 5,wt=beta)%>%
  ungroup()%>%
  ggplot(aes(x=reorder(term,beta),y=beta,fill=factor(topic)))+
  geom_bar(position='dodge', stat='identity')+
  facet_wrap(~topic, scales = 'free')+
  coord_flip()+guides(fill=F)+xlab('')

#gemma
topic2%>%
  tidy('gamma')%>%
  filter(as.integer(document)<=20)%>%
  ggplot(aes(x=reorder(document,as.numeric(document)),y=gamma,fill=factor(topic)))+
  geom_bar(position='fill',stat='identity')+xlab('id')+coord_flip()

#4. Text Classification
# RTextTools is no longer maintained, therefore has been removed from CRAN. You can install an older version 
#library(devtools)
#install_version(package = 'tree',version = '1.0-39',repos = "https://cran.us.r-project.org/")
#install_version(package = 'RTextTools',version = '1.4.2',repos = "https://cran.us.r-project.org/")
install.packages('devtools')
library(devtools)

set.seed(617)
split = sample(1:nrow(tweets), size = 500)
split[-split]

library(RTextTools)
container = create_container(xdtm, tweets[,1], trainSize=split,testSize=split[-split], virgin=FALSE)

#5. Train Machine Learning Models - *** because later we will construct
#predictions to evaluate accuracy of misclassification rate
#(between person and classified text??? misclassification)

svm = train_model(container,"SVM")
glmnet = train_model(container,"GLMNET")
slda = train_model(container,"SLDA")
boosting = train_model(container,"BOOSTING")
bagging = train_model(container,"BAGGING")
rf = train_model(container,"RF")
nnet = train_model(container,"NNET")
tree = train_model(container,"TREE")

### you can see there are 8 ML algorithms - 
print_algorithms()
#"BAGGING"  "BOOSTING" "GLMNET"   "NNET"     "RF"      
#"SLDA"  "SVM"   "TREE"   

true_class = tweets$person[split[-split]]
true_class[1:10]

#construct Predictions of each Machine Learning Models
svm_pred = classify_model(container, svm)
glmnet_pred = classify_model(container, glmnet)
slda_pred = classify_model(container, slda)
boosting_pred = classify_model(container, boosting)
bagging_pred = classify_model(container, bagging)
rf_pred = classify_model(container, rf)
nnet_pred = classify_model(container, nnet)
tree_pred = classify_model(container, tree)


#Evaluate Accuracy of 8 ML model predictions (This is to see misclassification rate)****
#as you can see, nnet ML model has the lowest misclassification rate.
mean(as.character(svm_pred$SVM_LABEL) != true_class)

mean(as.character(glmnet_pred$GLMNET_LABEL) != true_class)

mean(as.character(slda_pred$SLDA_LABEL) != true_class)

mean(as.character(boosting_pred$LOGITBOOST_LABEL) != true_class)

mean(as.character(bagging_pred$BAGGING_LABEL) != true_class)

mean(as.character(rf_pred$FORESTS_LABEL) != true_class)

mean(as.character(nnet_pred$NNETWORK_LABEL) != true_class)

mean(as.character(tree_pred$TREE_LABEL) != true_class)




#B
# 1. Look at Elon Musk's tweet and sentiment analysis
library(rtweet)
tweets_musk = search_tweets(q = 'elonmusk',n = 10000, include_rts = F)

head(tweets_musk)
tweets_musk[1:10, c('screen_name', 'text')]

#Clean text
#convert to lowercase; remove urls; remove emojis; remove punctuation; remove stopwords; remove whitespace
library(tm); library(SnowballC)
corpus2 = Corpus(VectorSource(tweets_musk$text))
library(magrittr)
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '@\\w+',replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '<.*>',replacement = ' ',x = x))) %>%  # remove emojis
  tm_map(content_transformer(function(x)iconv(x = x,from = "latin1", to = "ASCII", sub="")))%>%
  tm_map(removeWords, c(stopwords('english'),'narendramodi','elonmusk','realdonaldtrump','katyperry','&amp;','amp'))%>%
  tm_map(removePunctuation)

#create a dictionary of all words
dict2 = findFreqTerms(DocumentTermMatrix(corpus2),lowfreq = 0)
dict_corpus2 = Corpus(VectorSource(dict2))

#Use tm_map to stem words
# EXAMPLE --- . example: stemDocument(running,runs,ran) would return (run,run,ran) as the ouput.

corpus2 = 
  corpus2 %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)


#Extract words from all tweets. Examine first fifty rows of data. Note the change in the data from wide format to a long/tall format

library(tidytext); library(dplyr)
tweets_words = tweets_musk %>% 
  group_by(screen_name) %>% 
  unnest_tokens(output = word, input = text) %>% 
  ungroup() %>% 
  mutate(row=1:n())
as.data.frame(tweets_words)[1:50, c('screen_name', 'word')]


# Alternative Way - OR
tweets_words = tweets_musk %>% 
  group_by(screen_name) %>% 
  select(screen_name == "musk")
  unnest_tokens(output = word, input = text) %>% 
  ungroup() %>% 
  mutate(row=1:n())
as.data.frame(tweets_words)[1:50, c('screen_name', 'word')]

# 2. Positive or Negative
# There are a number of word lexicons that can be used to classify words
# as being positive or negative. The [Bing lexicon](https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html) categorizes words as being positive and negative

# Examine first fifty rows of bing lexicon
as.data.frame(get_sentiments('bing'))[1:50,]

# 3. Bing - Positive vs. Negative words
#Using the Bing Lexicon we can categorize all words in the tweets by valence. Here is a summary chart of positive and negative words


#see number of postiive and negative sentiment in tweets_musk
# As you can see, #Musk tweets have positive sentiment (64.5%) than negative sentiment (35.5%)
tweets_words %>%
  inner_join(get_sentiments('bing'),by = 'word')%>%
  select('sentiment')%>%
  group_by(sentiment)%>%
  summarize(freq=n())

#Visualize Bing Lexicon

library(ggplot2)
tweets_words %>%
  inner_join(get_sentiments('bing'),by = 'word')%>%
  select('sentiment')%>%
  group_by(sentiment)%>%
  summarize(freq=n())%>%
  ungroup()%>%
  ggplot(aes(x=sentiment,y=freq))+geom_bar(position='dodge',stat='identity',fill=c('darkred','darkgreen'))+
  coord_flip()

# 4. Sentiment -AFINN (scores positive and negative words)
afinn = as.data.frame(get_sentiments('afinn'))[1:50,]
afinn

afinn %>% 
  group_by(value) %>% 
  count()

#5. Scores all words
#By applying the afinn lexicon to the words in the tweets, we can compute a socre fo reach tweet
#By averaging the scores of all tweets, one can obtain an overall sentiment score

#As you can see from the histogram, the highest value of sentiment score is _____
tweets_words %>% 
  inner_join(afinn, by = 'word') %>% 
  select('value') %>% 
  ggplot(aes(x=value))+geom_histogram()

#7. Score EACH TWEET
tweets_words %>%
  left_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(value = mean(value,na.rm=T))%>%
  ungroup()%>%
  select('screen_name','value')%>%
  ggplot(aes(x=value))+geom_histogram()


#8. Sentiment Score for EACH TWEET
#Sentiment score is the average of sentiment fo all tweets. First, we get the sentiment of each tweet
tweets_words %>%
  inner_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(tweet_sentiment = mean(value,na.rm=T)) %>% 
  ungroup()

#overall sentiment score of ALL TWEETS OF MUSK = -1 
tweets_words %>%
  inner_join(afinn,by = 'word')%>%
  group_by(screen_name)%>%
  summarize(tweet_sentiment = mean(value,na.rm=T))%>%
  ungroup()%>%
  summarize(Overall_Sentiment=mean(tweet_sentiment,na.rm=T))

#9. Emotions in Tweets
#The 'nrc' lexicon categorizes words by emotion. This lexicon 
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',header = F,col.names = c('word','sentiment','num'),sep = '\t'); nrc = nrc[nrc$num!=0,]; nrc$num = NULL
nrc %>% 
  group_by(sentiment) %>% 
  count()

#Visualizing Emotions in Tweets_Musk
#chart of Words reflecting Emotion

#Overall, high on positive emotions followed by trust, anticipation!
library(RColorBrewer)
tweets_words %>% 
  inner_join(get_sentiments('nrc'), by = 'word') %>% 
  select('sentiment') %>% 
  group_by(sentiment) %>% 
  summarize(freq=n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=reorder(sentiment, desc(freq)), y=freq, fill=freq))+geom_bar(position = 'dodge', stat = 'identity')+xlab('Sentiment')+ylab('Frequency')+coord_flip()

# 10. A Word Cloud
library(tidyr); library(wordcloud)
wordcloud_data= 
  tweets_words %>%
  anti_join(rbind(stop_words,c('https','SMART'),c('t.co','SMART')),by = 'word')%>%
  count(word,sort=T)%>%
  ungroup()
wordcloud_data= as.data.frame(wordcloud_data)

wordcloud(words = wordcloud_data$word,wordcloud_data$n,scale=c(2,0.5),max.words = 150,colors=brewer.pal(9,"Spectral"))

#11. Comparing Positive and Negative Words Cloud
#A Comparison cloud comparing Positive words to Negative Words
library(tidyr); library(wordcloud)
wordcloud_data= 
  tweets_words %>%
  anti_join(rbind(stop_words,c('trump','SMART')),by = 'word')%>%
  inner_join(get_sentiments('bing'),by='word')%>%
  count(sentiment,word,sort=T)%>%
  ungroup()%>%
  spread(key = sentiment,value = 'n',fill = 0)
wordcloud_data= as.data.frame(wordcloud_data)
rownames(wordcloud_data) = wordcloud_data[,'word']
wordcloud_data = wordcloud_data[,c('positive','negative')]
comparison.cloud(wordcloud_data,scale=c(2,0.5),max.words = 150,rot.per = 0)  


