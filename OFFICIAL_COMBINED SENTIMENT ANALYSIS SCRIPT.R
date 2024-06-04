
##### STEP 1: Set up libraries and install packages
library("academictwitteR")
library("writexl")
library("readxl")
library("graphics")
library("purrr")
library("stringr") 
library("tm")
library("syuzhet")
library("ggplot2")
library("CGPfunctions")
library("lsr")
library("dplyr")
library("summarytools")
library("tidyr")
library("scales")
library("wordcloud")
library("reshape2")
library("wordcloud")
library("stringr")
library("widyr")
library("topicmodels")
library("arules")


###### STEP 2: Tweet Extraction

### GET AND SET BEARER TOKEN IN ".Renviron" File
get_bearer()
set_bearer()

### EXTRACTING TWEETS INTO JSON FILE (UNBINDED and not in data.frame format)

ug.twitter <-
  get_all_tweets(
    query = "Emys orbicularis",
    exclude = c("ninja", "tmnt", "toy", "chocolate", "music", "art", "band", "drawing", "jacks", "neck", "lake", "cabin", "island", "clothing", "bay", "brand"),
    start_tweets = "2015-01-01T00:00:00Z",
    end_tweets = "2022-06-30T00:00:00Z",
    data_path = "/Users/connornixon/Desktop/academictwitteR2/ECCC/raw_data/US/s.pondturtle",
    bind_tweets = FALSE,
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAKzZQgEAAAAAWI7XAx5fEcFacpYRMCdxo3vy3nk%3DPmikpwNa4kjlRJgYkamNwqLGE7bCuJCJGgaDb3xMUiAHItFyN1",
    country = "US",
    lang = "en",
    n=Inf )

##### BINDING TWEETS JSON FILES TO data.frame OBJECTS
tweet <- bind_tweets(data_path = "/Users/connornixon/Desktop/academictwitteR2/ECCC/raw_data/US/s.pondturtle")


##### STEP 3: Download & Re-upload Tweets from Microsoft Excel
write_xlsx(tweet,"/Users/connornixon/Desktop/academictwitteR2/ECCC/data/US/spinysoftshellturtle.xlsx")

ug.twitter <-
  get_all_tweets(
    query = "blandings turtle",
    query = "trade"
    c("blandings turtle", "turtle import", "turtle export", "buy turtle", "sell turtle", "wanted turtle"),
    exclude = c("ninja", "toy", "chocolate", "music", "art", "band", "drawing", "jacks", "neck", "lake", "cabin"),
    start_tweets = "2015-01-01T00:00:00Z",
    end_tweets = "2022-06-30T00:00:00Z",
    data_path = "/Users/connornixon/Desktop/academictwitteR/data/original.data/GB/blandings3us.tweet",
    bind_tweets = FALSE,
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAKzZQgEAAAAAWI7XAx5fEcFacpYRMCdxo3vy3nk%3DPmikpwNa4kjlRJgYkamNwqLGE7bCuJCJGgaDb3xMUiAHItFyN1",
    country = "CA",
    lang = "en",
    n=10 )

##### BINDING TWEETS JSON FILES TO data.frame OBJECTS
snap2 <- bind_tweets(data_path = "/Users/connornixon/Desktop/academictwitteR/data/original.data/GB/snapper2.tweet")


##### STEP 3: Download & Re-upload Tweets from Microsoft Excel
write_xlsx(tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/data1/US/publicfield.tweet.xlsx")

##### In MS Excel, delete all unessassary and/or combined columns and save.


### STEP 4: RE-uploading Cleaned Excel Document back into R
tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/data1/US/publicfield.tweet.xlsx")


##### STEP 5: Moving data to 'twitterCorpus' file to clean dataframe $text
```{r}
twitterCorpus <-Corpus(VectorSource(tweets$text))
#inspect(twitterCorpus[1:10])
twitterCorpus<- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus<- tm_map(twitterCorpus,removeWords,stopwords("en"))
twitterCorpus<- tm_map( twitterCorpus,removeNumbers)
twitterCorpus<- tm_map( twitterCorpus,removePunctuation)

removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)   
twitterCorpus<- tm_map(twitterCorpus,content_transformer(removeURL))

removeNonAscii<-function(x) textclean::replace_non_ascii(x) 
twitterCorpus<-tm_map(twitterCorpus,content_transformer(removeNonAscii))

twitterCorpus<- tm_map(twitterCorpus,removeWords,c("amp","ufef",
                                                   "ufeft","uufefuufefuufef","uufef","s"))

twitterCorpus<- tm_map(twitterCorpus,stripWhitespace)

inspect(twitterCorpus[1:10])


##### STEP 6: Moving Cleaned 'TwitterCorpus' Data to a Seperate Dataframe
dataframe <- data.frame(text=sapply(twitterCorpus, identity), 
                        stringsAsFactors=F)

##### STEP 7: Export new dataframe to excel and replace this newly cleaned $text coloumn with new data to prepare for the data analysis
# *use 'data2' for saving the twitterCopus file.*
write_xlsx(dataframe,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/data2/US/publicfield.tweet.cleanedtweets.xlsx")

##### STEP 8: Copy + Paste New Clean Data into the Previous Excel Document.
# In excel, copy the $text file from the saved dataframe and paste the cleand data into the previous excel document.
# xlsx files 2nd time
tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/data2/CA/communitygarden.tweet.xlsx")


##### STEP 9: Generate Sentiment Scores as '$sentiment' using syuzhet method
get_sentiment(tweets$text[1:10])
sentiment <- get_sentiment(tweets$text)
sentimentTweets <- dplyr::bind_cols(tweets,data.frame(sentiment))

##### STEP 10: Determining the Mean of Sentiment Positivity
meanSent<-function(i,n){
  mean(sentimentTweets2$sent[i:n])
}

##### STEP 11: Determining the Sentiment Scores for all of the Tweets
scores<-c(ALL=meansentiment(1,1817))

##### STEP 12: Create a rough geom_point Graph depicting the Sentiment Scores over time
ggplot(OFFICIAL2, aes(x=created_at, y=sentiment)) +
  geom_point( aes(alpha = 1), stat = "identity", show.legend = TRUE) +
  labs(x='Date (Years)', y='Tweet Sentiment Score',
  title = 'Sentiment in Tweets Since July 2018',
  subtitle = 'search query = city park')

##### STEP 13: Creating a Regular Sentiment Score using get_sentiment() Function
# ***please note: different methods may have different scales.***
# see the first row of the vector
# see summary statistics of the vector
syuzhet_vector <- get_sentiment(sentimentTweets$text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

# bing vector
bing_vector <- get_sentiment(sentimentTweets$text, method="bing")
head(bing_vector)
summary(bing_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(bing_vector))

#affin vector
afinn_vector <- get_sentiment(sentimentTweets$text, method="afinn")
head(afinn_vector)
summary(afinn_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(afinn_vector))

#nrc vector
nrc_vector <- get_sentiment(sentimentTweets$text, method="nrc")
head(nrc_vector)
summary(nrc_vector)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(nrc_vector))

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)))

##### STEP 14: Run NRC Sentiment Analysis to Return Dataframe with Each Row Classified as emotions rather than score.
##### **anger, anticipation, disgust, fear, joy, sadness, surprise, trust**
##### **Also counts the number of positive and negative emotions found / row.
d <- get_nrc_sentiment(sentimentTweets$text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

##### STEP 15: Bind NRC Sentiment Analysis to Dataframe
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(d))


##### STEP 16: Identify Mean and StDev within '$sent' to Determine a 95% Confidence Interval
summary(sentimentTweets$sentiment)
pos.neu.neg <- factor(sign(sentimentTweets$sentiment), (-1):1, c('negative', 'neutral', 'positive'))
final_tweets <- dplyr::bind_cols(sentimentTweets,data.frame(pos.neu.neg))

##### Choose specific country
final_tweets$Country <- "Australia"
final_tweets$Country <- "Canada"
final_tweets$Country <- "New Zealand"
final_tweets$Country <- "United Kingdom"
final_tweets$Country <- "United States"

class(final_tweets$created_at)
final_tweets$created_at <- as.Date(final_tweets$created_at)
class(final_tweets$created_at)
pre_panini <- final_tweets[final_tweets$created_at > "2018-01-01" & final_tweets$created_at < "2020-03-12", ]
post_panini <- final_tweets[final_tweets$created_at > "2020-03-11" & final_tweets$created_at < "2021-09-01", ]
pre_panini$pre_post <- "Pre pandemic"
post_panini$pre_post <- "Post pandemic"

final_tweets <- rbind(pre_panini, post_panini)
Chi_square <- final_tweets[, c("pre_post", "pos.neu.neg")]

#table(Chi_square$final_tweets.pos.neu.neg, Chi_square$final_tweets.pre_post)
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()

##### STEP xx: 2 and 2 Sample T-Tests as well as the Stdev of the dataset.
sd(final_tweets$sentiment)
t.test(final_tweets$sentiment)
boxplot(final_tweets$sentiment~final_tweets$pre_post)
t.test(final_tweets$sentiment~final_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)
t.test(final_tweets$sentiment[final_tweets$pre_post=="Pre pandemic"], final_tweets$sentiment[final_tweets$pre_post=="Post pandemic"])
summary(final_tweets$sentiment)

##### STEP 17: Write Excel Document to 
write_xlsx(final_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/communitygarden.tweet.xlsx")

##### STEP 18: Re-upload Excel Document to begin Creating Charts
final_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/citylake.tweet.xlsx")

CA <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/citylakes.tweet.xlsx")
US <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/pond.tweet.xlsx")
AU <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/pond.tweet.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/pond.tweet.xlsx")

comb_tweets <- rbind(CA, US, AU, GB)

write_xlsx(comb_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/citylakes.tweet.xlsx")


##### STEP 19: Create geom_point + geom_smooth Plot to Highlight the '$sent' Scores Since 2018 
#syuzhet method
ggplot(comb_tweets, aes(x=created_at, y=sentiment)) +
  geom_point(size = .5, alpha = .5, aes(fill=Country))+
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Canadian Tweets Over Time',
       subtitle = 'search query = city park , method = syuzhet')
#bing
ggplot(final_tweets, aes(x=created_at, y=bing_vector)) +
  geom_point(col='dark blue')+
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Canadian Tweets Over Time',
       subtitle = 'search query = city park , method = bing')
#afinn
ggplot(final_tweets, aes(x=created_at, y=afinn_vector)) +
  geom_point(col='dark blue')+
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'search query = city lake , method = afinn')
#get_sentiment fxn
ggplot(final_tweets, aes(x=created_at, y=sentiment)) +
  geom_point(col='dark blue')+
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australia Tweets Over Time',
       subtitle = 'search query = city park , method = syuzhet')

ggplot(final_tweets, aes(x=created_at))+
  geom_smooth(aes(y=afinn_vector), col='orange',se= FALSE)+
  geom_smooth(aes(y=bing_vector),col='blue', se= FALSE)+
  geom_smooth(aes(y=nrc_vector), col='red',se= FALSE)+
  geom_smooth(aes(y=sentiment),col='green', se= FALSE)+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Observing different sentiment analysis methods',
       subtitle = 'search query = city lake , method = multiple')



##### SIMPLE_PLOT: Plot of each vector with a scaled sentiment (-1 to 1) with Simplified Macro Shape
simple_plot(afinn_vector, title = "Afinn Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(bing_vector, title = "Bing Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(syuzhet_vector, title = "Syuzhet Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(nrc_vector, title = "NRC Plot", legend_pos = "topright", lps = 10, window = 0.1)

##### GGPLOT_SMOOTH: Negative-Positive NRC Sentiment Tweet Graph
ggplot(final_tweets, aes(x=created_at, y=positive-negative)) +
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "navy blue")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Positive-Negative in Tweets Since 2018',
       subtitle = 'search query = city park')


##### BARPLOT: Plotting the Most Frequent Words from the Dataframe
##### **reminder to exclude the first initial lines as that is what the twitter query will be**
barplot(dtm_d[2:21,]$freq, las = 2, names.arg = dtm_d[2:21,]$word,
        col ="red", main ="Top 20 most frequent words",
        ylab = "Word frequencies")

##### DOTPLOT: Plotting the # Tweets with Negative Words Since 2018
ggplot(final_tweets, aes(x=created_at, y=negative)) +
  geom_point(col='dark red', alpha = 0.35)+
  geom_smooth(se= FALSE, level = .995, span = 0.5, col= "black")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'search query = city park')

##### DOTPLOT: Plotting the # Tweets with Negative vs Positive Words Since 2018
ggplot(final_tweets, aes(x=created_at)) +
  geom_jitter(aes(y=negative), col='red')+
  geom_jitter(aes(y=positive), col='green')+
  labs(x='Date (years)', y='Number of Positive Words in tweet',
       title = 'Number of positive/negative words used per tweet Since 2018',
       subtitle = 'search query = city lake')

##### Line: Mean of Each Country
ggplot(comb_tweets, aes(x=created_at, col = Country, filter=Country)) +
  geom_smooth(aes(y=positive-negative),se= FALSE)+
  labs(x='Date (years)', y='Average # of positive/negative words used per tweet',
       title = 'Comparing NRCs Positive vs Negative scores Since 2018 CA',
       subtitle = 'search query = city park , method = NRC')


##### CHART: Indicating the change in mean for positive and negative tweets Since 2018
ggplot(comb_tweets, aes(x=created_at)) +
  geom_smooth(aes(y=negative), col='red',se= FALSE)+
  geom_smooth(aes(y=positive),col='green', se= FALSE)+
  labs(x='Date (years)', y='Average # of positive/negative words used per tweet',
       title = 'Comparing NRCs Positive vs Negative scores Since 2018 CA',
       subtitle = 'search query = city park , method = NRC')

##### CHART: Indicating the change in mean of emotions in tweets Since 2018
ggplot(final_tweets, aes(x=created_at), show.lengend = TRUE) +
  geom_smooth(aes(y=anger), col='red',se= FALSE)+
  geom_smooth(aes(y=anticipation),col='green', se= FALSE)+
  geom_smooth(aes(y=disgust),col='purple', se= FALSE)+
  geom_smooth(aes(y=fear),col='orange', se= FALSE)+
  geom_smooth(aes(y=joy),col='yellow', se= FALSE)+
  geom_smooth(aes(y=sadness),col='light blue', se= FALSE)+
  geom_smooth(aes(y=surprise),col='black', se= FALSE)+
  geom_smooth(aes(y=trust),col='grey', se= FALSE)+
  labs(x='Date (years)', y='Average # of positive/negative words used per tweet',
       title = 'Average change in emotional words used per AU tweet Since 2018',
       subtitle = 'search query = city park')

d<- ggplot(data = US, aes(x = created_at, y = sentiment)) +
  geom_smooth(se= FALSE, col = 'light blue', size = .5)

ggplot(a + b c + d)
a
b
c
d

ggplot(data = comb_tweets, aes(x = created_at, y = sentiment)) +
  geom_smooth(se= FALSE, col = 'orange', size = .5)+
  labs(title = "Comparison of Senitment Scores between Countries",
       subtitle = "query used = city lake , method = syuzhet",
       y = "Sentiment Score", x = "Date (years)")

ggplot(data = comb_tweets, aes(x = created_at, y = sentiment)) +
  geom_line(color = "steelblue", size = .025) +
  geom_point(color = "steelblue", size = .25) +
  geom_smooth(se= FALSE, col = 'orange', size = .5)+
  labs(title = "Comparison of Senitment Scores between Countries",
       subtitle = "query used = city lake , method = syuzhet",
       y = "Sentiment Score", x = "Date (years)") + 
  facet_grid(Country ~ .)

sentimentTweets_count <- filter(comb_tweets, Country%in%c("Canada", "United Kingdom", "United States", "Australia")) %>% 
  mutate(Country = factor(Country, levels = c("Canada", "United Kingdom", "United States", "Australia")))

ggplot(data = comb_tweets, aes(created_at , sentiment, color = Country)) +
  geom_smooth(size = 1, se= FALSE) +
  geom_point(size = .25) + 
  labs(title = "Comparing Seniment Scores between nations",
       subtitle = "query used = city lake , method = syuzhet",
       y = "Sentiment Scores", x = "Year")


data <- comb_tweets
data$year <- strftime(data$created_at, "%Y")
data$month <- strftime(data$created_at, "%m")
data_aggr1 <- aggregate(sentiment ~ month + year, data, FUN = sum)

data$created_at <- as.Date(data$created_at)
class(data$created_at)

library("lubridate")
data2 <- comb_tweets
data$year_month <- floor_date(data$created_at, "month")

library("dplyr")
group3 <- agg.group3 %>%
  group_by(year_month) %>%
  dplyr::summarize(value=sum(value)) %>%
  as.data.frame()

g3 <- comb_tweets
g3$created_at <- as.name(g3$created_at)
year_month <- str_sub(g3$created_at, 1, 7)
g3.1 <- dplyr::bind_cols(g3,data.frame(year_month))


#### PLOT Draft: Percentage of Pos.neu.neg tweets over time
final <- ggplot(g3.1) +
  aes(x=year_month, fill = pos.neu.neg) +
  geom_bar(position='fill') +
  labs(x='Date', y='Percentage of Tweet Polarity',
       title = 'Percentage of Tweet Polarity in Recreation-related Tweets Over Time',
       subtitle = 'Group 3: Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()

ggMarginal(final, type = "histogram", fill="transparent")



ggplot(sentimentTweets, aes(x=created_at)) +
  geom_smooth(aes(y=anger), col='red',se= FALSE)+
  geom_smooth(aes(y=anticipation),col='green', se= FALSE)+
  geom_smooth(aes(y=disgust),col='purple', se= FALSE)+
  geom_smooth(aes(y=fear),col='orange', se= FALSE)
  

emotions<-get_sentiment(sentimentTweets$text)
barplot(colSums(emotions),cex.names = .4,
        col = rainbow(10),
        main = "Sentiment scores for tweets")

tweets4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/data2/CA/greenspace2.tweet.xlsx")
final_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/greenspace2.tweet.xlsx")
write_xlsx(sentimentTweets2,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/greenspace2.tweet.xlsx")
write_xlsx(d,"/Users/connornixon/Desktop/emotions.xlsx")

simple_plot(afinn_vector, title = "Afinn Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(bing_vector, title = "Bing Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(syuzhet_vector, title = "Syuzhet Plot", legend_pos = "topright", lps = 10, window = 0.1)
simple_plot(nrc_vector, title = "NRC Plot", legend_pos = "topright", lps = 10, window = 0.1)



x <- rescale(sentimentTweets$sentiment)
sentimentTweets <- dplyr::bind_cols(sentimentTweets,data.frame(x))

