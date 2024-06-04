
library("ggExtra")


## Bind All countries data together
AUS <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/AUS.tweet.xlsx")
CAN <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/CAN.tweet.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/GB.tweet.xlsx")
USA <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/USA.tweet.xlsx")

##### STEP 2: Bind all files together.
group1_tweets <- rbind(AUS, CAN, GB, USA)

write_xlsx(group1_tweets, "/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group3.tweets.xlsx")

##### STEP 4: Conduct T-Test and StDev Tests
t.test(group1_tweets$sentiment)
sd(group1_tweets$sentiment)
summary(group1_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group1_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group1_tweets$sentiment~group1_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group1_tweets
pre_panini <- my.data[group1_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group1_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group1_tweets$sentiment)

USA$created_at <- as.Date(USA$created_at)
class(USA$created_at)

#DOT+SMOOTH: Overall Sentiment Measurement
g1 <- ggplot(USA, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha= .5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in American Tweets Over Time',
       subtitle = 'Group 1:Green Infrastructure',
       caption = 'Data collected from twitter.com')
g1

ggMarginal(g1, type = "histogram", fill="transparent")

#BAR: Shift in Polarity Pre-Post
ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 3:Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()

#PLOT: Mean Sentiment Scores
tweets4$created_at <- as.Date(tweets4$created_at)

ggplot(tweets4, aes(created_at, sentiment, colour=Country))+
  geom_smooth(se=FALSE, level=.95)+
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Mean Tweet Sentiment Scores by Country Over Time',
       subtitle = 'Group 3:Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()


#PLOT: Violin Graph
ggplot(group1_tweets, aes(Country, sentiment))+
  geom_violin()+
  labs(x='Country', y='Sentiment',
       title = 'Overall Sentiment in Tweets by Country',
       subtitle = 'Group 3:Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()

