
##### GROUPED DATA ANALYSIS (Group 1: Green Infrastructure)
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

##### GROUP 1: GREEN INFRASTRUCTURE
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/communitygarden.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/communitygardens.tweet.xlsx")
c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/greenspace.tweet.xlsx")
d <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/greenspace2.tweet.xlsx")
e <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/greenroof.tweet.xlsx")
f <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/urbanforest.tweet.xlsx")
g <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/citygarden.tweet.xlsx")
h <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/streettree.tweet.xlsx")

##### STEP 2: Bind all files together.
group1_tweets <- rbind(a,b,c,d,e,f,g,h)

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group1_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/CAN.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/USA.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/AUS.tweet.xlsx")


group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group1.tweets.xlsx")
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group2.tweets.xlsx")
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group3.tweets.xlsx")
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group4.tweets.xlsx")
group1_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group5.tweets.xlsx")


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

boxplot(group1_tweets$sentiment~group1_tweets$pre_post)


df <- as.data.frame(table)

ggplot(df, aes(Var1, Freq))+
  geom_col()


library("ggExtra")
group1_tweets$created_at <- as.Date(group1_tweets$created_at)
class(group1_tweets$created_at)

g1 <- ggplot(group1_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha= .5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'Group 1: Green Infrastructure',
       caption = 'Data collected from twitter.com')
g1

ggMarginal(g1, type = "histogram", fill="transparent")

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 1: Green Infrastructure',
       caption = 'Data collected from twitter.com')+
  theme_bw()


YEET <- rbind(group1_tweets$created_at, group1_tweets$sentiment, group1_tweets$pre_post)

data <- group1_tweets
data$year <- strftime(data$created_at, "%Y")
data$month <- strftime(data$created_at, "%m")
data_aggr1 <- aggregate(sentiment ~ month + year, data, FUN = sum)

library("lubridate")
data2 <- group1_tweets
data$year_month <- floor_date(data$created_at, "month")

library("dplyr")
group3 <- agg.group1 %>%
  group_by(year.month) %>%
  dplyr::summarize(value=sum(value)) %>%
  as.data.frame()

g3 <- group1_tweets
g3$created_at <- as.name(g3$created_at)
year_month <- str_sub(g3$created_at, 1, 7)
g3.1 <- dplyr::bind_cols(g3,data.frame(year_month))

g3.1





##### GROUP 2: NATURAL AREAS
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/pond.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/ponds.tweet.xlsx")
c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/greenspace.tweet.xlsx")
d <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/greenspace2.tweet.xlsx")
#e <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/citylake.tweet.xlsx")
f <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/citylakes.tweet.xlsx")
g <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/citynature.tweet.xlsx")
h <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbanforest.tweet.xlsx")
i <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/communitytrail.tweet.xlsx")
j <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/communitytrails.tweet.xlsx")
k <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbannature.tweet.xlsx")
l <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/citywildlife.tweet.xlsx")
m <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbanwildlife.tweet.xlsx")
n <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbannatural.tweet.xlsx")
o <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbanriver.tweet.xlsx")
p <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/urbanstream.tweet.xlsx")
q <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/stormpond.tweet.xlsx")
r <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/swm.tweet.xlsx")
s <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/publicpark.tweet.xlsx")
t <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/citysidewalk.tweet.xlsx")
u <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/citybeach.tweet.xlsx")
v <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/publicfield.tweet.xlsx")
w <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GB/streettree.tweet.xlsx")
##citylakes is broken - USA (taken out of searches)

##### STEP 2: Bind all files together.
group2_tweetsGB <- rbind(a, b, c, d, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
group2_tweetsGB <- rbind(a,b,c,d,g,i,k,l,m,n,o,r,s,u,v,w)

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group2_tweetsGB,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/GB.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group2_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/AUS.tweet.xlsx")

##### STEP 4: Conduct T-Test and StDev Tests
t.test(group2_tweets$sentiment)
boxplot(group2_tweets$sentiment~group2_tweets$pre_post)
t.test(group2_tweets$sentiment~group2_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)
t.test(group2_tweets$sentiment[group2_tweets$pre_post=="Pre pandemic"], group2_tweets$sentiment[group2_tweets$pre_post=="Post pandemic"])

sd(group2_tweets$sentiment)
summary(group2_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group2_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group2_tweets$sentiment~group2_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group2_tweets
pre_panini <- my.data[group2_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group2_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group2_tweets$sentiment)

library("ggExtra")
group2_tweets$created_at <- as.Date(group2_tweets$created_at)
class(group2_tweets$created_at)

g1 <- ggplot(group2_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha=.5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'Group 2: Nautral Areas',
       caption = 'Data collected from twitter.com')+
  theme_bw()

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 2: Natural Areas',
       caption = 'Data collected from twitter.com')+
  theme_bw()


ggMarginal(g1, type = "histogram", fill="transparent")
g1

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 2: Natural Areas',
       caption = 'Data collected from twitter.com')+
  theme_bw()




##### STEP 6: SAVE DATA as an excel file
write_xlsx(group2_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/CAN.tweet.xlsx")


##### GROUP 3: RECREATION
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citypark.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/parkette.tweet.xlsx")
c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/cityparks.tweet.xlsx")
d <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitygarden.tweet.xlsx")
e <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/covidwalk.tweet.xlsx")
f <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanpark.tweet.xlsx")
g <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitygardens.tweet.xlsx")
h <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/pandemicwalk.tweet.xlsx")
i <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanforest.tweet.xlsx")
j <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitytrail.tweet.xlsx")
k <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitytrails.tweet.xlsx")
l <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanparks.tweet.xlsx")
m <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanagriculture.tweet.xlsx")
n <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbannatural.tweet.xlsx")
o <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/stormpond.tweet.xlsx")
p <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/swm.tweet.xlsx")
q <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citygarden.tweet.xlsx")
r <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citybeach.tweet.xlsx")
s <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citysidewalk.tweet.xlsx")
t <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/publicpark.tweet.xlsx")
u <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/publicfield.tweet.xlsx")
v <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/streettree.tweet.xlsx")

##### STEP 2: Bind all files together.
group3_tweets <- rbind(a,c,e,q,r,t,u,v)

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group3_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/AUS.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group3_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/AUS.tweet.xlsx")

##### STEP 4: Conduct T-Test and StDev Tests
t.test(group3_tweets$sentiment)
sd(group3_tweets$sentiment)
summary(group3_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group3_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group3_tweets$sentiment~group3_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group3_tweets
pre_panini <- my.data[group3_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group3_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group3_tweets$sentiment)

library("ggExtra")
group3_tweets$created_at <- as.Date(group3_tweets$created_at)
class(group3_tweets$created_at)

g1 <- ggplot(group3_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha=.5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'Group 3: Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()


ggMarginal(g1, type = "histogram", fill="transparent")
g1

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 3: Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()






2late <- rbind(group3_tweets$created_at, group3_tweets$sentiment, group3_tweets$pre_post)

data <- group3_tweets
data$year <- strftime(data$created_at, "%Y")
data$month <- strftime(data$created_at, "%m")
data_aggr1 <- aggregate(sentiment ~ month + year, data, FUN = sum)

library("lubridate")
data2 <- group3_tweets
data$year_month <- floor_date(data$created_at, "month")

library("dplyr")
group3 <- agg.group3 %>%
  group_by(year.month) %>%
  dplyr::summarize(value=sum(value)) %>%
  as.data.frame()

g3 <- group3_tweets
g3$created_at <- as.name(g3$created_at)
year_month <- str_sub(g3$created_at, 1, 7)
g3.1 <- dplyr::bind_cols(g3,data.frame(year_month))


#### PLOT Draft: Percentage of Pos.neu.neg tweets over time
final <- ggplot(g3.1, aes(x=year_month, fill = pos.neu.neg) +
  geom_bar(position='fill') +
  labs(x='Date', y='Percentage of Tweet Polarity',
       title = 'Percentage of Tweet Polarity in Recreation-related Tweets Over Time',
       subtitle = 'Group 3: Recreation',
       caption = 'Data collected from twitter.com')+
  theme_bw()
 
ggMarginal(final, type = "histogram", fill="transparent")


##### STEP 6: SAVE DATA as an excel file
write_xlsx(group3_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/CAN.tweet.xlsx")


##### GROUP 4: BLUE INFRASTRUCTURE
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/pond.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/ponds.tweet.xlsx")
#c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/S/citylake.tweet.xlsx")
d <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citylakes.tweet.xlsx")
e <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/stormpond.tweet.xlsx")
f <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanriver.tweet.xlsx")
g <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanstream.tweet.xlsx")
h <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/swm.tweet.xlsx")
i <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citybeach.tweet.xlsx")

#*redo city lakes for America

##### STEP 2: Bind all files together.
group4_tweets <- rbind(a,b,i)

group4_tweetsGB <- group4_tweets

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group4_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/AUS.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group4_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/AUS.tweet.xlsx")

##### STEP 4: Conduct T-Test and StDev Tests
t.test(group4_tweets$sentiment)
sd(group4_tweets$sentiment)
summary(group4_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group4_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group4_tweets$sentiment~group4_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group4_tweets
pre_panini <- my.data[group4_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group4_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group4_tweets$sentiment)

boxplot(group5_tweets$sentiment~group5_tweets$pre_post)
group4_tweets$created_at <- as.Date(group4_tweets$created_at)
class(group4_tweets$created_at)

g1 <- ggplot(group4_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha=.5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'Group 4: Blue Infrastructure',
       caption = 'Data collected from twitter.com')+
  theme_bw()

ggMarginal(g1, type = "histogram", fill="transparent")
g1

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 4: Blue Infrastructure',
       caption = 'Data collected from twitter.com')+
  theme_bw()


##### STEP 6: SAVE DATA as an excel file
write_xlsx(group4_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/CAN.tweet.xlsx")


##### GROUP 5: AGRICULTURE
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/urbanagriculture.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitygarden.tweet.xlsx")
c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/communitygardens.tweet.xlsx")
d <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/citygarden.tweet.xlsx")
e <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/AU/cityfarm.tweet.xlsx")

##### STEP 2: Bind all files together.
group5_tweets <- rbind(d,e)

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group5_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/AUS.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group5_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/AUS.tweet.xlsx")


##### STEP 4: Conduct T-Test and StDev Tests
t.test(group5_tweets$sentiment)
sd(group5_tweets$sentiment)
summary(group5_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group5_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

Chi_square <- group4_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group5_tweets$sentiment~group5_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group5_tweets
pre_panini <- my.data[group5_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group5_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group5_tweets$sentiment)

boxplot(group5_tweets$sentiment~group5_tweets$pre_post)

group5_tweets$created_at <- as.Date(group5_tweets$created_at)
class(group5_tweets$created_at)
g1 <- ggplot(group5_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha=.5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Australian Tweets Over Time',
       subtitle = 'Group 5: Agriculture',
       caption = 'Data collected from twitter.com')+
  theme_bw()

ggMarginal(g1, type = "histogram", fill="transparent")
g1

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 5: Agriculture',
       caption = 'Data collected from twitter.com')+
  theme_bw()

##### STEP 6: SAVE DATA as an excel file
write_xlsx(group5_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/CAN.tweet.xlsx")


##### GROUP 6: COVID-19
##### STEP 1: Add Data into R.

a <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/covidwalk.tweet.xlsx")
b <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/covidcity.tweet.xlsx")
c <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/pandemicwalk.tweet.xlsx")

##### STEP 2: Bind all files together.
group6_tweets <- rbind(a, b)

##### STEP 3: Export to excel to delete all duplicate data.
write_xlsx(group6_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/6/AUS.tweet.xlsx")

##### STEP 4: Reload back into R with the duplicates deleted.
group6_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/6/GB.tweet.xlsx")

group6_tweets <- group6_tweetsGB

##### STEP 4: Conduct T-Test and StDev Tests
t.test(group6_tweets$sentiment)
sd(group6_tweets$sentiment)
summary(group6_tweets$sentiment)

##### STEP 5: Conduct a Chi-Square Test
Chi_square <- group6_tweets[, c("pre_post", "pos.neu.neg")]
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table
t.test(group6_tweets$sentiment~group6_tweets$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test

print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

my.data <- group6_tweets
pre_panini <- my.data[group6_tweets$pre_post== "Pre pandemic", ]
post_panini <- my.data[group6_tweets$pre_post== "Post pandemic", ]

t.test(pre_panini$sentiment)
t.test(post_panini$sentiment)
t.test(group6_tweets$sentiment)

boxplot(group6_tweets$sentiment~group6_tweets$pre_post)

group6_tweets$created_at <- as.Date(group6_tweets$created_at)
class(group6_tweets$created_at)
g1 <- ggplot(group6_tweets, aes(x=created_at, y=sentiment, colour=pre_post), x_int = "2020-03-11")+
  geom_point(size = .5, alpha=.5)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in UK Tweets Over Time',
       subtitle = 'Group 6: COVID-19',
       caption = 'Data collected from twitter.com')+
  theme_bw()

ggMarginal(g1, type = "histogram", fill="transparent")
g1

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Group 6: COVID-19',
       caption = 'Data collected from twitter.com')+
  theme_bw()






##### STEP 6: SAVE DATA as an excel file
write_xlsx(group4_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/CAN.tweet.xlsx")




##### PLOTTING



### Sentiment Scores (syuzhet) Since 2018
ggplot(group6_tweets, aes(x=created_at, y=sentiment)) +
  geom_point(col='dark blue', alpha=.25)+
  geom_smooth(se= FALSE, level = .995, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Canadian Tweets Over Time',
       subtitle = 'search query = city park , method = syuzhet')

##### DOTPLOT: Plotting the # Tweets with Negative vs Positive Words Since 2018
ggplot(final_tweets, aes(x=created_at), show.legend = FALSE) +
  geom_smooth(aes(y=negative), col='red')+
  geom_smooth(aes(y=positive), col='green')+
  labs(x='Date (years)', y='Number of Positive Words in tweet',
       title = 'Number of positive/negative words used per tweet Since 2018',
       subtitle = 'search query = city lake , method = NRC')

##### CHART: Indicating the change in mean of emotions in tweets Since 2018
ggplot(group1_tweets, aes(x=created_at), show.legend = TRUE) +
  geom_smooth(aes(y=anger, colour=anger), col='red',se= FALSE)+
  geom_smooth(aes(y=anticipation, colour=anticipation),col='green', se= FALSE)+
  geom_smooth(aes(y=disgust, colour=disgust),col='purple', se= FALSE)+
  geom_smooth(aes(y=fear, colour=fear),col='orange', se= FALSE)+
  geom_smooth(aes(y=joy, colour=joy),col='yellow', se= FALSE)+
  geom_smooth(aes(y=sadness, colour=sadness),col='light blue', se= FALSE)+
  geom_smooth(aes(y=surprise, colour=surprise),col='black', se= FALSE)+
  geom_smooth(aes(y=trust, colour=trust),col='grey', se= FALSE)+
  labs(x='Date (years)', y='Average # of positive/negative words used per tweet',
       colour = 'Emotions', title = 'Average change in emotional words used per AU tweet Since 2018',
       subtitle = 'search query = city park')


### Chi-Square Plot
ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()

