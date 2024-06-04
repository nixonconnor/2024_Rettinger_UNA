
load.packages("scales")
library(ggExtra)

###### Add Country groups:
group1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/GB.tweet.xlsx")
group2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/GB.tweet.xlsx")
group3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/GB.tweet.xlsx")
group4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/GB.tweet.xlsx")
group5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/GB.tweet.xlsx")
group6 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/6/GB.tweet.xlsx")
grouped_CAN <- rbind(group1, group2, group3, group4, group5)

USA <- distinct(grouped_CAN)


USA <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/USA.tweet.xlsx")

#CANADA CLEAN UP
my.data1 <- USA

pre_panini1 <- my.data1[USA$pre_post== "Pre pandemic", ]
post_panini1 <- my.data1[USA$pre_post== "Post pandemic", ]

t.test(pre_panini1$sentiment)
t.test(post_panini1$sentiment)
t.test(USA$sentiment)
t.test(USA$sentiment~USA$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)
Chi_square <- USA[, c("pre_post", "pos.neu.neg")]

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test
print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)


final_tweets$created_at <- as.Date(final_tweets$created_at)
class(final_tweets$created_at)

CAN <- final_tweets

plot1 <- ggplot(CAN, aes(x=created_at, y=sentiment, colour=pre_intra), x_int = "2020-03-11")+
  geom_point(size = .75)+
  geom_smooth(se= FALSE, level = .95, span = 0.3, col= "black")+
  scale_y_continuous(breaks=seq(-8, 8, 2)) +
  scale_x_date(date_breaks = "6 months",
               minor_breaks = "1 month",
               date_labels = "%Y-%b")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Tweet Sentiment Since 2018',
       subtitle = 'Country: Canada     Serch Query: pond',
       caption = 'Data collected from twitter.com')+
  theme_bw()
plot1

ggMarginal(plot1, type = "histogram", fill="transparent")

plot3 <- ggplot(Chi_square) +
  aes(x = pre_intra, fill = pos.neu.neg) +
  geom_bar()+
  labs(x='Pre/Post Pandemic', y='Count of Tweets', title= 'Frequency Distribution of Pre- vs Post-pandemic Tweets',
       subtitle = 'Country: Canada     Serch Query: pond',
       caption = 'Data collected from twitter.com')+
  theme_bw()
plot3


ggplot(CAN, aes(x=created_at), show.legend = TRUE) +
  geom_smooth(aes(y=anger, colour=anger), col='red',se= FALSE)+
  geom_smooth(aes(y=anticipation, colour=anticipation),col='green', se= FALSE)+
  geom_smooth(aes(y=disgust, colour=disgust),col='purple', se= FALSE)+
  geom_smooth(aes(y=fear, colour=fear),col='brown', se= FALSE)+
  geom_smooth(aes(y=joy, colour=joy),col='gold', se= FALSE)+
  geom_smooth(aes(y=sadness, colour=sadness),col='blue', se= FALSE)+
  geom_smooth(aes(y=surprise, colour=surprise),col='black', se= FALSE)+
  geom_smooth(aes(y=trust, colour=trust),col='grey', se= FALSE)+
  labs(x='Date (years)', y='Average Number of NRC Emotions Used per Tweet',
       colour = 'Emotions', title = 'Average Change in Emotions per American Tweet Since 2018',
       subtitle = 'Country: USA',
       caption = 'Data collected from twitter.com')+
  theme_bw()



plot1
plot2
plot3

write_xlsx(CANADA,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/aafigures/CAN.tweet.xlsx")
write_xlsx(USA,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/US/USA.tweet.xlsx")

group1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/CAN.tweet.xlsx")
group2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/CAN.tweet.xlsx")
group3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/CAN.tweet.xlsx")
group4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/CAN.tweet.xlsx")
group5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/CAN.tweet.xlsx")
grouped_CAN <- rbind(group1, group2, group3, group4, group5)


grouped_CAN <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group1.tweets.xlsx")


CANADA <- distinct(grouped_CAN)

USA <- CANADA

aa <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/aafigures/CANtest.tweet.xlsx")
USA <- aa

group1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/CAN.tweet.xlsx")


packages <- c("readxl","tidytext","plyr","dplyr","tidyr","ggplot2","scales",
              "purrr","textdata","wordcloud","reshape2","stringr","igraph",
              "ggraph","widyr","grid","arules","tm","topicmodels")
for(i in packages){
  if(!require(i,character.only = T, quietly = T)){
    install.packages(i)
  }
  library(i, character.only = T, quietly = T)
}
set.seed(2021)

CAN <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group5.tweets.xlsx")

CAN <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/Word Association_excel/group5.tweets.update.xlsx")

USA <- distinct(final_tweets)
USA <-na.omit(USA)


##Word Association
dfList<-list(USA$text)
result_list <- llply(dfList, function(x) {
  #only keep tweets using English as the main language
  x<-subset(x,x$`lang`== "en") 
  #change the variable name for future convenience
  x$tweet=x$`text` 
  #drop all other variables except tweet
  x<-x[,3] 
  #create a new variable to track the number of tweet
  x$tweetnumber<-1:length(x$text)
  #return the cleaner dataframe with 2 variables
  return(x) 
})

result_list[[1]]

#apply the function to each dataset
twts2<-as.data.frame(result_list[1])


data(stop_words)

#customize stop words
custom_stop_words <- bind_rows(
  tibble(word = c("asdfghjkll"), 
         lexicon = c("custom")), stop_words)


remove_reg <- "&amp;|&lt;|&gt;"

#create a list containing all six data frames
dfList2<-list(twts2)
dfList2<-na.omit(dfList2)
result_list2 <- 
  llply(dfList2, function(x) {
    y <- x %>% 
      #remove special symbols for the values under the tweet variable
      mutate(tweet = str_remove_all(text, remove_reg)) %>%
      #extract every word from every tweet 
      unnest_tokens(word, text, token = "tweets") %>%
      #filter out all stop words
      filter(!word %in% custom_stop_words$word,
             !word %in%str_remove_all(custom_stop_words$word, "'"),
             str_detect(word, "[a-z]"))
    return(y)})

result_list2 [[1]]


tidy1<-as.data.frame(result_list2[1])
CANADA_1 <- tidy1 %>%dplyr::count(word, sort = TRUE) 
CANADA_tidy <- CANADA_1[which(!grepl("[^\x01-\x7F]+", CANADA_1$word)),]
dfList3<-list(CANADA_tidy)

result_list3 <- 
  llply(dfList3, function(x) {
    plot <- x %>%
      #keep only the top 20 tokens
      dplyr::top_n(20) %>%
      #reorder word based on the count
      dplyr::mutate(word = reorder(word, n)) %>%
      #plot using ggplot2
      ggplot(aes(word, n, fill=word)) +
      #specify it's a bar plot
      geom_bar(stat="identity")+
      scale_fill_hue(c=45, l=80)+
      xlab(NULL) +
      coord_flip()+
      theme(legend.position="none")
    return(plot)})

result_list3[[1]]

result_list3[[1]]

dfList4<-list(tidy1)


nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

result_list4 <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(nrc_positive) %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::top_n(20) %>%
      dplyr::mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill=word)) +
      geom_bar(stat="identity")+
      scale_fill_hue(c=45, l=80)+
      xlab(NULL) +
      coord_flip()+
      theme(legend.position="none")
    return(plot)})

result_list4[[1]]

nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

result_list5 <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(nrc_negative) %>%
      dplyr::count(word, sort = TRUE) %>%
      dplyr::top_n(20) %>%
      dplyr::mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill=word)) +
      geom_bar(stat="identity")+
      scale_fill_hue(c=45, l=80)+
      xlab(NULL) +
      coord_flip()+
      theme(legend.position="none")
    return(plot)})

result_list5[[1]]

result_list_contribute <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(get_sentiments("bing")) %>%
      dplyr::count(word, sentiment, sort = TRUE)  %>%
      group_by(sentiment) %>%
      top_n(30) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
    return(plot)})

result_list_contribute[[1]]




##Word Association
result_list10 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      filter(n() >= 15) %>%
      pairwise_cor(word, tweetnumber, sort = TRUE)%>%
      filter(item1 %in% c("support")) %>%
      group_by(item1) %>%
      top_n(15) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation,fill=correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
    return(pair)})

result_list10[[1]]

