##### Sort tweets ito groups & save
#Group 1
CA <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/CAN.tweet.xlsx")
US <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/USA.tweet.xlsx")
AU <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/AUS.tweet.xlsx")
GB <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/GB.tweet.xlsx")

group1_tweets <- rbind(CA, US, AU, GB)
group1_tweets$group <- "Green Infrastructure"
group1_tweets <- unique(group1_tweets)
write_xlsx(group1_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group1.tweet.xlsx")

#Group 2
CA2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/CAN.tweet.xlsx")
US2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/USA.tweet.xlsx")
AU2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/AUS.tweet.xlsx")
GB2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/GB.tweet.xlsx")

group2_tweets <- rbind(CA2, US2, AU2, GB2)
group2_tweets$group <- "Natural Areas"
group2_tweets <- unique(group2_tweets)
write_xlsx(group2_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group2.tweet.xlsx")

#Group 3
CA3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/CAN.tweet.xlsx")
US3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/USA.tweet.xlsx")
AU3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/AUS.tweet.xlsx")
GB3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/GB.tweet.xlsx")

group3_tweets <- rbind(CA3, US3, AU3, GB3)
group3_tweets$group <- "Recreation"
group3_tweets <- unique(group3_tweets)
write_xlsx(group3_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group3.tweet.xlsx")

#Group 4
CA4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/CAN.tweet.xlsx")
US4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/USA.tweet.xlsx")
AU4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/AUS.tweet.xlsx")
GB4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/GB.tweet.xlsx")

group4_tweets <- rbind(CA4, US4, AU4, GB4)
group4_tweets$group <- "Waterways"
write_xlsx(group4_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group4.tweet.xlsx")

#Group 5
CA5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/CAN.tweet.xlsx")
US5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/USA.tweet.xlsx")
AU5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/AUS.tweet.xlsx")
GB5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/GB.tweet.xlsx")

group5_tweets <- rbind(CA5, US5, AU5, GB5)
group5_tweets$group <- "Agriculture"
write_xlsx(group5_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group5.tweet.xlsx")

#ALL

ALL_tweets <- rbind(group1_tweets, group2_tweets, group3_tweets, group4_tweets, group5_tweets)
write_xlsx(ALL_tweets,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ALL.tweets.xlsx")

#CLEAR


G1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group1.tweet.xlsx")
G2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group2.tweet.xlsx")
G3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group3.tweet.xlsx")
G4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group4.tweet.xlsx")
G5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/group5.tweet.xlsx")
ALL_tweets <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ALL.tweets.xlsx")

##### PLOTS
#Plot 1
#GROUP Means - ALL
ggplot(ALL_tweets, aes(x=created_at, y=sentiment, lty=group)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_fill_grey() +
  scale_colour_grey()+
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                           size=0.25, linetype="solid", 
                                           colour ="black"))


#Group 1
ggplot(G1, aes(x=created_at, y=sentiment, lty=Country)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_linetype_manual(values=c(1:6))+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_gray()+
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
#Group 2
ggplot(G2, aes(x=created_at, y=sentiment, lty=Country)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_linetype_manual(values=c(1:6))+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_gray()+
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
#Group 3
ggplot(G3, aes(x=created_at, y=sentiment, lty=Country)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_linetype_manual(values=c(1:6))+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
#Group 4
ggplot(G4, aes(x=created_at, y=sentiment, lty=Country)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_linetype_manual(values=c(1:6))+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
#Group 5
ggplot(G5, aes(x=created_at, y=sentiment, lty=Country)) +
  geom_smooth(se= FALSE, method= "gam", col='black')+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_linetype_manual(values=c(1:6))+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_grey()+
  theme(legend.position = 'bottom', legend.direction = "horizontal", legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))

#Linear Rergression Stat Analysis

lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = CA1)
summary(lmoutput)


INTL1 <- rbind(AU2, CA2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL1)
summary(lmoutput)

INTL2 <- rbind(AU2, GB2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL2)
summary(lmoutput)

INTL3 <- rbind(AU2, US2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL3)
summary(lmoutput)

INTL4 <- rbind(CA2, GB2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL4)
summary(lmoutput)

INTL5 <- rbind(CA2, US2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL5)
summary(lmoutput)

INTL6 <- rbind(GB2, US2)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL6)
summary(lmoutput)

INTL7 <- rbind(AU3, CA3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL7)
summary(lmoutput)

INTL8 <- rbind(AU3, GB3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL8)
summary(lmoutput)

INTL9 <- rbind(AU3, US3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL9)
summary(lmoutput)

INTL10 <- rbind(CA3, GB3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL10)
summary(lmoutput)

INTL11 <- rbind(CA3, US3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL11)
summary(lmoutput)

INTL12 <- rbind(GB3, US3)
lmoutput <- lm(formula = sentiment ~ Country + created_at + Country:created_at, data = INTL12)
summary(lmoutput)


#Word Frequency G2/3

result_list_contribute <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(get_sentiments("nrc")) %>%
      dplyr::count(word, sentiment, sort = TRUE)  %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()+
      scale_fill_grey() +
      scale_colour_grey() +
      theme_gray()
    return(plot)})

result_list_contribute[[1]]



#####
#Testing

ggplot(CA, aes(x=created_at, y=sentiment)) +
  geom_point(size=.25, alpha = .5)+
  geom_smooth(se= FALSE, method= "gam", level = .95, span = 0.3, col= "orange")+
  labs(x='Date (years)', y='Tweet Sentiment',
       title = 'Sentiment in Canadian Tweets Over Time',
       subtitle = 'search query = city park , method = bing')

ggplot(data = group1_tweets, aes(created_at , sentiment, color = Country)) +
  geom_smooth(size = 1, se= FALSE) +
  geom_point(size = .25) + 
  labs(title = "Comparing Sentiment Scores between nations",
       subtitle = "query used = city lake , method = syuzhet",
       y = "Sentiment Scores", x = "Year")


