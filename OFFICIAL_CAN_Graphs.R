
install.packages("ggExtra")

library(ggExtra)

### WE <3 CODE WHEN IT WORKS
#CANADA ARTICLE CODE GRAPHS
g1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ca_group1.tweet.xlsx")
g2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ca_group2.tweet.xlsx")
g3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ca_group3.tweet.xlsx")
g4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ca_group4.tweet.xlsx")
g5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/COMBINED/ca_group5.tweet.xlsx")

CA <- rbind(g1, g2, g3, g4, g5)
Canada <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/CAN.tweet.xlsx")

#Overall sentiment
p2 <- ggplot(CA, aes(x=created_at, y=sentiment)) +
  geom_point(size = .25, alpha = .5)+
  geom_smooth(se= FALSE, model= "gam", level = .95, col= "white")+
  labs(x='Date (years)', y='Observation Sentiment')+
  theme_classic()+
  scale_fill_grey() +
  scale_colour_grey() +
  theme_gray()

p1 <-ggplot(CA, aes(x=created_at, y=sentiment, lty=group)) +
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


ggMarginal(p2, type="histogram")
p1

t.test(g1$sentiment[g1$pre_post=="Pre pandemic"], g1$sentiment[g1$pre_post=="Post pandemic"])
t.test(g2$sentiment[g2$pre_post=="Pre pandemic"], g2$sentiment[g2$pre_post=="Post pandemic"])
t.test(g3$sentiment[g3$pre_post=="Pre pandemic"], g3$sentiment[g3$pre_post=="Post pandemic"])
t.test(g4$sentiment[g4$pre_post=="Pre pandemic"], g4$sentiment[g4$pre_post=="Post pandemic"])
t.test(g5$sentiment[g5$pre_post=="Pre pandemic"], g5$sentiment[g5$pre_post=="Post pandemic"])

t.test(CA$sentiment[CA$pre_post=="Pre pandemic"], CA$sentiment[CA$pre_post=="Post pandemic"])


Chi_square <- Canada[, c("pre_post", "pos.neu.neg")]

#table(Chi_square$final_tweets.pos.neu.neg, Chi_square$final_tweets.pre_post)
table <- table(Chi_square$pre_post, Chi_square$pos.neu.neg)
table

ggplot(Chi_square) +
  aes(x = pre_post, fill = pos.neu.neg) +
  geom_bar()

test <- chisq.test(table(Chi_square$pre_post, Chi_square$pos.neu.neg))
test
print(Chi_square)
Chi_2 <- table(Chi_square)
prop.table(Chi_2,1)

##### STEP xx: 2 and 2 Sample T-Tests as well as the Stdev of the dataset.
t.test(final_tweets$sentiment)
boxplot(g1$sentiment~g1$pre_post)
t.test(g1$sentiment~g1$pre_post, mu=0, alt="two.sided", conf=.95, var.eq=T, paired=F)
t.test(final_tweets$sentiment[final_tweets$pre_post=="Pre pandemic"], final_tweets$sentiment[final_tweets$pre_post=="Post pandemic"])



#Frequency Chart/dist.
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

#Word Association
#positive
dfList2 <- list(twts1)
result_list11 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, tweetnumber, sort = TRUE)%>%
      filter(item1 %in% c("tree")) %>%
      group_by(item1) %>%
      top_n(15) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation,fill=correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
    return(pair)})
result_list11[[1]]

#negative
dfList2 <- list(twts1)
result_list12 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, tweetnumber, sort = TRUE)%>%
      filter(item1 %in% c("scum")) %>%
      group_by(item1) %>%
      top_n(15) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation,fill=correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
    return(pair)})
result_list12[[1]]


#Linear Regession stat analysis

lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = comb)
summary(lmoutput)


CAN1 <- rbind(G1, G2)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN1)
summary(lmoutput)

CAN2 <- rbind(G1, G3)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN2)
summary(lmoutput)

CAN3 <- rbind(G1, G4)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN3)
summary(lmoutput)

CAN4 <- rbind(G1, G5)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN4)
summary(lmoutput)

CAN5 <- rbind(G2, G3)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN5)
summary(lmoutput)

CAN6 <- rbind(G2, G4)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN6)
summary(lmoutput)

CAN7 <- rbind(G2, G5)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN7)
summary(lmoutput)

CAN8 <- rbind(G3, G4)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN8)
summary(lmoutput)

CAN9 <- rbind(G3, G5)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN9)
summary(lmoutput)

CAN10 <- rbind(G4, G5)
lmoutput <- lm(formula = sentiment ~ group + created_at + group:created_at, data = CAN10)
summary(lmoutput)

