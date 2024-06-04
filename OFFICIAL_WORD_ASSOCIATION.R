
packages <- c("readxl","tidytext","plyr","dplyr","tidyr","ggplot2","scales",
              "purrr","textdata","wordcloud","reshape2","stringr","igraph",
              "ggraph","widyr","grid","arules","tm","topicmodels")
for(i in packages){
  if(!require(i,character.only = T, quietly = T)){
    install.packages(i)
  }
  library(i, character.only = T, quietly = T)
}

rm(list=ls())

#Set the seed to ensure that we get the same random numbers every time
#The seed could be any number you choose
set.seed(2021)

#Read the data set for June
#AUS
a1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/AUS.tweet.xlsx")
a2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/AUS.tweet.xlsx")
a3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/AUS.tweet.xlsx")
a4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/AUS.tweet.xlsx")
a5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/AUS.tweet.xlsx")
tweets1 <- rbind(a1, a2, a3, a4, a5)
tweets1 <- distinct(tweets1)

#CAN
c1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/CAN.tweet.xlsx")
c2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/CAN.tweet.xlsx")
c3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/CAN.tweet.xlsx")
c4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/CAN.tweet.xlsx")
c5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/CAN.tweet.xlsx")
tweets2 <- rbind(c1, c2, c3, c4, c5)
tweets2 <- distinct(tweets2)

#GB
g1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/GB.tweet.xlsx")
g2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/GB.tweet.xlsx")
g3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/GB.tweet.xlsx")
g4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/GB.tweet.xlsx")
g5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/GB.tweet.xlsx")
tweets3 <- rbind(g1, g2, g3, g4, g5)
tweets3 <- distinct(tweets3)

#USA
u1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/1/USA.tweet.xlsx")
u2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/2/USA.tweet.xlsx")
u3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/3/USA.tweet.xlsx")
u4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/4/USA.tweet.xlsx")
u5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/5/USA.tweet.xlsx")
tweets4 <- rbind(u1, u2, u3, u4, u5)
tweets4 <- distinct(tweets4)
tweets4 <- read_excel("/Users/connornixon/Desktop/twitteR2/CA/FINAL/011.xlsx")

## G3 = data frame being manipulated
CA <- G1
CA <- distinct(CA)
dfList<-list(CA)

#create a function to do all the data clean process
result_list <- llply(dfList, function(x) {
  x<-subset(x,x$`lang`== "en") 
  x$tweet=x$`text` 
  x<-x[,3] 
  x$tweetnumber<-1:length(x$text) 
  return(x)
})

#apply the function to each dataset
twts1<-as.data.frame(result_list[1])
twts2<-as.data.frame(result_list[1])
twts3<-as.data.frame(result_list[1])
twts4<-as.data.frame(result_list[1])

#stop_words is a combination of English stop words from three lexicons, as a data frame. 
data(stop_words)

#customize stop words
custom_stop_words <- bind_rows(
  tibble(word = c("t.co"), 
         lexicon = c("custom")), stop_words)

#store those special symbols in the variable so we can remove them later
remove_reg <- "&amp;|&lt;|&gt;"
ompleterecords <- na.omit(twts1)
twts1 <- ompleterecords

#create a list containing all six data frames
twts1[complete.cases(twts1), ]

dfList2<-list(twts1)
result_list2 <- 
  llply(dfList2, function(x) {
    y <- x %>% 
      #remove special symbols for the values under the tweet variable
      mutate(tweet = str_remove_all(text, remove_reg)) %>%
      #extract every word from every tweet 
      unnest_tokens(word, text, token = "tweets") %>%
      #filter out all stop words
      filter(!word %in% custom_stop_words$word,
             str_detect(word, "[a-z]"))
    return(y)})


tidy1<-as.data.frame(result_list2[1])
tidy2<-as.data.frame(result_list2[1])
tidy3<-as.data.frame(result_list2[1])
tidy4<-as.data.frame(result_list2[1])

#Count the Frequency for Each Word
tidy_week11 <- tidy1 %>%dplyr::count(word, sort = TRUE) 
tidy_week12 <- tidy2 %>%dplyr::count(word, sort = TRUE)
tidy_week21 <- tidy3 %>%dplyr::count(word, sort = TRUE)
tidy_week22 <- tidy4 %>%dplyr::count(word, sort = TRUE)

#Remove all non-english tokens
tidy1_english <- tidy_week11[which(!grepl("[^\x01-\x7F]+", tidy_week11$word)),]
tidy2_english <- tidy_week12[which(!grepl("[^\x01-\x7F]+", tidy_week12$word)),]
tidy3_english <- tidy_week21[which(!grepl("[^\x01-\x7F]+", tidy_week21$word)),]
tidy4_english <- tidy_week22[which(!grepl("[^\x01-\x7F]+", tidy_week22$word)),]

dfList3<-list(tidy1_english)
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
result_list3[[2]]
result_list3[[3]]

#create a list containing all six data frames (ADD tidy4)
dfList4<-list(tidy1)
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



#visualize using Word clouds
result_list_wordclouds <- 
  llply(dfList4, function(x) {
    plot <- x %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("gray20", "gray80"),max.words = 50)
    return(plot)})

#Find the most common positive words using nrc lexicon
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

#Find the most common negative words using nrc lexicon
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

#Create a visualization about how much each word contributed to each sentiment.
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

dfList2 <- list(twts1)
result_list10 <- 
  llply(dfList2, function(x) {
    pair <- x %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% custom_stop_words$word)%>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, tweetnumber, sort = TRUE)%>%
      filter(item1 %in% c("environment")) %>%
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

"scum", "cloudy", "fall", "hard", "bad"
"love", "beautiful", "happy", "fun", "fresh"

write_xlsx(twts1,"/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/WordAssociation/USA.tweet_updated.xlsx")

twts1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/WordAssociation/USA.tweet_updated.xlsx")


