
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
library("stringr") 
library("dplyr")
library("lubridate")
library("dplyr")
library("ggExtra")

final_tweets_can <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/CA/CAN.tweet.xlsx")
final_tweets1 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group1.tweets.xlsx")
final_tweets2 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group2.tweets.xlsx")
final_tweets3 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group3.tweets.xlsx")
final_tweets4 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group4.tweets.xlsx")
final_tweets5 <- read_excel("/Users/connornixon/Desktop/academictwitteR/data/clean.data/FINAL/GROUPS/group5.tweets.xlsx")

Chi_square <- final_tweets5[, c("pre_post", "pos.neu.neg")]

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