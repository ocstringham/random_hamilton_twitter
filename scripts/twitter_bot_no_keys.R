##### script for twitter bot #####


rm(list = ls())

library(magrittr)
library(stringr)
library(pipeR)


#### load data and functions ----
setwd("C:/Users/oliver/Google Drive/fun/random_hamilton_twitter")
source("scripts/functions/make_init_rand_hamilton_tweets.R")
source("scripts/functions/replace_tweets_over_140.R")
# source("scripts/functions/get_rand_hamilton_tweets.R")

df = readRDS("dfs/all_df.rds")



#### get tweets ----

# total number of tweets
N = 1

## run initial tweet generator 

list_tweets = make_init_rand_hamilton_tweets(df, N)

# extract data
tweets =  list_tweets %>>% `[`(1) %>% unlist()
char_over_ind = list_tweets %>>% `[`(2) %>% unlist()

## replace tweets over 140 characters
tweets = replace_tweets_over_140(tweets, char_over_ind)

tweets


# test to see if it worked
(tweets %>% nchar() > 140) %>% which()


#-----------------------------------------------------------------------------------------------#

library(twitteR)
library(ROAuth)

# fill in your codes here:
# api_key <- 
# api_secret <-
# access_token <- 
# access_token_secret <- 

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweet(tweets[1])
