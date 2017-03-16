

library(sentimentr)
library(plyr)
library(magrittr)


df_all = readRDS("C:/Users/oliver/Google Drive/fun/random_hamilton_twitter/dfs/all_df.rds")
df = readRDS("C:/Users/oliver/Google Drive/fun/random_hamilton_twitter/dfs/Hamilton_df.rds")


df = readRDS("C:/Users/oliver/Google Drive/fun/random_hamilton_twitter/dfs/Eliza_df.rds")


#add line number
df = seq.int(nrow(df))

#sentiment by line
test = sentiment(df$lyric)
# #remove zeros
# test$sentiment[ (test$sentiment == 0) %>% which() ] = NA

test = na.omit(test)
avg_sent = ddply(test, .(element_id), summarize, avg_sentiment=mean(sentiment) )


#sentiment by song
test2 = merge(x = df, y = avg_sent, by.x = "line", by.y = "element_id", all.y = T)

song_sent = ddply(test2, .(track_no), summarize, avg_sentiment=mean(avg_sentiment) )


# plot and lm
plot(test2$avg_sentiment ~ test2$track_no )
plot(song_sent$avg_sentiment ~ song_sent$track_no )


lm(song_sent$avg_sentiment ~ song_sent$track_no  ) %>% summary()
lm(test2$avg_sentiment ~ test2$track_no  ) %>% summary()






temp = sentiment_by(df$lyric, by = df$track_no) %>% na.omit()
  
plot(temp$ave_sentiment)
lm(temp$ave_sentiment ~ temp$track_no) %>% summary()
