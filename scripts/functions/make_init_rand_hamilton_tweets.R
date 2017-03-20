#### initial function to generate tweets

make_init_rand_hamilton_tweets = function(df,num_tweets){
  
  ##### 1 create tweets ----
  
  # # number of tweets 
  # N = 100
  
  
  ### random number generator
  
  line1 = runif(num_tweets, min = 1, max = nrow(df)) %>% round()
  line2 = runif(num_tweets, min = 1, max = nrow(df)) %>% round()
  
  ### add hashtag based off of track name
  
  tracks_hashtag = 
    df$track %>% 
    unique() %>%
    str_replace(" \\(The World Turned Upside Down\\)", "") %>%
    str_replace(" \\(Reprise\\)", " Reprise") %>% 
    str_replace_all(" |'|,", "") %>% 
    str_trim()
  
  tracks_hashtag = paste0("#", tracks_hashtag)
  
  
  # hashtags for each tweet
  hashtag_tweet = paste(tracks_hashtag[df$track_no[line1]], tracks_hashtag[df$track_no[line2]], 
                        "#Hamilton", sep = " ")
  
  
  
  #### compose tweets
  
  tweets = paste( df$lyric[line1], df$lyric[line2] , sep = "\n") %>%
    paste("\n") %>%
    paste(hashtag_tweet)
  
  
  
  
  ## check to see if under 140 characters
  char_over_ind = (nchar(tweets) > 140) %>% which()
  
  
  return(list(tweets, char_over_ind))
  
}