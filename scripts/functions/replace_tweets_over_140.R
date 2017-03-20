#### function to replace ones over 140 until all are under 140


replace_tweets_over_140 = function(tweets, char_over_ind){

  while(length(char_over_ind) > 0){
    
    num_replace = length(char_over_ind)
    
    # get new tweets
    list_tweets2 = make_init_rand_hamilton_tweets(df, num_replace)
    tweets2 =  list_tweets2 %>>% `[`(1) %>% unlist()
    
    #replace tweets in original
    d = 1
    for (j in 1:num_replace) {
      tweets[char_over_ind[j]] = tweets2[d]
      d = d + 1
    }
    
    # update variable
    char_over_ind = (tweets %>% nchar() > 140) %>% which()
      
  }
  
  return(tweets)
}