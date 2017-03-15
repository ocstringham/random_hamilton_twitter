
#### random hamilton twitter bot ####

library(readr)
library(stringr)
library(magrittr)
library(plyr)


# if line ends in colon:, then that's who said it

# want a df of 1. line, 2. who said it, 3. song


# test on one song

song1 = read_file("lyrics/01_Alexander_Hamilton.txt")
# 
# song1 = strsplit(song1, "\n") %>% unlist()
# song1 = str_replace(song, "\r", "")
# # add blank line at end to make loop work
# song1[(length(song)+1)] = ""

#### define function to do everything: data cleaning and storing ####

hamilton_song_clean = function(song)
{

  # split by each lyric line
  song = strsplit(song, "\n") %>% unlist()
  
  #replace /r with blank
  song = str_replace(song, "\r", "")
  
  #remove leading trailing spaces
  song = str_trim(song)
  
  
  d=1
  singer = list()
  lines_sang = list()
  
  ### loop through to looking for each set of lyrics 
  for(i in 1:length(song)){
    
    # if ends in colon, means start of lyric set
    if(str_detect(song[i], ":$")){
      
      # inits
      end_phrase = NULL
      start_phrase = NULL
      singer_temp = NULL
      
      # take that line, this is who sings it
      singer_temp = song[i] %>% str_replace(":", "")
      
      # record start phrase
      start_phrase = i + 1
      
      #loop through to find a blank line - indicates end of phrase
      for(j in start_phrase:length(song)){
        
        if(str_detect(song[j], ":$")){ #if dectect next colon - means start of next set
          # record line that has blank
          end_phrase = j-1
          
          # once found end of phrase, break loop
          break
          
        }else if(j == length(song)){ # this catches the last set, which doesn't have a proceeding colon
          end_phrase = j
        }
      } # end of loop for end_phrase
      
      # populate intitials
      
      lines_sang[[d]] = song[start_phrase:end_phrase]
      singer[[d]] = rep(singer_temp, times = length(lines_sang[[d]]) )
      
      d=d+1
      
    } # end of loop for one phrase

  } # end of all


# compile into df

# get song title
title = song[1] %>% str_replace("####", "") %>% str_replace("####", "") %>% str_trim()
title = rep(title, times = length(singer %>% unlist()))

# cbind
df = cbind.data.frame(lyric = lines_sang %>% unlist(), 
                      singer = singer %>% unlist(), 
                      track = title,
                      stringsAsFactors = FALSE)

# #remove blank rows
# df = df[ (df[1,] != "") , ]


return(df)

} #end function


test = hamilton_song_clean(song1)



#### Loop through all songs, and make one giant df ####

df_all = list()

setwd("lyrics/")

# get number of songs
num_songs = list.files() %>% length()

for(i in 1:num_songs){
  song_temp = NULL
  song_temp = read_file(list.files()[i])
  
  df_all[[i]] = hamilton_song_clean(song_temp)
}




final_df = rbind.fill(df_all)
final_df = subset(final_df, lyric != "")


sample(final_df$lyric, 2)
