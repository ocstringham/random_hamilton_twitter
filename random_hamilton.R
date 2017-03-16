
#### random hamilton twitter bot ####

rm(list=ls())


library(readr)
library(stringr)
library(stringi)
library(magrittr)
library(plyr)

#-------------------------------------------------------------------------------------------#

#### define function to do everything: data cleaning and storing for 1 song####

hamilton_song_clean = function(song, track_no)
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

# get track number
track_num = rep(track_no, times = length(singer %>% unlist()))


# cbind
df = cbind.data.frame(lyric = lines_sang %>% unlist(), 
                      singer = singer %>% unlist(), 
                      track = title,
                      track_no = track_num,
                      stringsAsFactors = FALSE)

# #remove blank rows
# df = df[ (df[1,] != "") , ]


return(df)

} #end function


test = hamilton_song_clean(song1, 1)


#-------------------------------------------------------------------------------------------#

#### Loop through all songs, and make one giant df ####

df_all = list()

setwd("lyrics/")

# get number of songs
num_songs = list.files() %>% length()

for(i in 1:num_songs){
  song_temp = NULL
  song_temp = read_file(list.files()[i])
  
  df_all[[i]] = hamilton_song_clean(song_temp, i)
}

final_df = rbind.fill(df_all)
final_df = subset(final_df, lyric != "")


#-------------------------------------------------------------------------------------------#

#### Standardize Singer names #### oh boy...

# it's ok if has multiple, just needs to be uniform throughout

# Hamilton
# Eliza
# Angelica
# Peggy
# Mulligan
# Lafayette
# Laurens
# Burr
# Washington
# Jefferson
# Madison
# Maria Reynolds
# James Reynolds
# Charles Lee
# Seabury
# Phillip
# Dolly
# Martha
# George Eaker
# Ensemble


### continue to data clean here. see if replacing company with ensemble makes sense.

final_df$lower_singer = final_df$singer %>% tolower()

### replace
final_df$lower_singer_replace = 
final_df$lower_singer %>%
  str_replace_all("aaron burr", "burr") %>%
  str_replace("john laurens|laurence", "laurens") %>%
  str_replace("philip", "phillip") %>%
  str_replace("thomas jefferson", "jefferson") %>%
  str_replace("james madison", "madison") %>%
  str_replace("george washington|washingon|washinton", "washington") %>%
  str_replace("alexander hamilton", "hamilton") %>%
  str_replace("angelica schuyler", "angelica") %>%
  str_replace("seabury", "samuel seabury") %>%
  str_replace("maria$", "maria reynolds") %>%
  str_replace("james$", "james reynolds") %>%
  str_replace("^george", "george eaker") %>%
  str_replace("lee", "charles lee") %>%
  str_replace_all(paste("company","company-women","company-men","ensemble-men",
                    "full company","men-ensemble", "women-ensemble", "ensemble-women",
                    "ensemble women","ensemble-man", "full ensemble", "ensemble men", 
                    "various ensemble", "male company" , 
                    sep = "|"),
                    "ensemble")   %>%
  str_replace("ensemble-women", "ensemble") %>%
  str_replace("ensemble-men", "ensemble")


# replace all ands with slash
final_df$lower_singer_replace = 
final_df$lower_singer_replace %>%
  str_replace_all(", *", "/") %>%
  str_replace(" *AND ", "/") %>%
  str_replace(" *and ", "/") %>%
  str_replace(" *& ", "/") %>%
  str_replace("//*/* ", "/")
  
  
#return to upper case
final_df$singer_clean = final_df$lower_singer_replace %>% stri_trans_general(id = "Title")
  


# final_df$singer_clean %>%  
#   unique() %>%
#   #### next string split by / and see unique
#   str_split("/") %>%
#   unlist() %>%
#   unique()



### subset
all_df = final_df[  , c(1,7,3,4)]

## save
saveRDS(hamilton_df, "C:/Users/oliver/Google Drive/fun/random_hamilton_twitter//dfs/all_df.rds")



#-------------------------------------------------------------------------------------------------------#

#### next create df for each character to do individual analysis ####

# want: lyric, indiv singer, all singers, track, track no


## get unique singers
singers_unique = 
all_df$singer_clean %>%  
  unique() %>%
  str_split("/") %>%
  unlist() %>%
  unique() 

# remove blank
singers_unique = singers_unique[ -14]


#### look through each, save in list ##

singers_df = list()


for(i in 1:length(singers_unique)){
  
  singers_df[[i]] = all_df[ all_df$singer_clean %>% str_detect(singers_unique[i]) %>% which() , ]
  singers_df[[i]]$indiv_singer = singers_unique[i]
  
  #save
  savenametemp = paste0("C:/Users/oliver/Google Drive/fun/random_hamilton_twitter/dfs/",
                        singers_unique[i] %>% str_replace(" ", "_"), "_df.rds")
  dftemp = singers_df[[i]] %>% rbind.fill()
  saveRDS(dftemp, savenametemp)
  
}


test = singers_df[[5]] %>% rbind.fill()



