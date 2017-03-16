#### scrape lyrics ####

rm(list = ls())

library(rvest)
library(stringr)
library(magrittr)


### get links to each songs ----

url = "http://www.themusicallyrics.com/h/351-hamilton-the-musical.html"

site = read_html(url)
links = html_nodes(site, css = "tr:nth-of-type(n+3) a") %>%
  html_attr("href") %>%
  unique()

links = paste0("http://www.themusicallyrics.com/", links)

#### loop through links and save each song

setwd("scraped_lyrics/")

for(i in 1:length(links)){
  
  # go to each song url
  site_song = read_html(links[i])
  
  # get lyrics
  song_temp = site_song %>%
    html_nodes(css = "p") %>%
    html_text()
  
  # get title
  
  title_temp = song_temp %>% str_extract("[A-z 0-9]+") %>% str_replace("Lyrics", "") %>% str_trim()
  
  title_temp = paste0(i, "_", title_temp, ".txt") %>% str_replace(" ", "_")
  
  
  write(x = song_temp, file = title_temp)
  
  
}

















