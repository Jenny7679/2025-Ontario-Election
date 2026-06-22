#Install packages
devtools::install_github("sjkiss/importProquest", force=T)
library(ImportProquest)
library(here)
library(tidyverse)


files<-list.files(path=here("News/"), recursive=T, pattern="*.txt?")
files<-paste("News/", files, sep="")
news<-files %>% 
  map(., prepare)
length(news)
news %>% 
  #Normally when we use map() it returns a list
  #But we would like to return a nice data frame, so we use map_df
  #extractMeta is my function from ProQuest that extract the metadat from each article
  map_df(., extractMeta)->df
files[[39]]
# Which file is #12?
files[12]


