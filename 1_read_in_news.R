#Install packages
library(ImportProquest)
library(here)
library(tidyverse)

#devtool,s::install_github("sjkiss/importProquest", force=T)
files<-list.files(path=here("News/"), recursive=T, pattern="*.txt?")
files<-paste("News/", files, sep="")
news<-files %>% 
  map(., prepare)

news %>% 
  #Normally when we use map() it returns a list
  #But we would like to return a nice data frame, so we use map_df
  #extractMeta is my function from ProQuest that extract the metadat from each article
  map_df(., extractMeta)->df


