#Install packages
#devtools::install_github("sjkiss/importProquest", force=T)
library(ImportProquest)
library(here)
library(tidyverse)


files<-list.files(path=here("News/"), recursive=T, pattern="*.txt?")
files<-paste("News/", files, sep="")
news<-files %>% 
  map(., prepare)
length(news)
extractMeta
news %>% 
  #Normally when we use map() it returns a list
  #But we would like to return a nice data frame, so we use map_df
  #extractMeta is my function from ProQuest that extract the metadat from each article
  map_df(., extractMeta)->df
#What is df?
# df is the data frame of all variables imported from the text files you imported. My function 
# prepare and extractMeta takes all the information in those text files and stores it in a dataframe
# it has a very nice structure
glimpse(df)
#16000 rows and 6 variables
# the variable text stores the full text

