# Quanteda
library(quanteda)
# The first step is to build a corpus from text data
names(df)
df_corp<-corpus(df,text_field="text")
glimpse(df_corp)
#Save out for use with claude
save(df_corp, file="df_corpus.rdata")

#tokenize
df_tokens<-tokens(df_corp, remove_punct=T) %>% 
  tokens_remove(., stopwords("en"))

#Search dictionary
#liberal dictionary
lib_dict<-dictionary(list(
  liberals=c("liberals", "liberal")
))
party_dict<-dictionary(
  list(
    liberals=c("liberals", "liberal"),
    ontario_liberals=c("ontario liberals", "ontario liberal"),
    conservatives=c("conservative", "conservatives"),
    progressive_conservatives=c("progressive conservatives", "PCs", "progressive conservative"), 
    ontario_conservatives=c("ontario conservatives"))
)
party_dict[[1]]
# search party dictionary terms

party_tokens<-tokens_lookup(df_tokens, dictionary=party_dict)
party_tokens
# Last step is a document frequency matrix
# dfm()
# Document frequency matrix has documents in rows and terms in columns
# Cell values are the number of 
dfm(party_tokens)
dfm(party_tokens) %>% colSums()
# leader count

# Look at how terms are used in kwic()

kwic(df_tokens, pattern=lib_dict, window=20)

## Basically we need to find a way to 

leader_dictionaries<-dictionary(list(
  ford=c("ford"),
  crombie=c("crombie"),
  stiles=c("stiles")
))
# textstat.frequency is in quanteda.textstats
#install
#install.packages('quanteda.textstats')
#load
library(quanteda.textstats)
#Start with tokenized 
df_tokens %>% 
  #lookup leader reference
  tokens_lookup(., leader_dictionaries) %>%
  #Convert to dfm
  dfm() %>% 
  #count
  textstat_frequency()
