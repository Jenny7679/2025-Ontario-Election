# Quanteda
library(quanteda)
library(quanteda.textstats)

# ---- 1. Build corpus ----
df_corp <- corpus(df, text_field = "text")
save(df_corp, file = "df_corpus.rdata")

# ---- 2. Tokenize ----
df_tokens <- tokens(df_corp, remove_punct = TRUE) %>%
  tokens_remove(stopwords("en"))

# ---- 3. Filter to Ontario-election stories ----
# Ontario-provincial signals
on_signal <- dictionary(list(
  on = c("crombie", "stiles", "schreiner", "doug ford",
         "ontario pc*", "ontario liberal*", "ontario ndp", "ontario green*",
         "ontario election", "ontario premier")
))

# Federal signals — to spot stories really about Ottawa
fed_signal <- dictionary(list(
  fed = c("carney", "trudeau", "poilievre", "freeland", "gould", "baylis",
          "dhalla", "jagmeet", "singh", "blanchet", "prime minister")
))

on_count  <- df_tokens %>% tokens_lookup(on_signal)  %>% dfm() %>% .[, "on"]  %>% as.numeric()
fed_count <- df_tokens %>% tokens_lookup(fed_signal) %>% dfm() %>% .[, "fed"] %>% as.numeric()

# Keep: strong Ontario signal AND not dominated by federal mentions
keep2 <- on_count >= 2 & on_count > fed_count

sum(keep2)                       # how many docs survived
df_tokens_on <- df_tokens[keep2]
df_corp_on   <- df_corp[keep2]

# ---- 4. Verify the filter ----
ndoc(df_corp_on)
head(df_corp_on$titles, 25)      # check stragglers are gone

# ---- 5. Dictionaries ----
party_dict <- dictionary(list(
  liberals                  = c("liberals", "liberal"),
  ontario_liberals          = c("ontario liberals", "ontario liberal"),
  conservatives             = c("conservative", "conservatives"),
  progressive_conservatives = c("progressive conservatives", "PCs", "progressive conservative"),
  ontario_conservatives     = c("ontario conservatives")
))

leader_dictionaries <- dictionary(list(
  ford      = c("doug ford", "ford"),
  crombie   = c("crombie"),
  stiles    = c("stiles"),
  schreiner = c("schreiner")
))

# ---- 6. Party counts (Ontario-only) ----
df_tokens_on %>%
  tokens_lookup(party_dict, nested_scope = "dictionary") %>%
  dfm() %>%
  colSums()

# ---- 7. Leader counts (Ontario-only) ----
df_tokens_on %>%
  tokens_lookup(leader_dictionaries) %>%
  dfm() %>%
  textstat_frequency()

# ---- 8. Optional: confirm "ford" mentions are Doug Ford ----
kwic(df_tokens_on, "ford", window = 8)



