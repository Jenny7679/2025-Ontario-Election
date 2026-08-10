#### Key ATM analysis ####
library(tidyverse)
library(quanteda)
library(keyATM)

dir.create("Plots", showWarnings = FALSE)

## keyATM_docs stores documents in $W_raw, not $W.
n_keyatm_docs <- function(x) if (!is.null(x$W_raw)) length(x$W_raw) else length(x)


## ---- 1. Compound multi-word keyword phrases -------------------------------
## Multi-word keywords can never match a unigram dfm. Compound them first.
PHRASES <- c("housing affordability", "health care", "primary care",
             "post secondary", "buy canadian", "buy american")

toks <- tokens_compound(df_tokens_on, phrase(PHRASES), concatenator = "_")


## ---- 2. Covariates: clean origin ONCE, properly ----------------------------
## Canonical key = lowercase, letters only, leading "the" dropped. This absorbs
## OCR spaces inside words ("Natio nal Post", "The Hamilton Spec tator") that
## str_squish and hand-typed replace() rules can never keep up with.
CANON <- c(
  globeandmail         = "The Globe and Mail",
  ottawacitizen        = "The Ottawa Citizen",
  ottawasun            = "The Ottawa Sun",
  torontostar          = "Toronto Star",
  torontosun           = "The Toronto Sun",
  windsorstar          = "The Windsor Star",
  hamiltonspectator    = "The Hamilton Spectator",
  spectator            = "The Hamilton Spectator",   # Spectator == Hamilton Spectator
  nationalpost         = "National Post",
  saultstar            = "Sault Star",
  sudburystar          = "Sudbury Star",
  kingstonwhigstandard = "Kingston Whig-Standard",
  standard             = "The Standard"              # St. Catharines -- separate paper
)

Vars <- docvars(toks) %>%
  mutate(
    raw_origin = str_squish(str_extract(origin, "^[^;(]+")),
    key    = str_remove(str_to_lower(str_remove_all(raw_origin, "[^A-Za-z]")), "^the"),
    origin = unname(CANON[key]),
    origin = replace(origin, is.na(origin), "Other"),
    ## Set the reference level EXPLICITLY -- otherwise it's whichever level
    ## your locale happens to sort first, and by_strata_DocTopic breaks.
    origin = relevel(factor(origin), ref = "Other")
  )

## Anything unmapped would silently become "Other" -- surface it instead.
unmapped <- setdiff(unique(Vars$key[nzchar(Vars$key)]), names(CANON))
if (length(unmapped)) message("Unmapped origin keys -> 'Other': ",
                              paste(sort(unmapped), collapse = ", "))

print(table(Vars$origin, useNA = "ifany"))


## ---- 3. Keywords ----------------------------------------------------------
keywords <- list(
  housing = c("housing", "rent", "rents", "renter", "landlord",
              "tenant", "mortgage", "homeless", "condo", "nimby",  # lowercase: dfm() lowercases
              "housing_affordability", "homebuilding", "homebuilder"),
  
  tariffs_trade = c("tariff", "tariffs", "trade", "protectionist",
                    "countervail", "buy_canadian", "buy_american"), # was c("buy","buy")
  
  taxes = c("tax", "taxes", "taxation", "taxpayer", "hst"),
  
  health_care = c("health_care", "healthcare", "hospital", "doctor",
                  "physician", "primary_care", "nurse", "nursing",
                  "ohip", "surgery"),
  
  education = c("education", "school", "teacher", "classroom",
                "curriculum", "kindergarten"),
  
  post_secondary = c("post-secondary", "post_secondary", "postsecondary",
                     "university", "universities", "college", "colleges",
                     "tuition", "professor", "opseu", "campus", "osap", "student"),
  
  crime = c("crime", "crimes", "criminal", "police", "policing", "theft",
            "shooting", "homicide", "murder", "violence", "bail",
            "carjacking", "gang", "violent", "prison")
)


## ---- 4. Build dfm, drop empty docs, keep Vars aligned ---------------------
dfm_all <- dfm(toks)

keep_nonempty <- ntoken(dfm_all) > 0
if (any(!keep_nonempty)) message("Dropping ", sum(!keep_nonempty), " empty documents.")
dfm_all <- dfm_all[keep_nonempty, ]
Vars    <- Vars[keep_nonempty, ]

## Drop keywords absent from the vocabulary -- keyATM chokes on zero-count
## keywords, and this tells you which phrases failed to compound.
vocab   <- featnames(dfm_all)
dropped <- unlist(lapply(keywords, setdiff, vocab))
if (length(dropped)) message("Keywords not in vocabulary (dropped): ",
                             paste(unique(dropped), collapse = ", "))
keywords <- lapply(keywords, intersect, vocab)

keyatm_dfm <- keyATM_read(texts = dfm_all)
stopifnot(n_keyatm_docs(keyatm_dfm) == nrow(Vars))   # the check that actually matters

key_viz <- visualize_keywords(docs = keyatm_dfm, keywords = keywords)
save_fig(key_viz, "Plots/keyword_freq.png", width = 8, height = 5)


## ---- 5. Covariate model ---------------------------------------------------
Vars2 <- Vars %>% select(origin)

newssource <- keyATM(
  docs = keyatm_dfm,
  no_keyword_topics = 5,
  keywords = keywords,
  model = "covariates",
  model_settings = list(covariates_data = Vars2, covariates_formula = ~ origin),
  options = list(seed = 1998)
)

Topic_frequency <- plot_topicprop(newssource, show_topic = 1:12)
save_fig(Topic_frequency, "Plots/Topic_frequency.png", width = 8, height = 5)  # not ggsave

covariates_info(newssource)
newsource_keyatm <- top_words(newssource, n = 100)


## ---- 6. Per-newspaper strata ----------------------------------------------
## Derive the paper list FROM the fitted model so a by_var can never be missing
## and no level can silently go unhandled.
cov_cols   <- colnames(covariates_get(newssource))
paper_cols <- grep("^origin", cov_cols, value = TRUE)
NEWSPAPERS <- sub("^origin", "", paper_cols)

Newspaper_df <- map_dfr(seq_along(NEWSPAPERS), function(i) {
  temp <- by_strata_DocTopic(newssource,
                             by_var = paper_cols[i],
                             labels = c("other-paper", NEWSPAPERS[i]))
  summary(temp)[[NEWSPAPERS[i]]]
})

## Zero ALL origin dummies, not a hand-typed subset -- otherwise leftover
## papers leak into the baseline prediction.
new_data <- covariates_get(newssource)
new_data[, paper_cols] <- 0
pred <- predict(newssource, new_data, label = "Other (baseline)")
Newspaper_df <- bind_rows(Newspaper_df, pred)


## ---- 7. Plot --------------------------------------------------------------
TOPIC_ORDER <- c("Housing*", "Tariffs/Trade*", "Taxes*", "Health Care*",
                 "Education*", "Post-Secondary*", "Crime*", "Transportation",
                 "Ontario Place", "Scandals", "Misc", "Leaders")

PREFERRED <- c("National Post", "The Globe and Mail", "Toronto Star",
               "The Toronto Sun", "The Ottawa Citizen", "The Ottawa Sun",
               "Sault Star", "The Hamilton Spectator", "Sudbury Star",
               "The Windsor Star", "Kingston Whig-Standard", "The Standard")

present     <- unique(Newspaper_df$label)
LABEL_ORDER <- c(intersect(PREFERRED, present),
                 setdiff(present, c(PREFERRED, "Other (baseline)")),
                 "Other (baseline)")

topic_by_newspaper <- Newspaper_df %>%
  mutate(
    Topic = case_match(Topic,
                       "1_housing"        ~ "Housing*",
                       "2_tariffs_trade"  ~ "Tariffs/Trade*",
                       "3_taxes"          ~ "Taxes*",
                       "4_health_care"    ~ "Health Care*",
                       "5_education"      ~ "Education*",
                       "6_post_secondary" ~ "Post-Secondary*",
                       "7_crime"          ~ "Crime*",
                       "Other_1"          ~ "Leaders",
                       "Other_2"          ~ "Misc",
                       "Other_3"          ~ "Scandals",
                       "Other_4"          ~ "Transportation",
                       "Other_5"          ~ "Ontario Place",
                       .default = Topic),
    Topic = factor(Topic, levels = rev(TOPIC_ORDER)),
    label = factor(label, levels = LABEL_ORDER)
  )

stopifnot(!any(is.na(topic_by_newspaper$Topic)),
          !any(is.na(topic_by_newspaper$label)))

topic_by_newspaper <- topic_by_newspaper %>%
  ggplot(aes(x = Point, xmin = Lower, xmax = Upper, y = Topic)) +
  geom_point() +
  geom_linerange() +
  facet_wrap(~label) +
  labs(x = expression(paste("Mean of ", theta))) +
  theme_bw()


ggsave("Plots/topic_by_newspaper.png", topic_by_newspaper, width = 8, height = 8)
topic_by_newspaper   # <- add this: prints to the plot pane


## ---- 8. Derive week from `dates`, clean, sort, rebuild --------------------
DATE_RANGE <- as.Date(c("2023-06-01", "2025-12-31"))   # plausible window -- adjust

## Hand-patches for OCR-corrupted years, keyed on the RAW string.
DATE_FIXES <- c(
  "0002-06-03" = "2024-06-03",
  "0020-02-24" = "2025-02-24",
  "0020-05-11" = "2024-05-11",
  "0202-01-04" = "2024-01-04"
)

Vars_w <- Vars %>%
  mutate(
    date_chr = as.character(dates),
    date_chr = coalesce(unname(DATE_FIXES[date_chr]), date_chr),
    date = as.Date(lubridate::parse_date_time(
      date_chr,
      orders = c("Ymd", "dmY", "mdY", "BdY", "dBY", "Ymd HMS"),
      quiet  = TRUE))
  )

## Report both failure modes with their RAW strings so you can extend
## DATE_FIXES rather than silently losing documents.
bad <- is.na(Vars_w$date)
if (any(bad)) message("Unparseable dates (", sum(bad), " docs): ",
                      paste(head(sort(unique(Vars_w$date_chr[bad])), 20), collapse = ", "))

odd <- !bad & (Vars_w$date < DATE_RANGE[1] | Vars_w$date > DATE_RANGE[2])
if (any(odd)) message("Out-of-range dates (", sum(odd), " docs): ",
                      paste(sort(unique(Vars_w$date_chr[odd])), collapse = ", "))

keep_weeks <- !bad & !odd
Vars_clean <- Vars_w[keep_weeks, ]
toks_clean <- toks[keep_nonempty][keep_weeks]

## Now derive the week bins.
Vars_clean <- Vars_clean %>%
  mutate(week = lubridate::floor_date(date, unit = "week", week_start = 1))  # Monday

## keyATM's dynamic model requires time_index to change by 0 or 1 between
## CONSECUTIVE documents, so sort chronologically before ranking.
doc_order  <- order(Vars_clean$week)
Vars_clean <- Vars_clean[doc_order, ]
toks_clean <- toks_clean[doc_order]

dfm_clean  <- dfm(toks_clean)
keep2      <- ntoken(dfm_clean) > 0
dfm_clean  <- dfm_clean[keep2, ]
Vars_clean <- Vars_clean[keep2, ]

keyatm_dfm_clean <- keyATM_read(texts = dfm_clean)
keywords_dyn     <- lapply(keywords, intersect, featnames(dfm_clean))

Vars3 <- Vars_clean %>% mutate(week_num = dense_rank(week)) %>% select(week_num)

stopifnot(
  length(Vars3$week_num) == n_keyatm_docs(keyatm_dfm_clean),
  min(Vars3$week_num) == 1,
  all(diff(Vars3$week_num) %in% c(0, 1))
)

message("Kept ", nrow(Vars_clean), " docs across ", max(Vars3$week_num), " weeks (",
        min(Vars_clean$week), " to ", max(Vars_clean$week), ")")

overtime <- keyATM(
  docs = keyatm_dfm_clean,
  no_keyword_topics = 5,
  keywords = keywords_dyn,
  model = "dynamic",
  model_settings = list(time_index = Vars3$week_num, num_states = 10),
  options = list(seed = 1998)
)

fig_timetrend <- plot_timetrend(overtime, time_index_label = Vars_clean$week, xlab = "Week")
save_fig(fig_timetrend, "Plots/timetrend.png", width = 10, height = 8)
fig_timetrend
