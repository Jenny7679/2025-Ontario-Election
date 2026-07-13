# ==============================================================
# 05: Leader mentions by week (Ontario-only corpus)
# SELF-CONTAINED defensive version — replaces all earlier versions.
# Requires: df_tokens_on in the environment (from the ON/fed filter).
# ==============================================================
library(quanteda)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)

leader_dictionaries <- dictionary(list(
  ford      = c("doug ford", "ford"),
  crombie   = c("crombie"),
  stiles    = c("stiles"),
  schreiner = c("schreiner")
))

# ---- 1. Week variable (drop NA and corrupted dates) ------------------
d <- as.Date(docvars(df_tokens_on, "dates"))
bad_date <- is.na(d) | d < as.Date("2023-06-01") | d > as.Date("2025-12-31")
message(sum(bad_date), " documents with missing/corrupted dates excluded")

df_tokens_on <- tokens_subset(df_tokens_on, !bad_date)

wk <- as.Date(cut(as.Date(docvars(df_tokens_on, "dates")), "week"))
stopifnot(inherits(wk, "Date"), sum(is.na(wk)) == 0,
          min(wk) > as.Date("2023-01-01"))
docvars(df_tokens_on, "week") <- wk

# ---- 2. Leader dfm ---------------------------------------------------
leader_dfm <- df_tokens_on %>%
  tokens_lookup(leader_dictionaries) %>%
  dfm()

# ---- 3. Aggregate by week WITHOUT the dfm_group round trip ----------
# week comes straight from the docvar, so it is a Date by construction.
leader_df <- convert(leader_dfm, to = "data.frame")
leader_df$week <- docvars(leader_dfm, "week")
leader_df$doc_id <- NULL

weekly_mentions <- leader_df %>%
  group_by(week) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  pivot_longer(-week, names_to = "leader", values_to = "mentions")

weekly_articles <- leader_df %>%
  mutate(across(-week, ~ as.numeric(. > 0))) %>%
  group_by(week) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  pivot_longer(-week, names_to = "leader", values_to = "articles")

# ---- 4. Diagnostics: MUST show week as class Date -------------------
str(weekly_mentions)
stopifnot(inherits(weekly_mentions$week, "Date"))

# ---- 5. Plot: total mentions ----------------------------------------
election_day <- as.Date("2025-02-27")
writ_drop    <- as.Date("2025-01-28")

leader_cols <- c(ford = "#1f4e9c", crombie = "#d71920",
                 stiles = "#f37021", schreiner = "#3d9b35")
leader_labs <- c(ford = "Ford (PC)", crombie = "Crombie (OLP)",
                 stiles = "Stiles (NDP)", schreiner = "Schreiner (GPO)")

p_mentions <- ggplot(weekly_mentions,
                     aes(x = week, y = mentions, colour = leader)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = c(writ_drop, election_day),
             linetype = "dashed", colour = "grey55") +
  scale_colour_manual(values = leader_cols, labels = leader_labs, name = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "Party leader mentions in Ontario election coverage, by week",
       subtitle = "Deduplicated Ontario-focused corpus; dashed lines: writ drop and election day",
       x = NULL, y = "Mentions per week",
       caption = "Sources: ProQuest Canadian news corpus") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(p_mentions)
# ggsave(here::here("Figures/leader_mentions_weekly.png"),
#        p_mentions, width = 9, height = 5.5, dpi = 300)

# ---- 6. Plot: articles mentioning ------------------------------------
p_articles <- ggplot(weekly_articles,
                     aes(x = week, y = articles, colour = leader)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = c(writ_drop, election_day),
             linetype = "dashed", colour = "grey55") +
  scale_colour_manual(values = leader_cols, labels = leader_labs, name = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "Articles mentioning each leader, by week",
       x = NULL, y = "Articles mentioning per week") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(p_articles)

# ---- 7. Plot: share of mentions (stacked area) ------------------------
weekly_share <- weekly_mentions %>%
  group_by(week) %>%
  mutate(share = mentions / sum(mentions)) %>%
  ungroup()

p_share <- ggplot(weekly_share, aes(x = week, y = share, fill = leader)) +
  geom_area(position = "stack") +
  geom_vline(xintercept = c(writ_drop, election_day),
             linetype = "dashed", colour = "white") +
  scale_fill_manual(values = leader_cols, labels = leader_labs, name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "Share of leader mentions, by week",
       x = NULL, y = "Share of all leader mentions") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_share)