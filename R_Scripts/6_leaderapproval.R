# ------------------------------------------------------------------
# Ontario leader approval — Abacus net-impression tracker, 2024–2025
# Data: data/ontario_leader_approval_polls (1).xlsx, sheet "Leader Approval Polls"
# Window: Jan 2024 through election day (Feb 27, 2025)
# ------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(here)

election_day <- as.Date("2025-02-27")

# --- 1. Read local file --------------------------------------------
raw <- read_excel(
  here("data", "ontario_leader_approval_polls (1).xlsx"),
  sheet = "Leader Approval Polls"
)

# --- 2. Clean ------------------------------------------------------
polls <- raw |>
  filter(
    str_detect(Pollster, regex("abacus", ignore_case = TRUE)),
    str_detect(Metric,   regex("net impression", ignore_case = TRUE))
  ) |>
  rename(
    field_end = `Field End`,
    leader    = Leader,
    positive  = `Positive %`,
    negative  = `Negative %`,
    net       = Net
  ) |>
  mutate(
    field_end = str_trim(as.character(field_end)),
    date_chr = if_else(
      str_detect(field_end, "^\\d{4}-\\d{2}$"),
      paste0(field_end, "-15"),   # month-only -> mid-month
      str_sub(field_end, 1, 10)   # already a full date
    ),
    date = ymd(date_chr, quiet = TRUE),
    across(c(positive, negative, net), as.numeric),
    net = coalesce(net, positive - negative),
    leader = factor(
      leader,
      levels = c("Ford", "Crombie", "Stiles", "Fraser (interim Lib)")
    )
  ) |>
  select(-date_chr) |>
  filter(!is.na(date), !is.na(net), !is.na(leader),
         date >= as.Date("2024-01-01"),
         date <= election_day) |>
  droplevels() |>
  arrange(leader, date)

stopifnot(nrow(polls) > 0)
message("Waves kept: ", nrow(polls))
print(count(polls, leader))

# --- 3. Direct labels at the end of each series --------------------
labels <- polls |>
  group_by(leader) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    label = str_replace(as.character(leader),
                        fixed("Fraser (interim Lib)"),
                        "Fraser\n(interim Lib)"),
    nudge_y = case_when(
      leader == "Stiles"               ~  1.5,
      leader == "Fraser (interim Lib)" ~ -1.5,
      TRUE                             ~  0
    )
  )

party_cols <- c(
  "Ford"                 = "#1A4782",
  "Crombie"              = "#D71920",
  "Stiles"               = "#F37021",
  "Fraser (interim Lib)" = "#8B0000"
)

y_bottom <- min(polls$net) - 2

# --- 4. Plot -------------------------------------------------------
p <- ggplot(polls, aes(x = date, y = net, colour = leader)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey55") +
  geom_vline(xintercept = election_day, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = election_day, y = y_bottom,
           label = "Election\nFeb 27, 2025",
           hjust = 1.05, vjust = 0, size = 3, colour = "grey30") +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2.2) +
  geom_text(data = labels,
            aes(label = label, y = net + nudge_y),
            hjust = -0.15, size = 3.4, fontface = "bold",
            lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = party_cols, guide = "none") +
  scale_x_date(breaks = seq(as.Date("2024-01-01"), election_day,
                            by = "1 month"),
               date_labels = "%b %Y",
               expand = expansion(mult = c(0.02, 0.14))) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Net Impressions of Ontario Party Leaders, 2024\u20132025",
    x = NULL, y = "Net impression (points)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(here("ontario_leader_net_impressions.png"), p,
       width = 10, height = 6, dpi = 300)