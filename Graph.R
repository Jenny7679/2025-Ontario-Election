library(ggplot2)
library(dplyr)

# Ensure data is sorted
polls <- polls %>%
  arrange(`Last Date of Polling`)

# Publication-ready plot
ggplot(polls, aes(x = `Last Date of Polling`)) +
  
  # Lines for each party
  geom_smooth(aes(y = PC, color = "PC"), se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = NDP, color = "NDP"), se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = Liberal, color = "Liberal"), se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = Green, color = "Green"), se = FALSE, linewidth = 1) +
  
  # Labels and theme
  labs(
    title = "Ontario Vote Intention Over the Campaign Period",
    subtitle = "Polling trends by party (2022–2025)",
    x = "Date",
    y = "Vote Intention (%)",
    color = "Party",
    caption = "Source: Wikipedia polling aggregate"
  ) +
  
  scale_color_manual(values = c(
    "PC" = "#1f4e79",
    "NDP" = "#FFA500",
    "Liberal" = "#e63946",
    "Green" = "#2a9d8f"
  )) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )