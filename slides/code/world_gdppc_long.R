library(readxl)
library(dplyr)
library(ggplot2)

d <- read_excel("/tmp/mpd2023.xlsx", sheet="Full data")

world <- d %>%
  filter(!is.na(gdppc), !is.na(pop)) %>%
  group_by(year) %>%
  summarize(
    gdppc_world = sum(gdppc * pop) / sum(pop),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  filter(n_countries >= 5)

p <- ggplot(world, aes(x = year, y = gdppc_world)) +
  geom_line(color = "#2c3e50", linewidth = 0.9) +
  geom_point(data = world %>% filter(year %in% c(1, 1000, 1500, 1700, 1820, 1900, 1950, 2000, 2022)),
             color = "#2c3e50", size = 2.2) +
  scale_y_log10(
    breaks = c(1000, 2000, 5000, 10000, 20000),
    labels = scales::dollar_format()
  ) +
  scale_x_continuous(
    breaks = c(1, 500, 1000, 1500, 1800, 1900, 2000),
    limits = c(1, 2022)
  ) +
  annotate("rect", xmin = 1800, xmax = 2022, ymin = 0, ymax = Inf,
           alpha = 0.08, fill = "#e67e22") +
  annotate("text", x = 1910, y = 1100, label = "post-1800",
           color = "#b25500", size = 3.5, fontface = "italic") +
  labs(
    title = "World GDP per capita, year 1 to 2022",
    subtitle = "Population-weighted average across all countries with available data, log scale",
    y = "GDP per capita (2011 USD, log scale)",
    x = NULL,
    caption = "Source: Maddison Project Database 2023 (Bolt and van Zanden)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#555555"),
    plot.caption = element_text(color = "#888888", hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave("/Users/carolinatorreblanca/Documents/GitHub/psci3200-globaldev-main/slides/img/world_gdppc_long.png",
       p, width = 9, height = 5.2, dpi = 200, bg = "white")

cat("Saved world_gdppc_long.png\n")
