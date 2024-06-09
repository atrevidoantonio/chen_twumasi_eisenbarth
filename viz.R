library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(ggthemes)
library(scales)
library(extrafont)
library(ggpubr)

sapphire <- "#255F85"
lapis <- "#26619c"
classic_blue <- "#0F4C81"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
raspberry <- "#C33149"
saffron <- "#F49D37"
dark_violet <- "#351C45"
dark_magenta <- "#861B54"
dark_emerald <- "#1F7A69"
satin <- "#D65C70"
onyx <- "#383C42"
rhythm <- "#6E758E"


wb_classes <- read_csv("./data/wb_classes.csv") |> janitor::clean_names() |>
  select(-economy)

countrycodes |>
  left_join(wb_classes, by = join_by(countrycode == code))

df <- read_csv("./data/merged_data.csv")

df <- mutate(df, region = if_else(country == "Cote d'Ivoire", "Sub-Saharan Africa", region))

df <-
  mutate(df, lmh = str_to_title(lmh) %>% factor(
    .,
    levels = c(
      "Low Income",
      "Lower-Middle Income",
      "Upper-Middle Income",
      "High Income"
    )
  ))


defacto <- readxl::read_xlsx("./data/defacto_regimes.xlsx")
dejure <- readxl::read_xlsx("./data/dejure_regimes.xlsx")

theme_clean <- function(...) {
  ggpubr::theme_classic2() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 1),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.25, colour = "black"),
      legend.title = element_text(size = 10, face = "plain"),
      legend.key = element_rect(fill = NA, color = NA),
      text = element_text(color = "black", family = "Arial", size = 10),
      strip.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      axis.ticks.length = unit(0.20, "cm"),
      axis.ticks = element_line(linewidth = 0.15)
    )
}

# grid style ggplot theme
theme_grid <- function(...) {
  theme_light() +
    theme(
      legend.background = element_rect(color = NA),
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      panel.border = element_blank(),
      legend.title = element_text(size = 10, face = "plain"),
      text = element_text(family = "Arial", color = "darkgrey"),
      plot.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(colour = 'black')
    )
}

ggplot(df, aes(
  x = pvd,
  y = cg,
  size  = pop,
  color = factor(region)
)) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = glossy_grape), nrow = 2)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(
    y = "Total government debt\n",
    x = "\nPrivate sector debt",
    fill = "",
    color = "",
    size = "Population"
  )


df %>% group_by(country, lmh) %>% summarize(
  pvd = mean(pvd, na.rm = TRUE),
  cg = mean(cg, na.rm = TRUE),
  pop = max(pop, na.rm = TRUE)
) %>%
  ggplot(aes(
    x = pvd,
    y = cg,
    size  = pop,
    color = lmh
  )) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = wesanderson::wes_palette("AsteroidCity3")) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")

ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    rgdpo = mean(rgdpo, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = rgdpo,
      y = cg,
      size  = pop,
      color = lmh)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_x_log10(guide = "axis_minor", labels = dollar_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = wesanderson::wes_palette("AsteroidCity3")) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")
  

ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    labsh = mean(labsh, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = labsh,
      y = cg,
      color = lmh,
      size  = pop)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_color_manual(values = wesanderson::wes_palette(name = "AsteroidCity1")) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(y = "Total government debt\n",
       x = "\nLabor share",
       color = "",
       size = "Population")

ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(
    x = lmh,
    y = cg,
    color = lmh,
    size  = pop
  )
) +
  geom_jitter(width = 0.1) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = glossy_grape), nrow = 2)) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = wesanderson::wes_palette("AsteroidCity1")) +
  labs(
    y = "Total government debt\n",
    x = "",
    fill = "",
    color = "",
    size = "Population"
  )


