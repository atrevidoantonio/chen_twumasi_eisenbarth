library(dplyr)
library(tidyverse)
library(tidyquant)
library(scales)
library(ggplot2)
library(distributional)
library(ggdist)
library(extrafont)
library(ggh4x)
library(ggthemes)
library(hrbrthemes)
library(fredr)

prussian <- "#293E66"
grape <- "#968AA8"
saffron <- "#F49D37"
magenta <- "#922D50"
sapphire <- "#255F85"
taupe <- "#715B5B"
rainy_day <- "#474C5c"
baby_pink <- "#EDBFC6"

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
      axis.line = element_line(linewidth = 0.10, colour = "#181818"),
      legend.title = element_text(size = 10, face = "plain"),
      legend.key = element_rect(fill = NA, color = NA),
      text = element_text(color = onyx, family = "Arial Narrow", size = 10),
      strip.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks = element_line(linewidth = 0.10)
    )
}


imf_debt <- haven::read_dta("./data/imf_debt_data.dta")
chudik <- read_csv("./data/chudik_countries.csv")

#' change ampersands to "and"
imf_debt <-
  mutate(imf_debt, country = gsub("&", "and", country)) |>
  mutate(
    #' this fixes the discrepancy between the two sources in terms of country names
    country = case_when(
      country == "C.A.R." ~ "Central African Republic",
      country == "Congo, Dem. Rep. of" ~ "Congo D.R.",
      country == "Cote D'Ivoire" ~ "Cote d'Ivoire",
      country == "Kyrgyz Republic" ~ "Kyrgyzstan",
      country == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
      country == "Slovak Republic" ~ "Slovakia",
      country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
      country == "St. Lucia" ~ "Saint Lucia",
      country == "Taiwan Province of China" ~ "Taiwan",
      country == "U.A.E." ~ "United Arab Emirates",
      TRUE ~ country,
    )
  ) |>
  #' remove unnecessary strings after comma
  mutate(country = gsub("^(.*?),.*", "\\1", country)) |>
  #' trim whitespace
  mutate(country = trimws(country)) |>
  mutate(across(
    c(pvd_all, pvd_ls, hh_all, hh_ls, nfc_all, nfc_ls, ps, nfps, gg, cg),
    ~ .x / 100
  ))


tbl <-
  df |>
  mutate(growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
         l_growth = log(rgdpo/lag(rgdpo)), .by = country) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg)))

missing_debt <-
  tbl |>
  summarize(missing_obs_gg = sum(is.na(gg)),
            missing_obs_cg = sum(is.na(cg)),
            missing_obs_debt = sum(is.na(debt)),
            .by = c(country, countrycode, ifscode))

excluded <- filter(missing_debt, missing_obs_debt > 20)

excluded_countries <- unique(excluded$country)

data_df <- filter(tbl, !country %in% c(excluded_countries)) |>
  mutate(
    growth = (gdp - lag(gdp)) / lag(gdp),
    l_growth = log(gdp / lag(gdp)),
    .by = country
  ) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg))) |>
  mutate(
    debt_imp = imputeTS::na_kalman(debt, model = "auto.arima"),
    growth_imp = imputeTS::na_interpolation(growth),
    .by = country
  ) |>
  left_join(wb_classes, by = join_by(countrycode == code)) |>
  relocate(c(region, income_group, lending_category), .after = country)

data_df |>
  select(year, country, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", raspberry), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  theme_clean()

chudik_countries <- unique(chudik$Country)

chudik_df <- filter(data_df, country %in% chudik_countries, year >= 1960)

chudik_df |>
  select(year, country, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", raspberry), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  theme_clean()


chudik_df |>
  select(year, country, growth, growth_imp) |>
  pivot_longer(cols = c(growth, growth_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", raspberry), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  theme_clean()


nested_data <- chudik_df %>%
  filter(year >= 1960) %>%
  group_by(country) %>%
  nest()

# Function to create time series including 'growth' and 'debt_imput'
create_time_series <- function(data) {
  ts_data <- ts(c(data$growth, data$debt_imp), start = 1950, end = 2019)
  return(ts_data)
}

library(timetk)

nest_ts <- nested_data %>%
  mutate(data.ts = map(.x = data,
                       .f = ~ ts(
                         select(.x, growth_imp, debt_imp),
                         start = 1960,
                         end = 2019
                       )))


# Define a function to fit ARDL model
fit_ardl <- function(data) {
  ARDL::auto_ardl(growth_imp ~ debt_imp, data = data, max_order = 3)  # Assuming auto_ardl function requires 'data' argument
}

ardl_fit <- nest_ts %>%
  mutate(ardl = map(data.ts, fit_ardl))

ardl_fit %>%
 mutate(tidy = map(ardl, sw_tidy))
    unnest(tidy)
    spread(key = category.secondary, value = estimate)
mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = category.secondary, value = estimate)

ts <- zoo::zooreg(data = chudik_df, start = 1950, end = 2019)


models <- ARDL::auto_ardl(growth ~ debt_imp, data = ts, max_order = 5)
models$best_order
models$best_model
