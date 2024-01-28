library(dplyr)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggh4x)
library(ggthemes)
library(scales)
library(ggpubr)
library(car)
library(ARDL)
library(estimatr)

df <- read_csv("./data/merged_data.csv")
defacto <- readxl::read_xlsx("./data/defacto_regimes.xlsx")
dejure <- readxl::read_xlsx("./data/dejure_regimes.xlsx")

df <- rename(df, 
       iso = countrycode,
       ifs = ifscode) |>
  mutate(gdp_growth = gdp_growth / 100)

distinct(df, country)

missing_debt <-
  df |>
  summarize(missing_obs = sum(is.na(gg)),
            .by = c(country, countrycode, ifscode))

dejure <- left_join(chudik_df, dejure) |>
  arrange(country, year)

defacto <- left_join(chudik_df, defacto) |>
  arrange(country, year)

fit <-
  lm(
    gdp_growth ~ gg + inflation + labsh + independence_dummy + us_pegger_dummy + eur_pegger_dummy + dual_market_dummy + european_union + exports + imports + exports *
      imports,
    weights = population,
    data = defacto
  )


estimatr::lm_robust(
  growth ~ debt_imp + lag(debt_imp),
  weights = pop,
  fixed_effects = ~ country,
  data = chudik_df
)

tbl <-
  dataset |>
  mutate(
    growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
    l_growth = log(rgdpo / lag(rgdpo)),
    .by = country
  ) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg)))

missing_debt <-
  tbl |>
  summarize(
    missing_obs_gg = sum(is.na(gg)),
    missing_obs_cg = sum(is.na(cg)),
    missing_obs_debt = sum(is.na(debt)),
    .by = c(country, countrycode)
  )

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

data_df |>
  as_tsibble(index = year,
             key = c(ifscode, countrycode, country)) -> ts

ts |>
  ggplot(aes(x = year, y = growth_imp, group = country)) +
  geom_line() +
  scale_y_continuous() +
  theme_clean() +
  facet_wrap(~region, scales = "free")

ts_decomp <- ts |>
  # Fit a non-seasonal STL decomposition
  model(
    stl = STL(growth_imp ~ season(period = 1), robust = TRUE)
  ) |>
  components()


outliers <- ts_decomp |>
  mutate(
    lower = quantile(remainder, 0.25) - 3 * IQR(remainder),
    upper = quantile(remainder, 0.75) + 3 * IQR(remainder),
    .by = country
  ) |>
  filter(remainder < lower |
           remainder > upper) |>
  transmute(country, year, outlier = TRUE)

ts |>
  anti_join(outliers) |>
  fill_gaps() -> ts_df
  ggplot(aes(x = year, y = growth_imp, group = country)) +
  geom_line() +
  scale_y_continuous() +
  theme_clean() +
  facet_wrap(~region, scales = "free")


chudik_countries <- unique(chudik$Country)

chudik_df <- filter(data_df, country %in% chudik_countries, year >= 1960)


maddison <- select(mpd2020, year, countrycode, rgdpc = gdppc, population = pop)

chudik |>
  janitor::clean_names() |>
  left_join(maddison) |>
  filter(year >= 1950) |>
  transmute(year,
            country, 
            countrycode, 
            rgdpc,
            population = population * 1000) |>
  mutate(growth =  (rgdpc - lag(rgdpc)) / lag(rgdpc),
         log_growth  = log(rgdpc / lag(rgdpc)), .by = country) -> chudik_gdp


chudik <- chudik |>
  mutate(country = if_else(country == "Morroco", "Morocco", as.character(country)))

countrycodes <- countrycodes |>
  mutate(country = if_else(country == "Korea", "South Korea", as.character(country)))



chudik |>
  janitor::clean_names() |>
  left_join(countrycodes) -> chudik

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
  select(year, country, growth, growth_imp, growth_arima) |>
  pivot_longer(cols = c(growth, growth_imp, growth_arima)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  ggsci::scale_colour_npg(labels = c("Original", "Imputed", "ARIMA Imputation")) +
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


library(plm)
ardl_fit %>%
  mutate(tidy = map(ardl, sw_tidy))
unnest(tidy)

spread(key = category.secondary, value = estimate)
mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = category.secondary, value = estimate)




wb_data_new <- read_csv("data/wb_data_new.csv") |> janitor::clean_names() |> filter(!is.na(country_code))
wb_data <- read_csv("data/wb_data_2005.csv")

distinct(wb_data_new, series_name, series_code) -> wb_vars

wb_vars |>
  mutate(variable_name = case_when(
    series_code == "FS.AST.DOMS.GD.ZS" ~ "domestic_credit",
    series_code == "NE.EXP.GNFS.ZS" ~ "exports",
    series_code == "DT.DOD.DECT.CD" ~ "external_debt_total",
    series_code == "BX.KLT.DINV.CD.WD" ~ "fdi_net_inflows",
    series_code == "NY.GDP.MKTP.CD" ~ "gdp_current",
    series_code == "NY.GDP.MKTP.KD.ZG" ~ "gdp_growth_annual",
    series_code == "NY.GNP.PCAP.CD" ~ "gni_per_capita_atlas",
    series_code == "NY.GNP.PCAP.PP.CD" ~ "gni_per_capita_ppp",
    series_code == "NY.GNP.ATLS.CD" ~ "gni_atlas_usd",
    series_code == "NY.GNP.MKTP.PP.CD" ~ "gni_ppp",
    series_code == "NE.GDI.TOTL.ZS" ~ "capital_formation",
    series_code == "NE.IMP.GNFS.ZS" ~ "imports",
    series_code == "SI.DST.FRST.20" ~ "income_share_lowest20",
    series_code == "NV.IND.TOTL.ZS" ~ "industry_value_added_gdp",
    series_code == "NY.GDP.DEFL.KD.ZG" ~ "inflation",
    series_code == "SP.DYN.LE00.IN" ~ "life_expectancy_birth",
    series_code == "TG.VAL.TOTL.GD.ZS" ~ "merchandise_trade",
    series_code == "MS.MIL.XPND.GD.ZS" ~ "military_expenditure",
    series_code == "SP.POP.GROW" ~ "population_growth_annual",
    series_code == "SP.POP.TOTL" ~ "population_total",
    series_code == "GC.TAX.TOTL.GD.ZS" ~ "tax_revenue",
    series_code == "DT.TDS.DECT.EX.ZS" ~ "total_debt_service_exports",
    series_code == "FM.LBL.BMNY.GD.ZS" ~ "broad_money",
    series_code == "GC.DOD.TOTL.GD.ZS" ~ "cg_debt",
    TRUE ~ NA_character_
  )) -> wb_vars

wb_data_new |>
  pivot_longer(names_to = "year",
               cols = 5:67) |>
  left_join(wb_vars) |>
  select(-c(series_name, series_code)) |>
  pivot_wider(names_from = variable_name) |>
  mutate(year = seq(1960, 2022, by = 1), .by = country_code) -> wb_data
  
wb_data |>
  mutate(across(
    c(
      domestic_credit,
      exports,
      imports,
      gdp_growth_annual,
      cg_debt,
      broad_money,
      tax_revenue,
      military_expenditure,
      merchandise_trade,
      inflation,
      total_debt_service_exports,
      population_growth_annual,
      income_share_lowest20,
      capital_formation,
      industry_value_added_gdp
    ),
    ~ .x / 100
  )) -> wb_data
