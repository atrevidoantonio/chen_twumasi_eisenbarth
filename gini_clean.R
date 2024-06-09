
gini <- read_dta("C:/Users/ageis/Downloads/swiid9_6.dta") |>
  select(country, year, 4:103) |>
  pivot_longer(cols = 3:102) |>
  summarize(gini = mean(value), .by = c(country, year)) |>
  complete(country, year)

gini_countries <-
gini |>
  distinct(country) |>
  mutate(match = TRUE) 

dataset_countries <-
dataset |>
  distinct(country) |>
  mutate(inclued = TRUE) |>
  left_join(gini_countries)

missing_gini <-
  gini |>
  filter(year >= 1980) |>
  summarize(missing = sum(if_else(is.na(gini), 1, 0)), .by = country) |>
  arrange(-missing)

included <-
  missing_gini |>
  filter(missing <= 20) |>
  distinct(country) |>
  as_vector()

included


final_gini <- gini |> filter(country %in% included, year >= 1980) |>
  mutate(
    country = case_when(
      country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      country == "Congo-Kinshasa" ~ "Congo",
      country == "Korea" ~ "South Korea",
      TRUE ~ as.character(country)
    )
  )

 distinct(final_gini, country)
