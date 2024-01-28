
full_data |>
  distinct(income_group)



group_df <- full_data |>
  group_by(income_group) |>
  nest()


linear_model <- function(df) {
  estimatr::lm_robust(
    growth ~ lag(growth) + debt_imp + lag(debt_imp) + lag(debt_imp, 2) + pl_x + pl_m,
    fixed_effects = ~ country + year,
    clusters = country,
    data = df
  )
}

group_df <-
  group_df |>
  mutate(model = map(data, linear_model))

linear_fits <-
  group_df |>
  mutate(glance = map(model, broom::tidy)) |>
  unnest(glance) |>
  select(income_group,
         term,
         estimate,
         statistic,
         p.value,
         conf.low,
         conf.high) |>
  mutate(model = "OLS")

write_csv(linear_fits, "Linear Fits.csv")

full_data |>
  distin
