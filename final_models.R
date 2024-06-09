library(tidyverse)
library(estimatr)
library(stargazer)
library(plm)

dataset = read_csv(dataset, "final_dataset.csv")

p_df <- plm::pdata.frame(dataset, index = c("year", "country"))

upper_mid <- filter(dataset, income_group == "Upper middle income")
high <- filter(dataset, income_group == "High income")
low <- filter(dataset, income_group == "Low income")
lower_mid <- filter(dataset, income_group == "Lower middle income")

summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + lag(gdp) + gdp*debt_imp, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + lag(gdp) + gdp*debt_imp, data = upper_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + lag(gdp) + gdp*debt_imp, data = high, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + lag(gdp) + gdp*debt_imp, data = low, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + lag(gdp) + gdp*debt_imp, data = lower_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))



fit1 <- lm_robust(gini ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy , data = dataset, fixed_effects =  ~ country + year)
fit2 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = upper_mid, fixed_effects =  ~ country + year)
fit3 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = high, fixed_effects =  ~ country + year)
fit4 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = low, fixed_effects =  ~ country + year)
fit5 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = lower_mid, fixed_effects =  ~ country + year)
fit6 <- lm_robust(gdp ~ +lag(gdp) + debt_imp + lag(debt_imp) + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = full_df, fixed_effects =  ~ country + year)


glance(fit1) |> mutate(model = "Pooled")

model1 <- tidy(fit1) |> mutate(model = "Pooled")
model2 <- tidy(fit2) |> mutate(model = "Upper Middle")
model3 <- tidy(fit3) |> mutate(model = "High")
model4 <- tidy(fit4) |> mutate(model = "Low")
model5 <- tidy(fit5) |> mutate(model = "Lower Middle")
model6 <- tidy(fit6) |> mutate(model = "Pooled (ARDL)")

coefs <- bind_rows(model1, model2, model3, model4, model5, model6)

write_csv(coefs, "model_coefs.csv")

ts <- zoo::zooreg(data = full_data, start = 1973, end = 2019)

models <- ARDL::auto_ardl(gdp ~ debt_imp, data = ts, max_order = 5)
models$best_order
models$best_model


summary(plm(growth ~ debt_imp + pl_x + pl_m, data = p_df, model = "within", effect = "twoways"))

g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
pcdtest(g)