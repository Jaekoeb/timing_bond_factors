
library(tidyverse)
bond <- read_csv("data/bond_returns.csv")


# -------------------------------------------------------------------------


df <- bond |> filter(!is.na(ret_exc), !is.na(yield))

df <- df |> 
  group_by(eom, rating_group) |> 
  mutate(
    portfolio = ntile(yield, 3)
  ) |> 
  ungroup()


check <- df |> group_by(eom, portfolio, rating_group) |> summarise(obs = n())
rm(check)



df <- df |> 
  group_by(eom, rating_group, portfolio) |> 
  summarise(
    return = weighted.mean(ret_exc, market_value),
    yield = weighted.mean(yield, market_value, na.rm = TRUE),
    duration = weighted.mean(duration, market_value, na.rm = TRUE),
    market_value = mean(market_value),
    .groups = "drop")
