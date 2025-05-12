


# Libraries and Data ------------------------------------------------------

library(tidyverse)

bond <- read_csv("data/bond_returns.csv")



# Build -------------------------------------------------------------------


df <- bond |> 
  filter(
    !is.na(ret_exc) & !is.na(market_value) & !is.na(ret_texc)
  ) |> 
  select(
    eom, cusip, market_value, ret_exc, ret_texc
  ) |> 
  group_by(eom) |> 
  summarize(
    market = sum(market_value * ret_exc) / sum(market_value),
    def = sum(market_value * ret_texc) / sum(market_value)
  ) |>
  ungroup()


# Create term column
market <- df |> mutate(term = market - def)


save(market, file = "data/market.RData")
