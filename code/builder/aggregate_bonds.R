


# Libraries and Data ------------------------------------------------------

library(tidyverse)

load("data/member.RData")
load("data/constituents.RData")
bond <- read_csv("data/bond_returns.csv", 
                 col_types = cols(eom = col_date(format = "%Y-%m-%d")))


# Preperare data frames & merge ---------------------------------------------------


# create id columns to merge, for bond lag the id to get the leading returns (as in the portfolio sort)
member <- member |> mutate(id = format(eom, "%Y-%m")) |> select(id, factor)
bond <- bond |> 
  group_by(cusip) |> 
  arrange(eom) |> 
  mutate(id = lag(format(eom, "%Y-%m"))) |> 
  select(-eom)


# Pivot longer and only keep long / short leg
const <- const |> 
  pivot_longer(!c(eom, cusip), names_to = "factor", values_to = "sort") |> 
  filter(sort %in% c(1,5))

# Create id column
const <- const |> mutate(id = format(eom, "%Y-%m"))

# Join constituents and multi-timing
aggre <- inner_join(const, member, join_by(id == id, factor == factor))

# Join bond info
aggre <- left_join(aggre, bond, join_by(id == id, cusip == cusip))

# Compute weights
aggre <- aggre |> 
  group_by(eom, factor, rating_group) |> 
  mutate(
    weight = market_value / sum(market_value) * (1/3)
  ) |> 
  ungroup() |>
  mutate(
    weight = ifelse(sort == 5, weight, -weight)
  )



# Summarize single bonds across portfolio
aggre <- aggre |> 
  group_by(eom, cusip) |> 
  summarize(
    weight = sum(weight),
    market_value = mean(market_value),
    ret_exc = mean(ret_exc),
    ret_texc = mean(ret_texc, na.rm = TRUE),
    yield = mean(yield, na.rm = TRUE),
    duration = mean(duration, na.rm = TRUE),
    .groups = "drop"
  )

set.seed(2025)
# Add flags for long-only, top bonds
aggre <- aggre |> 
  group_by(eom) |>
  mutate(
    flag_long = ifelse(weight > 0, 1, 0),
    weight_help = ifelse(flag_long == 1, weight, NA), # helper, fix top_1 and top_10
    flag_top1 = ifelse(weight >= quantile(weight_help, 0.99, na.rm = TRUE), 1, 0),
    flag_top10 = ifelse(weight >= quantile(weight_help, 0.9, na.rm = TRUE), 1, 0)
  ) |> 
  ungroup() |> 
  select(-weight_help)


rm(member, const, bond)


# Save data frame
save(aggre, file = "data/aggregated_bonds.RData")

