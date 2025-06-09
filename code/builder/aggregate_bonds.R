


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

# Compute weights
aggre <- aggre |> 
  group_by(eom, factor) |> 
  mutate(
    weight = 1 / n()
  ) |> 
  ungroup() |> 
  mutate(
    weight = ifelse(sort == 5, weight, -weight)
  )

# Join bond info
aggre <- left_join(aggre, bond, join_by(id == id, cusip == cusip))

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
    flag_max = ifelse(weight >= quantile(weight, 0.99), 1, 0),
    flag_top = rank(-weight, ties.method = "random"),
    flag_top = ifelse(flag_top <= 100, 1, 0)
  ) |> 
  ungroup()


rm(member, const, bond)


# Save data frame
save(aggre, file = "data/aggregated_bonds.RData")

