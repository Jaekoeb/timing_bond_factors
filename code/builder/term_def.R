


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(data.table)

bond <- read_csv("data/bond_returns.csv")
treasury_yld <- read_csv("data/treasury_yld.csv")



# Join Treasury Info ------------------------------------------------------

treasury_yld <- treasury_yld |> 
  mutate(
    eom = as.Date(MCALDT),
    duration = TMDURATN / 365,
    tyield = TMYLD * 365,
    id = format(eom, "%Y-%m")
  ) |> 
  select(
    eom, id, duration, tyield
  )


bond <- bond |> mutate(id = format(eom, "%Y-%m"))


bond <- setDT(bond)
treasury_yld <- setDT(treasury_yld |> na.omit())

# Set keys for both tables
setkey(bond, id, duration)
setkey(treasury_yld, id, duration)


# Perform the rolling join: for each row in df1, find the closest matching duration in df2
# The join below adds columns from df2 (e.g., yield) to df1 based on eom and the nearest duration.
bond <- treasury_yld[bond, roll = "nearest"]

bond <- as.data.frame(bond) |> select(-id)


# Build -------------------------------------------------------------------


df <- bond |> 
  filter(
    !is.na(ret_exc) & !is.na(market_value) & !is.na(ret_texc)
  ) |> 
  select(
    eom, cusip, market_value, ret_exc, ret_texc, yield, tyield, duration
  ) |> 
  group_by(eom) |> 
  summarize(
    market.return = sum(market_value * ret_exc) / sum(market_value),
    market.value = mean(market_value),
    market.yield = weighted.mean(yield, market_value, na.rm = TRUE),
    market.duration = weighted.mean(duration, market_value, na.rm = TRUE),
    def.return = sum(market_value * ret_texc) / sum(market_value),
    def.value = market.value,
    def.yield = market.yield - weighted.mean(tyield, market_value, na.rm = TRUE),
    def.duration = 0
  ) |>
  ungroup()


# Create term column
market <- df |>
  mutate(
    term.return = market.return - def.return,
    term.value = NA,
    term.yield = market.yield - def.yield,
    term.duration = market.duration
    )



market <- market |>
  pivot_longer(
    cols = -eom,                     # Select all columns except 'eom'
    names_to = c("factor", "measure_type"), # Create 'factor' and 'measure_type' columns
    names_sep = "\\.",               # Split column names by the dot separator
    values_to = "value_temp"         # Temporary column for the values
  ) |> 
  pivot_wider(
    names_from = measure_type,       # Use values from 'measure_type' as new column names
    values_from = value_temp         # Fill these new columns with values from 'value_temp'
  ) |> 
  # Ensure the desired column order (dplyr::select can be used here)
  select(eom, factor, return, value, yield, duration)


save(market, file = "data/market.RData")
