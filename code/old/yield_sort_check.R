

# Libraries and Data ------------------------------------------------------


library(tidyverse)
bond <- read_csv("data/bond_returns.csv") # raw bond returns
market <- read_csv("data/bond_mkt_term.csv") # DEF and TERM data
factors <- read_csv("data/bond_factors.csv") # Stolborg Factors

# -------------------------------------------------------------------------

# Lead returns
df <- bond |> 
  group_by(cusip) |> 
  arrange(eom) |> 
  mutate(return = lead(ret_exc)) |> 
  ungroup()

# Remove NAs
df <- df |> filter(!is.na(return), !is.na(yield))

# Sort into portfolio based on signal (here yield)
df <- df |> 
  group_by(eom, rating_group) |> 
  mutate(
    portfolio = ntile(yield, 3) # tertiles
  ) |> 
  ungroup()


# check <- df |> group_by(eom, portfolio, rating_group) |> summarise(obs = n())
# rm(check)


# Compute the market weighted portfolio for each rating group and yield portfolio
df <- df |> 
  group_by(eom, rating_group, portfolio) |> 
  summarise(
    return = weighted.mean(return, market_value),
    yield = weighted.mean(yield, market_value, na.rm = TRUE),
    duration = weighted.mean(duration, market_value, na.rm = TRUE),
    market_value = mean(market_value),
    .groups = "drop")


# Plot the return distribution in each rating group and portfolio
ggplot(df, aes(y = return, group = as.factor(portfolio))) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~ rating_group) +
  labs(title = "Return Distribution by Portfolio and Rating Group",
       x = "Portfolio",
       y = "Return") +
  theme_bw()


# Compute the equal weighted portfolio by aggregating the rating groups
df <- df |> 
  group_by(eom, portfolio) |> 
  summarise(
    return = mean(return),
    market_value = mean(market_value),
    yield = mean(yield),
    duration = mean(duration),
    .groups = "drop")


# Add long short portfolio
ls <- inner_join(
  x = df |> filter(portfolio == 1),
  y = df |> filter(portfolio == 3),
  join_by(eom == eom)
)

ls <- ls |> 
  mutate(
    portfolio = "ls",
    return = return.y - return.x,
    market_value = (market_value.x + market_value.y) / 2,
    yield = (yield.x + yield.y) / 2,
    duration = duration.y - duration.x
  ) |> 
  select(
    eom,
    portfolio,
    return,
    market_value,
    yield,
    duration
  )


# Join observations
df <- rbind(df, ls)
df <- df |> 
  arrange(eom, portfolio)


# Add the market factors to the long short returns
ls <- left_join(ls, market, join_by(eom == date))

# Add the Stolborg yield factor
ls <- left_join(ls,
                factors |> select(date, `Yield to maturity`) |> rename(stol := `Yield to maturity`),
                join_by(eom == date))


# Run regression (my factor)
reg <- lm(return ~ market + term, data = ls)
summary(reg) # significant negative alpha


# Run regression (Stolborg)
reg <- lm(stol ~ market + term, data = ls)
summary(reg) # significant negative alpha


# Correlation
with(ls, cor(return, stol))


# Plot performance of both factors
ls |> 
  mutate(
    pertl = cumprod(1 + return) * 100 / first(1 + return),
    stol = cumprod(1 + stol) * 100 / first(1 + stol),
  ) |> 
  select(eom, pertl, stol) |> 
  pivot_longer(!eom, names_to = "factor") |> 
  ggplot(aes(x = eom, y = value, group = factor, color = factor)) +
  geom_line(size = 1) + 
  theme_bw()
  
