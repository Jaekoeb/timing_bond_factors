

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(xtable)
library(Farben)
library(stargazer)
library(PeerPerformance)
library(sandwich)
library(lmtest)

load("data/aggregated_bonds.RData")
load("data/market.RData")


# Keep only market portfolio
market <- market |> 
  filter(factor == "market") |> 
  mutate(market = lead(return)) |> 
  select(eom, market)



# Number of Bonds ---------------------------------------------------------



# Lets see how many bonds are in each portfolio
check <- aggre |> 
  group_by(eom) |> 
  summarize(
    nobs = n(),
    long = sum(flag_long),
    top_10 = sum(flag_top10),
    top_1 = sum(flag_top1)
  )



check <- check |> select(-eom) |> as.matrix()

check <- apply(check, 2, FUN = median)


# Weights -----------------------------------------------------------------

weight <- aggre |> 
  group_by(eom) |> 
  summarize(
    long = sum(flag_long * weight),
    top_10 = sum(flag_top10 * weight),
    top_1 = sum(flag_top1 * weight),
    .groups = "drop"
  ) |> 
  mutate(
    top_10 = top_10 / long,
    top_1 = top_1 / long
  ) |> 
  select(top_10, top_1)


weight <- weight |> 
  pivot_longer(cols = everything(), names_to = "strategy", values_to = "weight") |> 
  group_by(strategy) |> 
  summarize(
    min = min(weight),
    q1 = quantile(weight, 0.25),
    median = quantile(weight, 0.5),
    q3 = quantile(weight, 0.75),
    max = max(weight)
  )

print(
  xtable(weight, caption = "weight"),
  file = "results/long_only/weight.txt"
)

rm(weight)

# Characteristics ---------------------------------------------------------


library(dplyr)
library(tidyr)

chars <- aggre |>
  mutate(market_value = market_value / 10^6, yield = 100 * yield) |> 
  pivot_longer(cols = c(flag_long, flag_top1, flag_top10), 
               names_to = "flag_type", 
               values_to = "flag_value") |>
  filter(flag_value == 1) |>
  group_by(flag_type) |>
  summarise(
    across(c(market_value, yield, duration), 
           list(p5 = ~quantile(.x, 0.05, na.rm = TRUE),
                q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                median = ~median(.x, na.rm = TRUE),
                q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                p95 = ~quantile(.x, 0.95, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) |>
  # Pivot longer to get one row per flag-characteristic combination
  pivot_longer(cols = -flag_type,
               names_to = c("characteristic", "statistic"),
               names_sep = "_(?=p5|q1|median|q3|p95)",
               values_to = "value") |>
  # Pivot wider to get statistics as columns
  pivot_wider(names_from = statistic,
              values_from = value) |> 
  arrange(characteristic, flag_type)


print(
  xtable(chars, caption = "characteristics of long only"),
  file = "results/long_only/characteristics.txt"
)

rm(chars)



# Turnover ----------------------------------------------------------------


turn <- aggre |> 
  pivot_longer(cols = c(flag_long, flag_top1, flag_top10),
               names_to = "portfolio",
               values_to = "flag_value") |> 
  filter(flag_value == 1) |> 
  group_by(eom, portfolio) |> 
  mutate(
    weight = weight / sum(weight, na.rm = TRUE)
  ) |> 
  ungroup()


turn <- turn |> 
  group_by(cusip, portfolio) |> 
  arrange(eom) |> 
  mutate(
    weight_lag = ifelse(is.na(lag(weight)), 0, lag(weight)),
    weight_lag = weight_lag * (1 + ret_exc),
    turnover = weight - weight_lag
  ) |> 
  ungroup()


turn <- turn |> 
  group_by(eom, portfolio) |> 
  summarize(
    turnover = sum(abs(turnover)) / 2,
    turnover = min(turnover, 2),
    .groups = "drop"
  )


# remove first observation
turn <- turn |> filter(eom != "2002-09-30")

turn <- turn |>
  mutate(turnover = 100 * turnover) |> 
  group_by(portfolio) |> 
  summarise(
    min = min(turnover),
    q1 = quantile(turnover, 0.25),
    median = quantile(turnover, 0.5),
    q3 = quantile(turnover, 0.75),
    max = max(turnover),
    .groups = "drop"
  )


print(
  xtable(turn, caption = "turnover"),
  file = "results/long_only/turnover.txt"
)

rm(turn)


# Returns Series ----------------------------------------------------------


# Compute the return of each portfolio
returns <- aggre |> 
  group_by(eom) |> 
  summarize(
    long = sum(flag_long * weight * ret_exc) / sum(flag_long * weight),
    top_1 = sum(flag_top1 * weight * ret_exc) / sum(flag_top1 * weight),
    top_10 = sum(flag_top10 * weight * ret_exc) / sum(flag_top10 * weight),
    .groups = "drop"
  )


# Join market data frame
returns <- left_join(returns, market, join_by(eom == eom))

# Remove outlier day
returns <- returns |> filter(eom != "2003-12-31")

rm(market)


# Performance -------------------------------------------------------------

perf <- xts(returns[,-1], order.by = returns$eom)


# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0),
  VaR(perf, method = "gaussian") * 100
)


# Change column names
rownames(perf) <- c("Ann. Return", "Ann. Volatility", "Worst Drawdown", "Sharpe Ratio", "VaR (95%)")



print(
  xtable(perf, caption = "Multi-Factor Metrics"),
  file = "results/long_only/performance.txt"
)




# Define target monthly volatility (since returns appear to be monthly)
target_monthly_vol <- 0.05 / sqrt(12)  # Convert annual target to monthly

df <- returns |> 
  mutate(
    # Scale each series to target monthly volatility
    across(c(market, long, top_1, top_10),
           ~ .x * target_monthly_vol / sd(.x, na.rm = TRUE)),
    # Convert to cumulative performance (base 100)
    across(c(market, long, top_1, top_10), 
           ~ 100 * cumprod(1 + .x) / first(1 + .x))
  ) |> 
  select(eom, market, long, top_1, top_10) |> 
  pivot_longer(!eom, names_to = "strategy", values_to = "value")


gg <- ggplot(data = df, aes(x = eom, y = value, group = strategy, color = strategy)) +
  geom_line(linewidth = 1) +
  labs(title = "Long-Only Strategies compared to the Market Portfolio",
       subtitle = "Scaled to 5% annual volatility",
       x = "",
       y = "",
       color = "Strategy") +
  scale_color_manual(
    values = c("market" = "black", "long" = space[1], "top_1" = space[2], "top_10" = space[3]),
    labels = c("market" = "Market Portfolio", "long" = "Long-Only", "top_1" = "Top 1% Long", "top_10" = "Top 10% Long")  # Custom legend labels
  ) +
  theme_bw()


ggsave(
  filename = "results/long_only/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(df, gg, target_monthly_vol)



# Sharpe Ratios -----------------------------------------------------------

sharpe <- data.frame()

# Long Only
stat <- with(returns, sharpeTesting(long, market))
stat <- as.data.frame(t(unlist(stat)))
stat$name <- "Long Only"
sharpe <- rbind(sharpe, stat)

# Top 1% Bonds
stat <- with(returns, sharpeTesting(top_1, market))
stat <- as.data.frame(t(unlist(stat)))
stat$name <- "Top 1%"
sharpe <- rbind(sharpe, stat)

# Top 100 Bonds
stat <- with(returns, sharpeTesting(top_10, market))
stat <- as.data.frame(t(unlist(stat)))
stat$name <- "Top 100"
sharpe <- rbind(sharpe, stat)

colnames(sharpe) <- c("Obs.", "Strategy SR", "Market SR", "Difference", "T-Stat", "P-Value", "Strategy")
sharpe <- sharpe |> select(Strategy, everything())

print(
  xtable(sharpe, caption = "Long Only Sharpe Ratios"),
  file = "results/long_only/sharpe.txt"
)



# Regressions -------------------------------------------------------------


# Scale to %
returns <- returns |> mutate(across(-eom, ~ 100*.))

# Run your regressions
reg1 <- lm(long ~ market, data = returns)
reg2 <- lm(top_10 ~ market, data = returns)
reg3 <- lm(top_1 ~ market, data = returns)

# Compute Newey-West standard errors
# Specify the lag parameter based on your data frequency
nw_se1 <- sqrt(diag(NeweyWest(reg1, lag = 6, prewhite = FALSE)))
nw_se2 <- sqrt(diag(NeweyWest(reg2, lag = 6, prewhite = FALSE)))
nw_se3 <- sqrt(diag(NeweyWest(reg3, lag = 6, prewhite = FALSE)))

# Create list of standard errors for stargazer
se_list <- list(nw_se1, nw_se2, nw_se3)

# Save regression results with Newey-West standard errors
stargazer(reg1, reg2, reg3, 
          type = "latex", 
          se = se_list,
          out = "results/long_only/regression.tex")

rm(reg1, reg2, reg3, nw_se1, nw_se2, nw_se3, se_list)


# ICE BofA ----------------------------------------------------------------


bofa <- read_csv("data/ice_bofa_index.csv", 
                 col_types = cols(observation_date = col_date(format = "%Y-%m-%d")))

colnames(bofa) <- c("eom", "bofa")

returns <- left_join(returns, bofa, join_by(eom == eom))

returns <- returns |> mutate(bofa = 100 * (lead(bofa) - bofa) / bofa)


# Run your regressions
reg1 <- lm(long ~ bofa, data = returns)
reg2 <- lm(top_10 ~ bofa, data = returns)
reg3 <- lm(top_1 ~ bofa, data = returns)

# Compute Newey-West standard errors
# Specify the lag parameter based on your data frequency
nw_se1 <- sqrt(diag(NeweyWest(reg1, lag = 6, prewhite = FALSE)))
nw_se2 <- sqrt(diag(NeweyWest(reg2, lag = 6, prewhite = FALSE)))
nw_se3 <- sqrt(diag(NeweyWest(reg3, lag = 6, prewhite = FALSE)))

# Create list of standard errors for stargazer
se_list <- list(nw_se1, nw_se2, nw_se3)

# Save regression results with Newey-West standard errors
stargazer(reg1, reg2, reg3, 
          type = "latex", 
          se = se_list,
          out = "results/long_only/regression_bofa.tex")

rm(reg1, reg2, reg3, nw_se1, nw_se2, nw_se3, se_list)



