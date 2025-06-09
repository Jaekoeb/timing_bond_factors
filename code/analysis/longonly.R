

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)

load("data/aggregated_bonds.RData")
load("data/market.RData")


# Keep only market portfolio
market <- market |> 
  filter(factor == "market") |> 
  mutate(market = lead(return)) |> 
  select(eom, market)



# Lets see how many bonds are in each portfolio
check <- aggre |> 
  group_by(eom) |> 
  summarize(
    nobs = n(),
    long = sum(flag_long),
    top_max = sum(flag_max),
    top_100 = sum(flag_top)
  )



# Returns Series ----------------------------------------------------------

# Compute the return of each portfolio
returns <- aggre |> 
  group_by(eom) |> 
  summarize(
    long = sum(flag_long * weight * ret_exc) / sum(flag_long * weight),
    top_max = sum(flag_max * weight * ret_exc) / sum(flag_max * weight),
    top_100 = sum(flag_top * weight * ret_exc) / sum(flag_top * weight),
    .groups = "drop"
  )


# Join market data frame
returns <- left_join(returns, market, join_by(eom == eom))
rm(market)

# Performance -------------------------------------------------------------

perf <- xts(returns[,-1], order.by = returns$eom)


# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0)
)


# Change column names
rownames(perf) <- c("Ann. Return", "Ann. Volatility", "Worst Drawdown", "Sharpe Ratio")


sink("results/long_only/performance.txt")
xtable::xtable(perf,
               caption = "Multi-Factor Metrics")
sink()




# Scale the return series to match in volatility
target_vol <- 0.05
market.vol <- sd(returns$market) * sqrt(12)
long.vol <- sd(returns$long) * sqrt(12)
topmax.vol <- sd(returns$top_max) * sqrt(12)
top100.vol <- sd(returns$top_100) * sqrt(12)

df <- returns |> 
  mutate(
    market = market * target_vol / market.vol,
    market = 100 * cumprod(1+market) / first(1+market),
    long = long * target_vol / long.vol,
    long = 100 * cumprod(1+long) / first(1+long),
    top_max = top_max * target_vol / topmax.vol,
    top_max = 100 * cumprod(1+top_max) / first(1+top_max),
    top_100 = top_100 * target_vol / top100.vol,
    top_100 = 100 * cumprod(1+top_100) / first(1+top_100)
  ) |> 
  select(eom, market, long, top_max, top_100) |> 
  pivot_longer(!eom, names_to = "strategy", values_to = "value")


gg <- ggplot(data = df, aes(x = eom, y = value, group = strategy, color = strategy)) +
  geom_line(linewidth = 1) +
  labs(title = "Portfolio Performance Over Time",
       x = "",
       y = "Value",
       color = "Portfolio") +
  theme_bw()

ggsave(
  filename = "results/long_only/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(df, gg, perf, long.vol, market.vol, target_vol, top100.vol, topmax.vol)




# Regressions -------------------------------------------------------------


# Scale to (%) for interpretability
returns <- returns |> mutate(across(-eom, ~ 100 * .))



# long only
reg <- lm(long ~ market, data = returns)
summary(reg)


# Top 1% bonds
reg <- lm(top_max ~ market, data = returns)
summary(reg)


# Top 100 bonds
reg <- lm(top_100 ~ market, data = returns)
summary(reg)
