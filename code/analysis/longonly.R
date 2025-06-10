

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(xtable)
library(Farben)
library(stargazer)
library(PeerPerformance)

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



print(
  xtable(perf, caption = "Multi-Factor Metrics"),
  file = "results/long_only/performance.txt"
)




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
  labs(title = "Long-Only Strategies compared to the Market Portfolio",
       x = "",
       y = "",
       color = "Strategy") +
  scale_color_manual(
    values = c("market" = "black", "long" = space[1], "top_max" = space[2], "top_100" = space[3]),
    labels = c("market" = "Market Portfolio", "long" = "Long-Only", "top_max" = "Top 1% Long", "top_100" = "Top 100 Long")  # Custom legend labels
  ) +
  theme_bw()


ggsave(
  filename = "results/long_only/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(df, gg, perf, long.vol, market.vol, target_vol, top100.vol, topmax.vol)



# Sharpe Ratios -----------------------------------------------------------

sharpe <- data.frame()

# Long Only
stat <- with(returns, sharpeTesting(long, market))
stat <- as.data.frame(t(unlist(stat)))
stat$name <- "Long Only"
sharpe <- rbind(sharpe, stat)

# Top 1% Bonds
stat <- with(returns, sharpeTesting(top_max, market))
stat <- as.data.frame(t(unlist(stat)))
stat$name <- "Top 1%"
sharpe <- rbind(sharpe, stat)

# Top 100 Bonds
stat <- with(returns, sharpeTesting(top_100, market))
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


# Scale to (%) for interpretability
returns <- returns |> mutate(across(-eom, ~ 100 * .))



# long only
reg1 <- lm(long ~ market, data = returns)


# Top 1% bonds
reg2 <- lm(top_max ~ market, data = returns)


# Top 100 bonds
reg3 <- lm(top_100 ~ market, data = returns)

# Save regression results
stargazer(reg1, reg2, reg3, type = "latex", out = "results/long_only/regression.tex")
