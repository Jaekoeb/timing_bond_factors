

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


# Scale to (%) for interpretability
returns <- returns |> mutate(across(-eom, ~ 100 * .))



# long only
reg1 <- lm(long ~ market, data = returns)


# Top 1% bonds
reg2 <- lm(top_1 ~ market, data = returns)


# Top 100 bonds
reg3 <- lm(top_10 ~ market, data = returns)

# Save regression results
stargazer(reg1, reg2, reg3, type = "latex", out = "results/long_only/regression.tex")

rm(reg1, reg2, reg3, stat)
