


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)

load("data/timing.RData")
load("data/market.RData")

# filter only for the "all" signal
data <- data |>
  filter(signal == "all") |>
  select(-signal, -category) |> 
  arrange(eom)


# remove the market, def and term factors
data <- data |> 
  filter(!(factor %in% c("market", "def", "term")))




# Multi Factor Builder ----------------------------------------------------


# Parameter for number of quantile
quant <- 5

port <- data |> 
  group_by(eom) |> 
  mutate(
    sort = ntile(weight, quant)
  )

# Save the member of the multi-factor portfolio
member <- port |> 
  filter(sort == quant) |> 
  select(eom, factor, weight, sort)


# check <- test |> 
#   filter(!is.na(sort)) |> 
#   group_by(factor) |> 
#   summarize(
#     one = sum(sort == 1),
#     two = sum(sort == 2),
#     three = sum(sort == 3),
#     four = sum(sort == 4),
#     five = sum(sort == 5),
#     .groups = "drop"
#   )


port <- port |> 
  group_by(eom) |> 
  summarize(
    port = mean(ifelse(sort == 5, uret, NA), na.rm = TRUE),
    bench = mean(uret, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  filter(!is.nan(port), !is.na(bench))



# Performance -------------------------------------------------------------



perf <- xts(port[,-1], order.by = port$eom)


# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0)
)


# Change column names
rownames(perf) <- c("Ann. Return", "Ann. Volatility", "Worst Drawdown", "Sharpe Ratio")

# Add difference column
perf <- cbind(perf, "diff" = perf[,1] - perf[,2])

sink("results/multi_timing/performance.txt")
xtable::xtable(perf,
               caption = "Multi-Factor Metrics")
sink()



# Scale the return series to match in volatility
target_vol <- 0.05
port.vol <- sd(port$port) * sqrt(12)
bench.vol <- sd(port$bench) * sqrt(12)

df <- port |> 
  mutate(
    port = port * target_vol / port.vol,
    port = 100 * cumprod(1+port) / first(1+port),
    bench = bench * target_vol / bench.vol,
    bench = 100 * cumprod(1+bench) / first(1+bench)
  ) |> 
  select(eom, port, bench) |> 
  pivot_longer(!eom, names_to = "strategy", values_to = "value")


gg <- ggplot(data = df, aes(x = eom, y = value, group = strategy, color = strategy)) +
  geom_line(size = 1) +
  labs(title = "Portfolio Performance Over Time",
       x = "",
       y = "Value",
       color = "Portfolio") +
  theme_bw()

ggsave(
  filename = "results/multi_timing/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(df, gg, perf, bench.vol, port.vol, target_vol)





# Regressions -------------------------------------------------------------


# Prepare Market Data
market <- market |> 
  select(eom, factor, return) |> 
  filter(factor != "market") |> 
  pivot_wider(names_from = factor, values_from = return)


# Left Join with portfolio
port <- left_join(port, market, join_by(eom == eom))



# Regression against benchmark portfolio
reg <- lm(data = port, port ~ bench)
summary(reg)

# Regression against default and term factor
reg <- lm(data = port, port ~ def + term)
summary(reg)

# Regression of Benchmark against default and term factor
reg <- lm(data = port, bench ~ def + term)
summary(reg)






