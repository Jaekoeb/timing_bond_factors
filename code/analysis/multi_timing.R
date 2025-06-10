


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(stargazer)
library(xtable)
library(PeerPerformance)

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
  ) |> 
  ungroup()

# Save the member of the multi-factor portfolio
member <- port |> 
  filter(sort == quant) |> 
  select(eom, factor, weight, sort)


save(member, file = "data/member.RData")

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

print(
  xtable(perf, caption = "Multi-Factor Metrics"),
  file = "results/multi_timing/performance.txt"
)



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
  rename(
    `Multi-Timing` = port,
    Benchmark = bench
  )


# Base plot with ribbon
gg <- ggplot(df, aes(x = eom)) +
  geom_ribbon(aes(ymin = pmin(Benchmark, `Multi-Timing`), ymax = pmax(Benchmark, `Multi-Timing`)),
              fill = "grey70", alpha = 0.4) +
  geom_line(aes(y = Benchmark, color = "Benchmark"), linewidth = 1) +
  geom_line(aes(y = `Multi-Timing`, color = "Multi-Timing"), linewidth = 1) +
  scale_color_manual(values = c("Benchmark" = space[3], "Multi-Timing" = space[1])) +
  labs(title = "Performance Comparison of Timed vs Untimed Factor Portfolio",
       subtitle = "Both portfolios scaled to an annual volatility of 5%",
       x = "",
       y = "",
       color = "Strategy") +
  theme_bw()


ggsave(
  filename = "results/multi_timing/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(df, gg, perf, bench.vol, port.vol, target_vol)




# Sharpe Ratios -----------------------------------------------------------


sharpe <- with(port, sharpeTesting(port, bench))
sharpe <- as.data.frame(t(unlist(sharpe)))
colnames(sharpe) <- c("Obs.", "Monthly Timed SR", "Monthly Untimed SR", "Difference", "T-Stat", "P-Value")


print(
  xtable(sharpe, caption = "Multi-Factor Sharpe Ratios"),
  file = "results/multi_timing/sharpe.txt"
)

# Regressions -------------------------------------------------------------


# Prepare Market Data
market <- market |> 
  select(eom, factor, return) |> 
  filter(factor != "market") |> 
  pivot_wider(names_from = factor, values_from = return)


# Left Join with portfolio
port <- left_join(port, market, join_by(eom == eom))

# Scale to % for interpretability
port <- port |> 
  mutate(across(-eom, ~100*.))


# Regression against benchmark portfolio
reg <- lm(data = port, port ~ bench)
stargazer(reg, type = "latex", out = "results/multi_timing/benchmark_regression.tex")


# Regression against default and term factor
reg1 <- lm(data = port, port ~ def + term)

# Regression of Benchmark against default and term factor
reg2 <- lm(data = port, bench ~ def + term)

stargazer(reg1, reg2, type = "latex", out = "results/multi_timing/defterm_regression.tex")





