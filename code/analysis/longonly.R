

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)

load("data/aggregated_bonds.RData")




# Returns Series ----------------------------------------------------------

# Compute the return of each portfolio
returns <- aggre |> 
  group_by(eom) |> 
  summarize(
    long = sum(flag_long * weight * ret_exc) / sum(flag_long * weight),
    top10 = sum(flag_top10 * weight * ret_exc) / sum(flag_top10 * weight),
    top5 = sum(flag_top5 * weight * ret_exc) / sum(flag_top5 * weight),
    top1 = sum(flag_top1 * weight * ret_exc) / sum(flag_top1 * weight),
    .groups = "drop"
  )


returns <- xts(returns[,-1], order.by = returns$eom)


Return.annualized(returns)
StdDev.annualized(returns)
SharpeRatio.annualized(returns)
maxDrawdown(returns)
