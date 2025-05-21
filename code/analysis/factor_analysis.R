



# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)
source("code/builder/portfolio_sort.R")

load("data/bond.RData")





# Single Factor Analysis --------------------------------------------------


single_factor_analysis <- function(portfolios, quantile){
  
  
  # Set colors
  colors = c(colorRampPalette(goodbad)(quantile), space[3])
  
  # Performance Table & graph
  perf.table <- pivot_wider(portfolios |> select(eom, portfolio, return),
                            names_from = portfolio,
                            values_from = return)
  
  # Table
  perf.table <- xts::xts(perf.table[, -1], order.by = perf.table$eom)
  perf.table <- rbind(
    PerformanceAnalytics::Return.annualized(perf.table),
    PerformanceAnalytics::StdDev.annualized(perf.table),
    PerformanceAnalytics::VaR(perf.table) * sqrt(12),
    PerformanceAnalytics::maxDrawdown(perf.table),
    PerformanceAnalytics::SharpeRatio.annualized(perf.table, Rf = 0.02 / 12)
  )
  
  perf.table <- as.data.frame(perf.table) |> 
    mutate(
      stat = rownames(perf.table)
    ) |> 
    select(stat, everything())
  
  rownames(perf.table) <- NULL
  
  # Graph
  portfolios <- portfolios |> 
    arrange(eom) |> 
    group_by(portfolio) |> 
    mutate(
      value = 100 * cumprod(1 + return) / first( 1 + return)
    )
  
  
  perf.plot <- ggplot(portfolios, aes(x = eom, y = value, color = portfolio, group = portfolio)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = colors) +  # Manually set colors
    theme_bw() +
    labs(title = "Portfolio Performance Over Time",
         x = "",
         y = "Value",
         color = "Portfolio")  # Label for the legend
  
  
  
  # Confidence Intervals for Returns
  # Compute means and confidence intervals
  summary_data <- portfolios %>%
    group_by(portfolio) %>%
    summarise(
      mean_return = mean(return, na.rm = TRUE),
      se = sd(return, na.rm = TRUE) / sqrt(n()),  # Standard error
      lower = mean_return - qt(0.975, df = n() - 1) * se,  # 95% CI lower bound
      upper = mean_return + qt(0.975, df = n() - 1) * se   # 95% CI upper bound
    )
  
  return.plot <- ggplot(summary_data, aes(x = portfolio, y = mean_return, color = portfolio)) +
    geom_point(size = 3) +  # Mean as points
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +  # 95% CI as error bars
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +  # Grey reference line at 0
    scale_color_manual(values = colors) +
    theme_bw() +  # Clean theme
    theme(legend.position = "none") +
    labs(title = "Portfolio Returns with 95% CI",
         x = "Portfolio",
         y = "Mean Monthly Return",
         color = "Portfolio")  # Legend label
  
  
  
  # Yield Plot
  yield.plot <- ggplot(portfolios, aes(x = portfolio, y = yield, fill = portfolio)) +
    geom_boxplot(outlier.shape = 3, outlier.size = 1, alpha = 0.9) +  # Boxplot with visible outliers
    scale_fill_manual(values = colors) +
    theme_bw() +  # Clean theme
    theme(legend.position = "none") +  # Remove legend
    labs(title = "Yield Distribution Across Portfolios",
         x = "Portfolio",
         y = "Yield")
  
  
  # Duration Plot
  duration.plot <- ggplot(portfolios, aes(x = portfolio, y = duration, fill = portfolio)) +
    geom_boxplot(outlier.shape = 3, outlier.size = 1, alpha = 0.9) +  # Boxplot with visible outliers
    scale_fill_manual(values = colors) +
    theme_bw() +  # Clean theme
    theme(legend.position = "none") +  # Remove legend
    labs(title = "Duration Distribution Across Portfolios",
         x = "Portfolio",
         y = "Yield")
  
  
  
  
  return(list(
    perf.table = perf.table,
    perf.plot = perf.plot,
    return.plot = return.plot,
    yield.plot = yield.plot,
    duration.plot = duration.plot
  ))
  
  
}








# Test Zone ---------------------------------------------------------------


# yield, amt_out, rating, spread, bond_age, mkt_val, dura

# test <- portfolio_sort(data = bond, signal = yields, ret_col = ret_exc, quantile = 3)
# abc <- single_factor_analysis(test$portfolios, quantile = 3)
# View(abc$perf.table)
# abc$perf.plot
# abc$return.plot
# abc$yield.plot
# abc$duration.plot
# 
# 
# 
# ggplot(data = df, aes(x = eom, y = yield, color = portfolio)) +
#   geom_line(size = 1) +
#   theme_bw()
