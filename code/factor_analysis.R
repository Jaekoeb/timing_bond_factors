



# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)
source("code/portfolio_sort.R")

load("data/bond.RData")



# Constituents Summarizer -------------------------------------------------


constituents <- function(data, signals, ret_col, quantile = 3){
  
  
  # Initialze result data frame to add afterwards
  result <- data |> arrange(eom, cusip) |>  select(eom, cusip)
  
  for (k in signals) {
    
    # Convert the character string to a symbol
    signal <- sym(k)
    
    # Run your portfolio_sort function on the current signal column.
    res <- portfolio_sort(data, !!signal, {{ ret_col }}, quantile)
    
    result <- left_join(result, res$data, join_by(eom == eom, cusip == cusip))
    
    cat("Finished column", k, "\n")
    
  }
  
  return(result)
  
  
}




# Extract all factor names
# factors <- colnames(bond)[-c(1:9)]
# 
# test <- constituents(
#   data = bond,
#   signals = factors,
#   ret_col = ret_exc,
#   quantile = 3
# )
# 

# Look at correlations
# test <- test |> select(-c(eom, cusip)) |> as.matrix()
# test <- cor(test, method = "spearman", use = "pairwise.complete.obs")
# pdf(file = "results/const_corr.pdf",
#     height = 5,
#     width = 5)
# corrplot::corrplot(test)
# dev.off()





# Single Factor Analysis --------------------------------------------------


single_factor_analysis <- function(portfolios, quantile){
  
  
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
    scale_color_manual(values = c(colorRampPalette(goodbad)(quantile), space[3])) +  # Manually set colors
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
    scale_color_manual(values = c(colorRampPalette(goodbad)(5), space[3])) +
    theme_bw() +  # Clean theme
    theme(legend.position = "none") +
    labs(title = "Portfolio Returns with 95% CI",
         x = "Portfolio",
         y = "Mean Monthly Return",
         color = "Portfolio")  # Legend label
  
  
  
  
  return(list(
    perf.table = perf.table,
    perf.plot = perf.plot,
    return.plot = return.plot
  ))
  
  
}



# 
# test <- portfolio_sort(bond, vola, ret_exc, quantile = 5)
# View(test$portfolios)
# 
# abc <- single_factor_analysis(test$portfolios, quantile = 5)
# abc$perf.table
