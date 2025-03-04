

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)
library(PerformanceAnalytics)
source("code/portfolio_sort.R")

load("data/bond.RData")



# Constituents Summarizer -------------------------------------------------


constituents <- function(data, factors, ret_col, quantile = 3){
  
  
  # Initialze result data frame to add afterwards
  result <- data |> arrange(eom, cusip) |>  select(eom, cusip)
  
  for (k in factors) {
    
    # Convert the character string to a symbol
    signal <- sym(k)
    
    # Run your portfolio_sort function on the current signal column.
    res <- portfolio_sort(data, !!signal, {{ ret_col }}, quantile)
    
    result <- left_join(result, res$data, join_by(eom == eom, cusip == cusip))
    
    cat("Finished column", k, "\n")
    
  }
  
  return(result)
  
  
}





# All factor returns ------------------------------------------------------



all_factors <- function(data, factors, type = "long", quantile, ret_col) {
  
  # initiate list
  result <- list()
  
  for (k in seq_along(factors)) {
    
    # Run Portfolio Sort
    df <- portfolio_sort(data, !!rlang::sym(factors[k]), ret_col = {{ret_col}}, quantile = quantile)
    df <- df$portfolios
    
    
    # Keep portfolio depending on type input
    if (type == "long") {
      df <- df |> filter(portfolio == quantile)
    }
    
    if (type == "ls") {
      df <- df |> filter(portfolio == "ls")
    }
    
    
    # keep only return and rename
    df <- df |> 
      select(eom, return) |> 
      rename(
        !!rlang::sym(factors[k]) := return
      )
    
    
    # Save dataframe to result list
    result[[k]] <- df
    
    cat("Finished factor", factors[k], "\n")
    
  }
  
  # Merge all dataframes in list
  result <- Reduce(function(x, y) merge(x, y, by = "eom", all = TRUE), result)
  
  return(result)
}



# Test Zone ---------------------------------------------------------------

# Extract all factor names
# factors <- colnames(bond)[-c(1:9)]
# 
# test <- constituents(
#   data = bond,
#   factors = factors,
#   ret_col = ret_exc,
#   quantile = 3
# )


# Look at correlations
# test <- test |> select(-c(eom, cusip)) |> as.matrix()
# test <- cor(test, method = "spearman", use = "pairwise.complete.obs")
# pdf(file = "results/const_corr.pdf",
#     height = 5,
#     width = 5)
# corrplot::corrplot(test)
# dev.off()



# Extract all factor names
factors <- colnames(bond)[-c(1:9)]
test <- all_factors(bond, factors = factors, type = "ls", quantile = 3, ret_col = ret_exc)


# Performance Summary
perf <- xts(test[, -1], order.by = test$eom)

# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0.02/12)
)


sink("results/factor_metrics.txt")
xtable::xtable(t(perf),
       caption = "Factor Metrics")
sink()


# Performance Plot
value <- test |>
  pivot_longer(!eom, names_to = "factor", values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(factor) |> 
  mutate(
    value = 100 * cumprod(1 + value) / first(1 + value)
  ) |> 
  ungroup()


gg <- ggplot(value, aes(x = eom, y = value, color = factor, group = factor)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(title = "Portfolio Performance Over Time",
       x = "",
       y = "Value",
       color = "Portfolio")  # Label for the legend

ggsave(
  filename = "results/performance.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)


# Standard errors plot

# Reshape data to long format
stats <- test |> 
  pivot_longer(cols = -eom, names_to = "factor", values_to = "excess_return") |> 
  mutate(excess_return = 100 * excess_return) |> 
  group_by(factor) |> 
  summarise(
    mean_return = mean(excess_return, na.rm = TRUE),
    se = sd(excess_return, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  ) |> 
  mutate(
    lower = mean_return - 1.96 * se,  # 95% CI lower bound
    upper = mean_return + 1.96 * se   # 95% CI upper bound
  )


gg <- ggplot(stats, aes(x = factor, y = mean_return)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +  # Error bars
  geom_point(aes(color = factor), size = 4) +  # Colored points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(
    title = "Bond factor replication",
    x = NULL,  # Remove x-axis label
    y = "Monthly Excess Return (%)"
  ) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


ggsave(
  filename = "results/exc_ret.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)
