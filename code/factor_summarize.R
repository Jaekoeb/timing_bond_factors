

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)
library(PerformanceAnalytics)
source("code/portfolio_sort.R")

load("data/bond.RData")

market <- read_csv("data/bond_mkt_term.csv", 
                   col_types = cols(date = col_date(format = "%Y-%m-%d")))



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



# Correlations ------------------------------------------------------------


factors <- colnames(bond)[-c(1:9)]

test <- constituents(
  data = bond,
  factors = factors,
  ret_col = ret_exc,
  quantile = 3
)


# Look at correlations
test <- test |> select(-c(eom, cusip)) |> as.matrix()
test <- cor(test, method = "spearman", use = "pairwise.complete.obs")
pdf(file = "results/corr_ls.pdf",
    height = 5,
    width = 5)
corrplot::corrplot(test)
dev.off()



# Long Short --------------------------------------------------------------


# Extract all factor names
factors <- colnames(bond)[-c(1:9)]
test <- all_factors(bond, factors = factors, type = "ls", quantile = 3, ret_col = ret_exc)



## Performance -------------------------------------------------------------


# Performance Summary
perf <- xts(test[, -1], order.by = test$eom)

# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0)
)

perf <- t(perf)
perf <- perf[order(perf[, 1], decreasing = TRUE), ]

sink("results/factor_metrics_ls.txt")
xtable::xtable(perf,
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
  filename = "results/performance_ls.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



## Excess Returns ----------------------------------------------------------

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
  geom_point(aes(fill = factor), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(
    title = "Bond factor replication: Excess Return",
    x = NULL,  # Remove x-axis label
    y = "Monthly Excess Return (%)"
  ) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


ggsave(
  filename = "results/exc_ret_ls.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)




## Alphas ------------------------------------------------------------------

# merge market data
test <- left_join(test, market, join_by(eom == date))


# Run regressions and extract the intercept and its standard error
stats <- lapply(factors, function(dep) {
  # Build the regression formula for the current dependent variable
  formula <- as.formula(paste(dep, "~ market + term"))
  
  # Fit the linear model
  model <- lm(formula, data = test)
  
  # Extract the summary coefficients
  coefs <- summary(model)$coefficients
  
  # Get the intercept (alpha) and its standard error
  alpha <- coefs["(Intercept)", "Estimate"] * 100
  alpha_se <- coefs["(Intercept)", "Std. Error"] * 100
  
  # Return a named vector with the results
  c(alpha = alpha, se = alpha_se)
})

# Combine the results into a data frame for clarity
stats <- do.call(rbind, stats)
stats <- stats |> 
  as.data.frame() |> 
  mutate(
    factor = factors,
    lower = alpha - 1.96 * se,  # 95% CI lower bound
    upper = alpha + 1.96 * se   # 95% CI upper bound
    
  )



gg <- ggplot(stats, aes(x = factor, y = alpha)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +  # Error bars
  geom_point(aes(fill = factor), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(
    title = "Bond factor replication: Alpha",
    x = NULL,  # Remove x-axis label
    y = "Alpha in %"
  ) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


ggsave(
  filename = "results/alpha_ls.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



# Long Only  --------------------------------------------------------------


# Extract all factor names
factors <- colnames(bond)[-c(1:9)]
test <- all_factors(bond, factors = factors, type = "long", quantile = 5, ret_col = ret_exc)



## Performance -------------------------------------------------------------


# Performance Summary
perf <- xts(test[, -1], order.by = test$eom)

# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0)
)

perf <- t(perf)
perf <- perf[order(perf[, 1], decreasing = TRUE), ]

sink("results/factor_metrics_long.txt")
xtable::xtable(perf,
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
  filename = "results/performance_long.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



## Excess Returns ----------------------------------------------------------

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
  geom_point(aes(fill = factor), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(
    title = "Bond factor replication: Excess Return",
    x = NULL,  # Remove x-axis label
    y = "Monthly Excess Return (%)"
  ) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


ggsave(
  filename = "results/exc_ret_long.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)




## Alphas ------------------------------------------------------------------

# merge market data
test <- left_join(test, market, join_by(eom == date))


# Run regressions and extract the intercept and its standard error
stats <- lapply(factors, function(dep) {
  # Build the regression formula for the current dependent variable
  formula <- as.formula(paste(dep, "~ market + term"))
  
  # Fit the linear model
  model <- lm(formula, data = test)
  
  # Extract the summary coefficients
  coefs <- summary(model)$coefficients
  
  # Get the intercept (alpha) and its standard error
  alpha <- coefs["(Intercept)", "Estimate"] * 100
  alpha_se <- coefs["(Intercept)", "Std. Error"] * 100
  
  # Return a named vector with the results
  c(alpha = alpha, se = alpha_se)
})

# Combine the results into a data frame for clarity
stats <- do.call(rbind, stats)
stats <- stats |> 
  as.data.frame() |> 
  mutate(
    factor = factors,
    lower = alpha - 1.96 * se,  # 95% CI lower bound
    upper = alpha + 1.96 * se   # 95% CI upper bound
    
  )



gg <- ggplot(stats, aes(x = factor, y = alpha)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +  # Error bars
  geom_point(aes(fill = factor), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Dashed line at y = 0
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(12)) +
  labs(
    title = "Bond factor replication: Alpha",
    x = NULL,  # Remove x-axis label
    y = "Alpha in %"
  ) +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


ggsave(
  filename = "results/alpha_long.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)
