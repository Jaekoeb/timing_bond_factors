


# Libraries and Data ------------------------------------------------------

load("data/constituents.RData")
load("data/factors.RData")
factors <- readLines("data/signals.txt")

library(tidyverse)
library(corrplot)
library(xts)
library(PerformanceAnalytics)
library(sandwich)
library(scales)
library(broom)
library(Farben)

# Constituents Familiarity ------------------------------------------------

df <- const |> select(-eom, -cusip)

# 1. Compute Kendall's Tau Correlation Matrix
# Use method = "spearman" for Spearman's Rho
# Use use = "pairwise.complete.obs" for pairwise deletion of NAs
cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")


pdf(
  file = "results/cross_section/correlation.pdf",
  height = 5,
  width = 5
)

# Generate the plot
corrplot(cor_matrix,
         method = "color",       # Use color to represent correlation strength
         order = "hclust",       # Order rows/columns using hierarchical clustering
         tl.col = "black",       # Color of text labels
         tl.srt = 45,            # Rotate text labels for better readability
         tl.cex = 0.7,           # Size of text labels (adjust as needed)
         col = COL2('RdYlBu'),    # Use a Red-Yellow-Blue color palette (good for correlations)
         title = "Familiarity between Factors",
         mar = c(0,0,1,0)        # Adjust plot margins c(bottom, left, top, right)
)


dev.off()

rm(df, cor_matrix)




# Factor Characteristics --------------------------------------------------


# Yield
gg <- data %>%
  # Convert 'factor' to a factor type if it isn't already
  mutate(factor = factor(factor), yield = yield * 100) %>%
  # Reorder factors based on median yield (largest to smallest)
  # We use -yield inside reorder for descending order based on the summary function (default is median)
  ggplot(aes(x = reorder(factor, yield, FUN = function(x) -median(x)), y = yield)) +
  geom_boxplot(outliers = FALSE) + # Changed outliers = FALSE to outlier.shape = NA for clarity
  theme_bw() +
  # Tilt x-axis labels by 45 degrees and adjust alignment
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  # Add custom labels
  labs(
    title = "Yield Distribution by Factor",
    x = "",
    y = "Yield (%)"
  )


ggsave(
  filename = "results/cross_section/yield_distribution.pdf",
  plot = gg,
  unit = "cm",
  width = 20,
  height = 10
)



# Duration
gg <- data %>%
  # Convert 'factor' to a factor type if it isn't already
  mutate(factor = factor(factor)) %>%
  # Reorder factors based on median yield (largest to smallest)
  # We use -yield inside reorder for descending order based on the summary function (default is median)
  ggplot(aes(x = reorder(factor, duration, FUN = function(x) -median(x)), y = duration)) +
  geom_boxplot(outliers = FALSE) + # Changed outliers = FALSE to outlier.shape = NA for clarity
  theme_bw() +
  # Tilt x-axis labels by 45 degrees and adjust alignment
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  # Add custom labels
  labs(
    title = "Duration Distribution by Factor",
    x = "",
    y = "Duration"
  )


ggsave(
  filename = "results/cross_section/duration_distribution.pdf",
  plot = gg,
  unit = "cm",
  width = 20,
  height = 10
)

# Prepare Return Data Frame -----------------------------------------------

fact <- data |> 
  select(eom, factor, return) |> 
  pivot_wider(names_from = factor, values_from = return)

# Performance -------------------------------------------------------------

# Performance Summary
perf <- xts(fact[, -1], order.by = fact$eom)

# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf) * 100,
  StdDev.annualized(perf) * 100,
  maxDrawdown(perf) * 100,
  SharpeRatio.annualized(perf, Rf = 0)
)

# Transpose
perf <- t(perf)

# Change column names
colnames(perf) <- c("Ann. Return", "Ann. Volatility", "Worst Drawdown", "Sharpe Ratio")


sink("results/cross_section/factor_performance.txt")
xtable::xtable(perf,
               caption = "Factor Metrics")
sink()


# Performance Plot
value <- fact |>
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
  scale_color_manual(values = colorRampPalette(rainbow)(24)) +
  labs(title = "Portfolio Performance Over Time",
       x = "",
       y = "Value",
       color = "Portfolio")  # Label for the legend

ggsave(
  filename = "results/cross_section/performance_graph.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)

rm(value, perf, gg)




# Excess Returns ----------------------------------------------------------

# assume `fact` is your data.frame, date in col 1 and factors in col 2:24
factors <- names(fact)[-1]

# Loop over each factor, fit lm(~1), get NW‐CI on intercept
res_df <- map_dfr(factors, function(f) {
  # fit constant-only model
  mod <- lm(reformulate(termlabels = "1", response = f), data = fact)
  
  # tidy it, asking for Newey–West vcov and conf.int
  tidy(mod,
       conf.int  = TRUE,
       conf.level = 0.95,
       vcov.     = function(x) NeweyWest(x, lag = NULL, prewhite = FALSE)
  ) %>%
    filter(term == "(Intercept)") %>%
    transmute(
      Factor = f,
      Estimate = estimate,
      Lower    = conf.low,
      Upper    = conf.high
    )
})




gg <- ggplot(res_df, aes(x = Factor, y = Estimate)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_point(shape = 21, fill = "grey", color = "black", size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "Excess Return",
    title = "Monthly Excess Returns with 95% Newey–West CIs"
  )


ggsave(
  filename = "results/cross_section/excess_returns.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)


rm(gg, res_df)




# Alphas (Multiple Testing) ------------------------------------------------------------------

# 1. define which columns are your dependent vars (exclude date, def, term)
response_vars <- setdiff(names(fact)[-1], c("def", "term", "market"))

# 2. loop, fit y ~ def + term, extract alpha & CIs
res_df <- map_dfr(response_vars, function(f) {
  form <- reformulate(c("def", "term"), response = f)
  mod  <- lm(form, data = fact)
  
  tidy(mod,
       conf.int   = TRUE,
       conf.level = 0.95,
       vcov.      = function(x) NeweyWest(x, lag = NULL, prewhite = FALSE)
  ) %>%
    filter(term == "(Intercept)") %>%
    transmute(
      Factor   = f,
      Estimate = estimate * 100,
      Lower    = conf.low  * 100,
      Upper    = conf.high * 100,
      p_raw    = p.value
    )
})


# 3. adjust for multiple testing
res_df <- res_df |> 
  mutate(
    p_fdr   = p.adjust(p_raw, method = "BH")
  )

sink("results/cross_section/exret_alpha.txt")
xtable::xtable(res_df,
               caption = "Factor Metrics")
sink()



# 4. Plot
gg <- ggplot(res_df, aes(x = Factor, y = Estimate)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_point(shape = 21, fill = "grey", color = "black", size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = 1) +
  scale_y_continuous(labels = number_format(suffix = "%", accuracy = 0.1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x     = NULL,
    y     = "Intercept (α) in %",
    title = "Estimated α from  y ~ def + term  with 95% Newey–West CIs"
  )


# 5. Save
ggsave(
  filename = "results/cross_section/exret_alphas.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



rm(response_vars, gg)


# Dynamic Alphas ----------------------------------------------------------

source("code/analysis/rolling_alpha.R")
library(runner)



for (name in factors) {
  
  # Perform rolling regression
  df <- roll_alpha(
    data = fact,
    response_col_name = name,
    window_size = 24
  )
  
  # Remove all NAs
  df <- na.omit(df)
  
  # Scale to %
  df <- df |> mutate(across(c(alpha, low_ci, high_ci), ~100*.))
  
  gg <- df |> 
    ggplot(aes(x = eom)) +
    # Add the confidence interval ribbon first (behind the line)
    geom_ribbon(aes(ymin = low_ci, ymax = high_ci),
                fill = "grey", alpha = 0.5) + # Adjust color and transparency
    # Add the line for the intercept estimate
    geom_line(aes(y = alpha), color = "black", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = 1) +
    labs(
      title = "Rolling Window Regression Intercept (Alpha) with 95% CI",
      subtitle = paste0("Formula: ", name, " ~ term + def ", "Window: 24"),
      x = "",
      y = "Rolling Alpha (in %)"
    ) +
    theme_bw()
  
  filename <- paste0("results/cross_section/alpha/", name, ".pdf")
  
  
  ggsave(
    filename = filename,
    plot = gg,
    unit = "cm",
    width = 18,
    height = 12
  )
  
}



