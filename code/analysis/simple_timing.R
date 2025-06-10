



# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(corrplot)
library(pheatmap)
library(Farben)

load("data/timing.RData")


# Aggregate by timing signal
signals <- data |> 
  group_by(eom, signal) |> 
  summarize(
    weight = mean(weight, na.rm = TRUE),
    return = mean(return, na.rm = TRUE),
    uret = mean(uret, na.rm = TRUE),
    category = first(category),
    .groups = "drop"
  ) |> 
  mutate(
    across(-c(eom, signal, category), ~ na_if(., NaN))
  )


# Set colors for all plots
colors = c(
  "all" = "grey",
  "char_spread" = basic[1],
  "macro" = basic[2],
  "momentum" = basic[3],
  "reversal" = basic[4],
  "volatility" = basic[5]
)

# Correlation -------------------------------------------------------------


df <- data |>
  select(eom, factor, signal, weight) |> 
  pivot_wider(names_from = signal, values_from = weight) |> 
  select(-c(eom, factor)) |> 
  as.matrix()

# 1. Compute Kendall's Tau Correlation Matrix
# Use method = "spearman" for Spearman's Rho
# Use use = "pairwise.complete.obs" for pairwise deletion of NAs
cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")


pdf(
  file = "results/timing/correlation.pdf",
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
         title = "Familiarity between Timing Signals",
         mar = c(0,0,1,0)        # Adjust plot margins c(bottom, left, top, right)
)


dev.off()

rm(df, cor_matrix)




# Weights -----------------------------------------------------------------


# Calculate summary statistics per signal
summary_df <- signals %>%
  group_by(signal, category) %>%
  summarise(
    mean_weight = mean(weight, na.rm = TRUE),
    lower_95 = quantile(weight, 0.05, na.rm = TRUE),
    upper_95 = quantile(weight, 0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Sort signals by mean weight in descending order
  arrange(desc(mean_weight)) %>%
  # Convert signal back to factor with the new order for plotting
  mutate(signal = factor(signal, levels = unique(signal)))

# Create the plot with adjustments
gg <- ggplot(summary_df, aes(x = signal, y = mean_weight, color = category, fill = category)) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2, linewidth = 0.7, color = "black") + # Thicker black error bars
  geom_point(shape = 21, size = 3, stroke = 1, color = "black") + # Thicker point with black outline and colored fill
  labs(
    x = "Signal",
    y = "Weight",
    color = "Category",
    fill = "Category", # Added fill to labs for legend
    title = "Mean Weight per Signal with 5th and 95th Percentiles"
  ) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels


ggsave(
  plot = gg,
  file = "results/timing/weights.pdf",
  units = "cm",
  width = 20,
  height = 12
)

rm(gg, summary_df)



# Excess Return -----------------------------------------------------------------


# Calculate summary statistics per signal
summary_df <- signals %>%
  mutate(excret = 100*(return - uret)) |> 
  group_by(signal, category) %>%
  summarise(
    mean_return = mean(excret, na.rm = TRUE),
    lower = quantile(excret, 0.25, na.rm = TRUE),
    upper = quantile(excret, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Sort signals by mean weight in descending order
  arrange(desc(mean_return)) %>%
  # Convert signal back to factor with the new order for plotting
  mutate(signal = factor(signal, levels = unique(signal)))

# Create the plot with adjustments
gg <- ggplot(summary_df, aes(x = signal, y = mean_return, color = category, fill = category)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.7, color = "black") + # Thicker black error bars
  geom_point(shape = 21, size = 3, stroke = 1, color = "black") + # Thicker point with black outline and colored fill
  labs(
    x = "Signal",
    y = "Excess Return",
    color = "Category",
    fill = "Category", # Added fill to labs for legend
    title = "Mean Excess Return against untimed per Signal with 25th and 75th Percentiles"
  ) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels


ggsave(
  plot = gg,
  file = "results/timing/exc_ret.pdf",
  units = "cm",
  width = 20,
  height = 12
)

rm(gg, summary_df)


# Sharpe Ratio ------------------------------------------------------------


sharpe <- data |>
  group_by(factor, signal) |>
  summarise(
    sharpe = 12 * mean(return, na.rm = TRUE) / sd(return, na.rm = TRUE),
    usharpe = 12 * mean(uret, na.rm = TRUE) / sd(uret, na.rm = TRUE),
    diff = sharpe - usharpe,
    category = first(category),
    .groups = "drop"
  )


df_summary <- sharpe |> 
  group_by(signal) |> 
  summarise(
    sharpe = mean(sharpe),
    usharpe = mean(usharpe),
    diff = mean(diff),
    category = first(category)
  )



gg <- ggplot(data = df_summary,
       aes(x = reorder(signal, -diff), y = diff, group = category, color = category, fill = category)) +
  geom_col(color = "black") + # Add black outline and use fill for color
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Tilt x-axis labels
  labs(
    x = "Signal",
    y = expression(Delta * SR),
    color = "Category",
    fill = "Category",
    title = "Average Difference in Sharpe Ratio between Timed vs Untimed" # Example title
  )


# Save plot
ggsave(
  plot = gg,
  file = "results/timing/sharpe.pdf",
  units = "cm",
  width = 20,
  height = 12
)


# 1) Compute a wide matrix
mat <- sharpe %>%
  select(factor, signal, diff) |> 
  pivot_wider(
    names_from  = signal,
    values_from = diff,
    values_fill = 0
  ) %>%
  column_to_rownames("factor") %>%
  as.matrix()

# 2) Hierarchical clustering
#  – on rows (factors)
row_hc <- hclust(dist(mat), method = "ward.D2")
row_ord <- row_hc$labels[row_hc$order]
#  – on columns (signals)
col_hc <- hclust(dist(t(mat)), method = "ward.D2")
col_ord <- col_hc$labels[col_hc$order]

# 3) Pivot back into long format, but with ordered factor levels
df_long <- mat %>%
  as.data.frame() %>%
  rownames_to_column("factor") %>%
  pivot_longer(
    -factor,
    names_to  = "signal",
    values_to = "alpha"
  ) %>%
  mutate(
    factor = factor(factor, levels = row_ord),
    signal = factor(signal, levels = col_ord)
  )

# 4) Plot with ggplot2
gg <- ggplot(df_long, aes(x = signal, y = factor, fill = alpha)) +
  geom_tile(color = "white", size = 0.3) +
  # diverging palette centered at zero
  scale_fill_gradient2(
    low    = "darkred",
    mid    = "grey80",
    high   = "darkgreen",
    midpoint = 0,
    name   = expression(Delta * SR)
  ) +
  labs(
    x = "Signal", y = "Factor",
    title = "Difference in Sharpe Ratio between Timed vs Untimed for each pair"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )


# Save plot
ggsave(
  plot = gg,
  file = "results/timing/sharpe_heatmap.pdf",
  units = "cm",
  width = 20,
  height = 12
)




# Alpha -------------------------------------------------------------------


# Compute Alphas
alpha <- data %>%
  group_by(factor, signal) %>%
  summarise(
    alpha = coef(lm(return ~ uret))[1],
    t_stat = coef(summary(lm(return ~ uret)))[1, 3],
    n_obs = n(),
    significance = case_when(
      n_obs > 2 & abs(t_stat) > qt(0.975, n_obs - 2) & alpha > 0 ~ 1,
      n_obs > 2 & abs(t_stat) > qt(0.975, n_obs - 2) & alpha < 0 ~ -1,
      TRUE ~ 0),
    category = first(category),
    .groups = 'drop'
  )


# Summary of Alphas by Signal
summary_df <- alpha |> 
  group_by(signal, category) |> 
  summarise(
    mean_alpha = 100 * mean(alpha, na.rm = TRUE),
    lower = 100 * quantile(alpha, 0.05, na.rm = TRUE),
    upper = 100 * quantile(alpha, 0.95, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Sort signals by mean weight in descending order
  arrange(desc(mean_alpha)) %>%
  # Convert signal back to factor with the new order for plotting
  mutate(signal = factor(signal, levels = unique(signal)))



# Create the plot with adjustments
gg <- ggplot(summary_df, aes(x = signal, y = mean_alpha, color = category, fill = category)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.7, color = "black") + # Thicker black error bars
  geom_point(shape = 21, size = 3, stroke = 1, color = "black") + # Thicker point with black outline and colored fill
  labs(
    x = "Signal",
    y = "Alpha",
    color = "Category",
    fill = "Category", # Added fill to labs for legend
    title = "Mean Alpha against untimed per Signal with 5th and 95th Percentiles"
  ) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

# Save plot
ggsave(
  plot = gg,
  file = "results/timing/alpha.pdf",
  units = "cm",
  width = 20,
  height = 12
)


# Save table
summary_df <- summary_df |> 
  mutate(
    sort_key = ifelse(grepl("^[^0-9]+$", signal), 0, 1),
    category = as.character(category),
    signal = as.character(signal)
  ) %>%
  arrange(category, sort_key, signal) %>%
  mutate(
    category = as.factor(category),
    signal = as.factor(signal)
  ) %>%
  select(-sort_key)


colnames(summary_df) <- c("Signal", "Category", "Estimate", "Lower", "Upper")

sink("results/timing/alpha.txt")
xtable::xtable(summary_df,
               caption = "Signal Alpha Metrics")
sink()



# Statistical Significance of Alphas
df <- alpha |>
  select(signal, significance, category) |> 
  mutate(
    significance = case_when(
      significance == -1 ~ "Negatively Significant",
      significance == 0 ~ "Not Significant",
      significance == 1 ~ "Positively Significant"
    ),
    significance = as.factor(significance)
  )


gg <- df |>
  group_by(signal, significance) |>
  summarise(n = n(), .groups = 'drop') |>
  group_by(signal) |>
  mutate(percentage = 100 * n / sum(n)) |>
  ggplot(aes(x = signal, y = percentage, fill = significance)) +
  geom_col() +
  scale_fill_manual(values = c("Positively Significant" = "darkgreen", "Negatively Significant" = "darkred", "Not Significant" = "grey")) +
  labs(title = "Percentage of significant Alphas for each Signal",
       y = "", x = "Signal", fill = "", color = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save plot
ggsave(
  plot = gg,
  file = "results/timing/alpha_significance.pdf",
  units = "cm",
  width = 20,
  height = 12
)



rm(gg, summary_df, df)

