


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(sandwich)
library(scales)
library(Farben)
library(broom)

fact <- read_csv("data/bond_factors.csv", 
                 col_types = cols(date = col_date(format = "%Y-%m-%d")))

factors <- c(
  "value", "mkt_val", "amt_out", "dura", "bond_age",
  "btm", "gspread", "yields", "var5", "vola", "es10",
  "def_beta", "term_beta", "var10", "skew", "mom3",
  "mom6", "mom9", "mom12", "mom_equ", "str", "ltr"
)

colnames(fact) <- c("date", factors)


market <- read_csv("data/bond_mkt_term.csv") |> rename(def := market)

fact <- left_join(fact, market, join_by(date == date))


# Compare sample period
fact <- fact |> filter(date >= "2002-08-31")


# Performance -------------------------------------------------------------

# Performance Summary
perf <- xts(fact[, -1], order.by = fact$date)

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


sink("results/cross_section/robustness/factor_performance.txt")
xtable::xtable(perf,
               caption = "Factor Metrics")
sink()


# Performance Plot
value <- fact |>
  pivot_longer(!date, names_to = "factor", values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(factor) |> 
  mutate(
    value = 100 * cumprod(1 + value) / first(1 + value)
  ) |> 
  ungroup()


gg <- ggplot(value, aes(x = date, y = value, color = factor, group = factor)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = colorRampPalette(rainbow)(24)) +
  labs(title = "Portfolio Performance Over Time",
       x = "",
       y = "Value",
       color = "Portfolio")  # Label for the legend

ggsave(
  filename = "results/cross_section/robustness/performance_graph.pdf",
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
  filename = "results/cross_section/robustness/excess_returns.pdf",
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
    p_bonf  = p.adjust(p_raw, method = "bonferroni"),
    p_fdr   = p.adjust(p_raw, method = "BH")
  )

sink("results/cross_section/robustness/exret_alpha.txt")
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
  filename = "results/cross_section/robustness/exret_alphas.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



rm(response_vars, gg)



# Excess Returns ----------------------------------------------------------

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
    y = "Mean Excess Return",
    title = "Monthly Excess Returns with 95% Newey–West CIs"
  )


ggsave(
  filename = "results/cross_section/robustness/excess_returns.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)


rm(gg, res_df)




# Alphas (Multiple Testing) ------------------------------------------------------------------

# 1. define which columns are your dependent vars (exclude date, def, term)
response_vars <- setdiff(names(fact)[-1], c("def", "term"))

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
    p_bonf  = p.adjust(p_raw, method = "bonferroni"),
    p_fdr   = p.adjust(p_raw, method = "BH")
  )

sink("results/cross_section/robustness/exret_alpha.txt")
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
  filename = "results/cross_section/robustness/exret_alphas.pdf",
  plot = gg,
  unit = "cm",
  width = 18,
  height = 12
)



rm(response_vars, res_df, gg)




# Dynamic Alphas ----------------------------------------------------------

source("code/analysis/rolling_alpha.R")
library(runner)



for (name in factors) {
  
  gg <- roll_alpha(
    data = fact,
    response_col_name = name,
    window_size = 30
  ) |> 
    ggplot(aes(x = date)) +
    # Add the confidence interval ribbon first (behind the line)
    geom_ribbon(aes(ymin = low_ci, ymax = high_ci),
                fill = "grey", alpha = 0.5) + # Adjust color and transparency
    # Add the line for the intercept estimate
    geom_line(aes(y = alpha), color = "black", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = 1) +
    labs(
      title = "Rolling Window Regression Intercept (Alpha) with 95% CI",
      subtitle = paste0("Formula: ", name, " ~ term + def ", "Window: 30"),
      x = "Date",
      y = "Rolling Alpha (Intercept)"
    ) +
    theme_bw()
  
  filename <- paste0("results/cross_section/robustness/alpha/", name, ".pdf")
  
  
  ggsave(
    filename = filename,
    plot = gg,
    unit = "cm",
    width = 18,
    height = 12
  )
  
}



