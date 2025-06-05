


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(zoo)
library(slider)
load("data/factors.RData")
load("data/macro.RData")


# Helper Columns ----------------------------------------------------------

data <- data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    
    # Compute the 3 year rolling volatility
    rollvol = rollapplyr(return, 
                         width = 36, 
                         FUN = function(x) {
                           if(sum(!is.na(x)) < 24) {
                             NA
                           } else {
                             sd(x, na.rm = TRUE)
                           }
                         }, 
                         fill = NA,         
                         align = "right")
  )




# Macro -------------------------------------------------------------------


# Parameters
window_size <- 12
mu <- 0.5
scale <- 10

# change macro data set to monthly observations
macro <- macro |> 
  mutate(
    id = format(date, "%Y-%m")
  ) |> 
  arrange(date) |> 
  group_by(id) |> 
  slice_tail(n=1) |> 
  ungroup() |> 
  select(-date)


# merge the two
data <- data |> 
  mutate(
    id = format(eom, "%Y-%m")
  ) |> 
  left_join(macro, join_by(id == id)) |> 
  select(-id)



get_beta <- function(df, y_col, x_col) {
  n <- nrow(df)
  if (n < 2) return(NA_real_)
  
  y <- df[[y_col]]
  x <- df[[x_col]]
  
  # Lag x by one: align x[1:(n-1)] with y[2:n]
  y_lagged <- y[2:n]
  x_lagged <- x[1:(n-1)]
  
  # Drop NAs (in either y or lagged x)
  keep <- !is.na(y_lagged) & !is.na(x_lagged)
  if (sum(keep) < 2) return(NA_real_)
  
  model <- lm(y_lagged[keep] ~ x_lagged[keep])
  coef(model)[["x_lagged[keep]"]]
}


# Apply rolling regression by group
data <- data |> 
  group_by(factor) |> 
  mutate(
    
    # Inflation
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "cpi"),
      .before = window_size - 1,
      .complete = TRUE),
    
    cpi = cpi * beta * scale + mu,
    
    # Economic Growth
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "gdp"),
      .before = window_size - 1,
      .complete = TRUE),
    
    gdp = gdp * beta * scale + mu,
    
    
    # Federal Funds Rate
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "fed"),
      .before = window_size - 1,
      .complete = TRUE),
    
    fed = fed * beta * scale + mu,
    
    
    # Implied Volatility Index
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "vix"),
      .before = window_size - 1,
      .complete = TRUE),
    
    vix = vix * beta * scale + mu,
    
    # Implied Volatility Index (Bond)
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "tvix"),
      .before = window_size - 1,
      .complete = TRUE),
    
    tvix = tvix * beta * scale + mu,
    
    # Yield Curve Slope
    beta = slide_dbl(
      .x = cur_data(),
      .f = ~ get_beta(.x, y_col = "return", x_col = "slope"),
      .before = window_size - 1,
      .complete = TRUE),
    
    slope = slope * beta * scale + mu
    
  ) |> 
  ungroup() |> 
  select(-beta)



# Momentum ----------------------------------------------------------------

data <- data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    
    # 1 - month 
    mom1 = sign(return),
    smom1 = pmax(pmin(return / rollvol, 2), -2),
    
    
    
    # 3 - month
    mom3 = rollapplyr(return, 
               width = 3, 
               FUN = function(x) {
                 if(sum(!is.na(x)) < 2) {
                   NA
                 } else {
                   mean(x[!is.na(x)])
                 }
               }, 
               fill = NA,         
               align = "right"),
    
    smom3 = pmax(pmin(sqrt(12) * mom3 / rollvol, 2), -2),
    mom3 = sign(mom3),
    
    
    # 6 - month
    mom6 = rollapplyr(return, 
                           width = 6, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 4) {
                               NA
                             } else {
                               mean(x[!is.na(x)])
                             }
                           }, 
                           fill = NA,         
                           align = "right"),
    
    smom6 = pmax(pmin(sqrt(12) * mom6 / rollvol, 2), -2),
    mom6 = sign(mom6),
    
    
    # 12 - month
    mom12 = rollapplyr(return, 
                           width = 12, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 9) {
                               NA
                             } else {
                               mean(x[!is.na(x)])
                             }
                           }, 
                           fill = NA,         
                           align = "right"),
    
    smom12 = pmax(pmin(sqrt(12) * mom12 / rollvol, 2), -2),
    mom12 = sign(mom12),
    
  ) |> 
  ungroup()





# Volatility --------------------------------------------------------------


data <- data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    
    # 12 - month
    vol1 = rollapplyr(rollvol, 
                      width = 12, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 9) {
                          NA
                        } else {
                          mean(x[!is.na(x)]) / x[12]
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    # 6 - month
    vol2 = rollapplyr(rollvol, 
                      width = 6, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 4) {
                          NA
                        } else {
                          mean(x[!is.na(x)]) / x[6]
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    # 3 - month
    vol3 = rollapplyr(rollvol, 
                      width = 3, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 2) {
                          NA
                        } else {
                          mean(x[!is.na(x)]) / x[3]
                        }
                      }, 
                      fill = NA,         
                      align = "right")
    
  )







# Reversal ----------------------------------------------------------------



data <- data |> 
  group_by(factor) |> 
  arrange(eom) |>
  mutate(
    
    
    # 3 - month
    rev1 = rollapplyr(return, 
                      width = 3, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 2) {
                          NA
                        } else {
                          mean(x[!is.na(x)])
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    rev1 = 1 - rev1 * 12,
    
    # 6 - month
    rev2 = rollapplyr(return, 
                      width = 6, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 4) {
                          NA
                        } else {
                          mean(x[!is.na(x)])
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    rev2 = 1 - rev2 * 12,
    
    # 12 - month
    rev3 = rollapplyr(return, 
                      width = 12, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 9) {
                          NA
                        } else {
                          mean(x[!is.na(x)])
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    rev3 = 1 - rev3 * 12
    
    
    
  )





# Characteristics Spread --------------------------------------------------


data <- data |> 
  group_by(factor) |> 
  arrange(eom) |>
  mutate(
    
    # 12 - month
    char1 = rollapplyr(return, 
                       width = 12, 
                       FUN = function(x) {
                         if(sum(!is.na(x)) < 9) {
                           NA
                         } else {
                           (x[12] - mean(x[!is.na(x)])) / sd(x, na.rm = TRUE)
                         }
                       }, 
                       fill = NA,         
                       align = "right"),
    
    # 6 - month
    char2 = rollapplyr(return, 
                       width = 6, 
                       FUN = function(x) {
                         if(sum(!is.na(x)) < 4) {
                           NA
                         } else {
                           (x[6] - mean(x[!is.na(x)])) / sd(x, na.rm = TRUE)
                         }
                       }, 
                       fill = NA,         
                       align = "right"),
    
    # 3 - month
    char3 = rollapplyr(return, 
                       width = 3, 
                       FUN = function(x) {
                         if(sum(!is.na(x)) < 2) {
                           NA
                         } else {
                           (x[3] - mean(x[!is.na(x)])) / sd(x, na.rm = TRUE)
                         }
                       }, 
                       fill = NA,         
                       align = "right")
  )



# PLS ---------------------------------------------------------------------

library(pls)  # Make sure to add this to your libraries section

# Define the signals to use in PLS (excluding aggregated signals and other variables)
pls_signals <- c("cpi", "gdp", "fed", "vix", "slope", "tvix",
                 "mom1", "smom1", "mom3", "smom3", "mom6", "smom6", "mom12", "smom12",
                 "vol1", "vol2", "vol3", 
                 "rev1", "rev2", "rev3",
                 "char1", "char2", "char3")

# Rolling window PLS function to predict return_t+1 with signals_t
fit_pls_rolling <- function(df, signals, target = "return", window = 12) {
  
  X <- df[, signals, drop = FALSE]
  y <- df[[target]]
  n <- nrow(df)
  predictions <- rep(NA, n)
  
  for (i in 6:(n-1)) {  # Start at 6 (half window) and stop at n-1 since we need t+1
    
    # Define training window - use data up to current period
    start_idx <- max(1, i - window + 1)
    train_idx <- start_idx:i
    
    # Training: use signals_t to predict return_t+1
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx + 1]  # Lead the target by 1 period
    
    # Keep rows where future target is available and predictors have some data
    valid_rows <- !is.na(y_train) & apply(X_train, 1, function(x) sum(!is.na(x)) >= 3)
    
    if (sum(valid_rows) < 5) next  # Need minimum training observations (reduced for 12-month window)
    
    X_train <- X_train[valid_rows, , drop = FALSE]
    y_train <- y_train[valid_rows]
    
    # Keep predictors that have at least 40% non-NA values and some variation
    keep_cols <- sapply(X_train, function(x) {
      non_na_pct <- sum(!is.na(x)) / length(x)
      has_variation <- length(unique(x[!is.na(x)])) > 1
      non_na_pct >= 0.4 & has_variation  # Lowered threshold for 12-month window
    })
    
    if (sum(keep_cols) < 2) next  # Need at least 2 predictors
    
    X_train <- X_train[, keep_cols, drop = FALSE]
    
    # Fit PLS model
    tryCatch({
      pls_data <- data.frame(y = y_train, X_train)
      pls_fit <- plsr(y ~ ., 
                      data = pls_data,
                      ncomp = 1,
                      scale = TRUE,
                      validation = "none")
      
      # Predict return_t+1 using current signals_t
      X_current <- X[i, names(X_train), drop = FALSE]
      non_na_count <- sum(!is.na(X_current))
      
      if (non_na_count >= max(1, floor(ncol(X_current) * 0.3))) {
        pred <- predict(pls_fit, newdata = X_current, ncomp = 1)
        predictions[i + 1] <- as.numeric(pred)  # Store prediction for t+1
      }
      
    }, error = function(e) {
      # Continue on error
    })
  }
  
  return(predictions)
}

# Apply PLS to each factor
data <- data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    pls = fit_pls_rolling(cur_data(), pls_signals, "return"),
    pls1 = sign(pls),
    pls2 = pls * scale + mu
  ) |> 
  ungroup() |> 
  select(-pls)


# Averaging ---------------------------------------------------------------


data <- data |> 
  group_by(factor) |> 
  arrange(eom) |>
  mutate(
    
    # Aggregate Macro Signals
    macro = rowMeans(pick(cpi, gdp, fed, vix, slope, tvix)),
    
    # Aggregate Momentum Signals
    mom = rowMeans(pick(mom1, mom3, mom6, mom12, smom1, smom3, smom6, smom12), na.rm = TRUE),
    
    # Aggregate Volatility Signals
    vol = rowMeans(pick(vol1, vol2, vol3), na.rm = TRUE),
    
    # Aggregate Reversal Signals
    rev = rowMeans(pick(rev1, rev2, rev3), na.rm = TRUE),
    
    # Aggregate Characteristics Spread Signals
    char = rowMeans(pick(char1, char2, char3), na.rm = TRUE),
    
    # Aggregate All Signals
    all = rowMeans(pick(macro, mom, vol, rev, char), na.rm = TRUE)
  )





# Shape -------------------------------------------------------------------


# Pivot the data frame longer
long <- data |>
  select(-return, -signal, -market_value, -yield, -duration, -rollvol) |> 
  pivot_longer(cols = !c(eom, factor), names_to = "signal", values_to = "weight")


# Join back the returns
long <- left_join(
  long,
  data |> select(eom, factor, return),
  join_by(eom == eom , factor == factor)
)


# Lag the signals
long <- long |> 
  group_by(factor, signal) |> 
  arrange(eom) |> 
  mutate(
    weight = lag(weight)
  ) |> 
  ungroup()


# Compute the return
long <- long |>
  mutate(
    return = return * weight,
    uret = return / weight
  )


# Categorize the signals
long <- long |> 
  mutate(
    category = case_when(
      signal %in% c("mom1", "smom1", "mom3", "smom3", "mom6", "smom6", "mom12", "smom12", "mom") ~ "momentum",
      signal %in% c("vol1", "vol2", "vol3", "vol4", "vol") ~ "volatility",
      signal %in% c("rev1", "rev2", "rev3", "rev") ~ "reversal",
      signal %in% c("char1", "char2", "char3", "char") ~ "char_spread",
      signal %in% c("cpi", "gdp", "fed", "vix", "tvix", "slope", "macro") ~ "macro",
      signal %in% c("all", "pls1", "pls2") ~ "all"
    )
  )


# Save --------------------------------------------------------------------


data <- long

# Save the data frame
save(data, file = "data/timing.RData")
