

# Libraries and Data ------------------------------------------------------

library(tidyverse)

load("data/aggregated_bonds.RData")
load("data/bond.RData")

# Keep only relevant columns
bond <- bond |> select(eom, cusip, ret_exc)
aggre <- aggre |>
  select(eom, cusip, flag_long) |> 
  filter(flag_long == 1)

# Left Join
data <- left_join(bond, aggre, join_by(eom == eom, cusip == cusip))

data <- data |>
  mutate(
    flag_long = ifelse(is.na(flag_long), 0, 1)
    )

# Added lagged long flag
data <- data |> 
  group_by(cusip) |> 
  arrange(eom) |> 
  mutate(flag_long_lag = lag(flag_long)) |> 
  ungroup()


rm(aggre, bond)

# -------------------------------------------------------------------------



# Remain, Sold, Bought ----------------------------------------------------


data <- data |> 
  mutate(
    status = case_when(
      flag_long == 1 & flag_long_lag == 1 ~ "remain",
      flag_long == 1 & flag_long_lag == 0 ~ "bought",
      flag_long == 0 & flag_long_lag == 1 ~ "sold",
      .default = NA
      
    )
  )




# Helper Function: Create Binary Column -----------------------------------



restricted_trading <- function(data, probability){
  
  
  # Restrict Trading
  df <- data |> 
    mutate(
      col = case_when(
        status == "remain" ~ 1,
        status == "sold" ~ rbinom(n(), 1, 1 - probability),
        status == "bought" ~ rbinom(n(), 1, probability),
        .default = 0
      )
    )
  
  # Compute aggregate returns
  return <- df |> 
    filter(col == 1) |> 
    group_by(eom) |>
    summarise(
      return = mean(ret_exc, na.rm = TRUE)
    ) |> 
    select(return) |> 
    pull() |> 
    mean()
  
  
  
  
  return(return)
  
}




# Run Simluations ---------------------------------------------------------


n_sim <- 20

probabilities <- runif(n_sim)


df <- data.frame(
  restriction = numeric(),
  return = numeric()
)

for (p in probabilities) {
  
  result <- data.frame(restriction = p, return = restricted_trading(data, p))
  
  df <- rbind(df, result)
  
}


rm(n_sim, probabilities, p, result)


# Analysis ----------------------------------------------------------------



ggplot(data = df, aes(x = restriction, y = 100 * return)) +
  geom_point(color = "black", alpha = 1, size = 1.5) + # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.7) + # Linear regression line
  theme_bw() + # Black and white theme
  labs(title = "",
       x = "",
       y = "Monthly Return (in %)")



test <- data |> group_by(status) |> summarise(value = mean(ret_exc, na.rm = TRUE))
