

# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)
library(PerformanceAnalytics)
source("code/builder/portfolio_sort.R")

load("data/bond.RData")
load("data/market.RData")

# market <- read_csv("data/bond_mkt_term.csv",
#                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

# market <- market |> 
#   rename(def := market)

factors <- readLines("data/signals.txt")



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
  
  # initiate data frame
  result <- data.frame()
  
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
      mutate(
        factor = factors[k]
      ) |>
      select(eom, factor, everything()) |> 
      select(-portfolio)
    
    
    # Save dataframe to result list
    result <- rbind(result, df)
    
    cat("Finished factor", factors[k], "\n")
    
  }
  
  
  # Lag returns for all factors
  result <- result |> 
    group_by(factor) |> 
    arrange(eom) |> 
    mutate(
      return = lag(return)
    ) |> 
    ungroup()
  
  
  return(result)
}





# Build -------------------------------------------------------------------


const <- constituents(
  data = bond,
  factors = factors,
  ret_col = ret_exc,
  quantile = 5
  )


fact <- all_factors(bond, factors = factors,
                  type = "ls",
                  quantile = 5,
                  ret_col = ret_exc
                  )


# lag returns

# Prepate market dataframe for merging
market <- market |> 
  mutate(signal = NA) |> 
  rename(market_value = value) |> 
  select(
    eom, factor, return, signal, market_value, yield, duration
  )


# Join Market and Term Factor
data <- rbind(fact, market) |> 
  arrange(eom, factor)


save(const, file = "data/constituents.RData")
save(data, file = "data/factors.RData")
