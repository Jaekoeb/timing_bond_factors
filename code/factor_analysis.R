



# Libraries and Data ------------------------------------------------------

library(tidyverse)
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
# test <- test |> select(-c(eom, cusip)) |> as.matrix()
# test <- cor(test, method = "spearman", use = "pairwise.complete.obs")
# pdf(file = "results/const_corr.pdf",
#     height = 5,
#     width = 5)
# corrplot::corrplot(test)
# dev.off()





# Single Factor Analysis --------------------------------------------------


