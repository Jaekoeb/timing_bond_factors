


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)


bond <- read_csv("data/bond_returns.csv")




# Constructing Signals for Factors
# Base it on regular excess returns
# (dur. matched exc. returns better?)




# VaR and ES --------------------------------------------------------------

# Follow Dickerson et. al
# requiring a minimum of 24-months of data

start <- Sys.time()


bond <- bond |> 
  arrange(cusip, eom) |>   # make sure data is sorted by time within each group
  group_by(cusip) |> 
  mutate(var5 = rollapplyr(ret_exc, 
                           width = 36, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 24) {
                               NA
                             } else {
                               sort(x[!is.na(x)])[2]
                             }
                           }, 
                           fill = NA,         
                           align = "right"),
         
         var10 = rollapplyr(ret_exc, 
                            width = 36, 
                            FUN = function(x) {
                              if(sum(!is.na(x)) < 24) {
                                NA
                              } else {
                                sort(x[!is.na(x)])[4]
                              }
                            }, 
                            fill = NA,         
                            align = "right"),
         
         es10 = rollapplyr(ret_exc, 
                           width = 36, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 24) {
                               NA
                             } else {
                               mean(sort(x[!is.na(x)])[1:4])
                             }
                           }, 
                           fill = NA,         
                           align = "right")
  ) |> 
  ungroup()



end <- Sys.time()


# Check when signal exists
# check <- bond |> 
#   group_by(eom) |> 
#   summarize(
#     nobs = n(),
#     exists = sum(!is.na(es10))) |> 
#   ungroup()



cat("Finished VaR and ES signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")


# Liquidity ---------------------------------------------------------------





# Reversal ----------------------------------------------------------------


start <- Sys.time()

bond <- bond |> 
  arrange(cusip, eom) |> 
  group_by(cusip) |> 
  mutate(
    
    # Short Term Reversal
    str = -lag(ret_exc),
    
    # Long Term Reversal
    ltr = rollapplyr(ret_exc,
                     width = 48,
                     FUN = function(x) {
                       # Select the 36 observations from 48 to 13 months back
                       x_subset <- x[1:36]
                       # Check if less than 24 non-NA observations are present
                       if(sum(!is.na(x_subset)) < 24) {
                         return(NA)
                       } else {
                         # Compute the product of (1 + ret_exc) for non-NA observations
                         return(prod(1 + x_subset[!is.na(x_subset)]))
                       }
                     },
                     fill = NA,   # Returns NA if fewer than 48 observations are available
                     align = "right")
  ) |> 
  ungroup()



end <- Sys.time()

# Check when signal exists
# check <- test |>
#   group_by(eom) |>
#   summarize(
#     nobs = n(),
#     exists = sum(!is.na(str))) |>
#   ungroup()


cat("Finished Reversal signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")



# Moments -----------------------------------------------------------------


# Load the moments package
library(moments)


start <- Sys.time()

bond <- bond |> 
  arrange(cusip, eom) |> 
  group_by(cusip) |> 
  mutate(
    
    
    # Volatility
    vola = rollapplyr(ret_exc, 
                      width = 36, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 24) {
                          NA
                        } else {
                          sd(x, na.rm = TRUE)
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    # Skewness
    # (compute negative skewness, negative signal)
    skew = rollapplyr(ret_exc, 
                      width = 36, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 24) {
                          NA
                        } else {
                          - skewness(x, na.rm = TRUE)
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    # Kurtosis
    kurt = rollapplyr(ret_exc, 
                      width = 36, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 24) {
                          NA
                        } else {
                          kurtosis(x, na.rm = TRUE)
                        }
                      }, 
                      fill = NA,         
                      align = "right")
    
    
  ) |> 
  ungroup()


end <- Sys.time()

cat("Finished Moments signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")


# Momentum ----------------------------------------------------------------

start <- Sys.time()

bond <- bond |> 
  arrange(cusip, eom) |> 
  group_by(cusip) |>
  mutate(
    
    mom3 = rollapplyr(ret_exc, 
                      width = 3, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 2) {
                          NA
                        } else {
                          prod(1 + x[!is.na(x)]) - 1
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    mom6 = rollapplyr(ret_exc, 
                      width = 6, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 4) {
                          NA
                        } else {
                          prod(1 + x[!is.na(x)]) - 1
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    mom9 = rollapplyr(ret_exc, 
                      width = 9, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 6) {
                          NA
                        } else {
                          prod(1 + x[!is.na(x)]) - 1
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    mom12 = rollapplyr(ret_exc, 
                      width = 12, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 9) {
                          NA
                        } else {
                          prod(1 + x[!is.na(x)]) - 1
                        }
                      }, 
                      fill = NA,         
                      align = "right")
    
  )


end <- Sys.time()

cat("Finished Momentum signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")


# Save Data ---------------------------------------------------------------


cat("Now saving data to PC", "\n", 
    "--------------------------------------------------------------------------")

save(
  bond,
  file = "data/bond.RData"
)


beepr::beep(sound = 1)
cat("Jobs done!")

