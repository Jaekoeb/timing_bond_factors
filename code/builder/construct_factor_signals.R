


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)
library(data.table)
library(TTR)


bond <- read_csv("data/bond_returns.csv")
bond_info <- read_csv("data/bond_info.csv")
treasury_yld <- read_csv("data/treasury_yld.csv")


# Join bond info ----------------------------------------------------------


bond_info <- bond_info |> 
  mutate(
    eom = DATE,
    cusip = CUSIP,
    off_date = OFFERING_DATE,
    off_price = OFFERING_PRICE,
    maturity = MATURITY,
    amt_out = AMOUNT_OUTSTANDING,
    rating = RATING_NUM,
    spread = as.numeric(gsub("%", "", T_Spread)) / 100,
    price = PRICE_EOM,
    bond_age = as.double(eom - off_date) / as.double(maturity - off_date)
  ) |> 
  select(
    eom, cusip, off_date, off_price, maturity, price, amt_out, rating, spread, bond_age
  )


bond <- bond |> left_join(bond_info, by = join_by(eom == eom, cusip == cusip))





# Join Treasury Info ------------------------------------------------------

treasury_yld <- treasury_yld |> 
  mutate(
    eom = as.Date(MCALDT),
    duration = TMDURATN / 365,
    tyield = TMYLD * 365
  ) |> 
  select(
    eom, duration, tyield
  )


bond <- setDT(bond)
treasury_yld <- setDT(treasury_yld |> na.omit())

# Set keys for both tables
setkey(bond, eom, duration)
setkey(treasury_yld, eom, duration)


# Perform the rolling join: for each row in df1, find the closest matching duration in df2
# The join below adds columns from df2 (e.g., yield) to df1 based on eom and the nearest duration.
bond <- treasury_yld[bond, roll = "nearest"]

bond <- as.data.frame(bond)


# Basics ------------------------------------------------------------------

start <- Sys.time()

# rating is already done

bond <- bond |> 
  arrange(cusip, eom) |>   # make sure data is sorted by time within each group
  group_by(cusip) |>
  mutate(
    mkt_val = -market_value,
    amt_out = -amt_out,
    dura = -duration,
    yields = yield,
    btm = off_price / price,
    gspread = yield - tyield
  )


end <- Sys.time()

# Check when signal exists
# check <- test |>
#   group_by(eom) |>
#   summarize(
#     nobs = n(),
#     exists = sum(!is.na(str))) |>
#   ungroup()


cat("Finished Basic signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")



# Liquidity ---------------------------------------------------------------

# Liquidity signals: bond_age and spread already constructed

# VaR and ES --------------------------------------------------------------

# Follow Dickerson et. al
# requiring a minimum of 24-months of data

start <- Sys.time()


bond <- bond |> 
  arrange(cusip, eom) |>   # make sure data is sorted by time within each group
  group_by(cusip) |> 
  mutate(var5 = rollapplyr(lag(ret_exc, 1), 
                           width = 36, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 24) {
                               NA
                             } else {
                               -1 * sort(x[!is.na(x)])[2]
                             }
                           }, 
                           fill = NA,         
                           align = "right"),
         
         var10 = rollapplyr(lag(ret_exc, 1), 
                            width = 36, 
                            FUN = function(x) {
                              if(sum(!is.na(x)) < 24) {
                                NA
                              } else {
                                -1 * sort(x[!is.na(x)])[4]
                              }
                            }, 
                            fill = NA,         
                            align = "right"),
         
         es10 = rollapplyr(lag(ret_exc, 1), 
                           width = 36, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 24) {
                               NA
                             } else {
                               -1 * mean(sort(x[!is.na(x)])[1:4])
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
    vola = rollapplyr(lag(ret_exc, 1), 
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
    skew = rollapplyr(lag(ret_exc, 1), 
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
    kurt = rollapplyr(lag(ret_exc, 1), 
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
    
    mom3 = rollapplyr(lag(ret_exc, 1), 
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
    
    
    mom6 = rollapplyr(lag(ret_exc, 1), 
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
    
    
    mom9 = rollapplyr(lag(ret_exc, 1), 
                      width = 9, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 6) {
                          NA
                        } else {
                          mean(x[!is.na(x)])
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    
    
    mom12 = rollapplyr(lag(ret_exc, 1), 
                      width = 12, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 9) {
                          NA
                        } else {
                          mean(x[!is.na(x)])
                        }
                      }, 
                      fill = NA,         
                      align = "right")
    
  ) |> 
  ungroup()


end <- Sys.time()

cat("Finished Momentum signals", "\n",
    "--------------------------------------------------------------------------", "\n",
    "Chunk took ", difftime(end, start, units = "secs"), " seconds")


# Save Data ---------------------------------------------------------------


cat("Now saving data to PC", "\n", 
    "--------------------------------------------------------------------------")



# Remove unnecessary columns
bond <- bond |> 
  select(-off_date, -off_price)




# Save all signals names
signals <- colnames(bond)[13:33]
signals <- sort(signals)

writeLines(
  signals,
  "data/signals.txt"
)


# Reorder data frame and save
columns <- c(colnames(bond[1:12]), signals)

bond <- bond |> select(all_of(columns))

file_path <- "data/bond.RData"

# Check if the file exists
if (file.exists(file_path)) {
  # Prompt the user for confirmation
  response <- askYesNo(paste("File", file_path, "already exists. Overwrite?"))
  
  # Handle the response
  if (isTRUE(response)) {
    # User confirmed, so overwrite the file
    save(bond, file = file_path)
    print("File overwritten.")
  } else {
    # User declined, so don't overwrite
    print("File not overwritten.")
  }
} else {
  # File doesn't exist, so save it
  save(bond, file = file_path)
  print("File saved.")
}

cat("Jobs done!")

