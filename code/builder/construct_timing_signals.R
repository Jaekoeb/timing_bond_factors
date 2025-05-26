


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(zoo)
load("data/factors.RData")




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
                      align = "right"),
    
    
    # Absolute 12 month vola difference
    vol4 = rollapplyr(rollvol, 
                      width = 12, 
                      FUN = function(x) {
                        if(sum(!is.na(x)) < 9) {
                          NA
                        } else {
                          mean(x[!is.na(x)]) - x[12]
                        }
                      }, 
                      fill = NA,         
                      align = "right"),
    vol4 = sign(vol4)
    
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





# Averaging ---------------------------------------------------------------


data <- data |> 
  group_by(factor) |> 
  arrange(eom) |>
  mutate(
    
    # Aggregate Momentum Signals
    mom = rowMeans(pick(mom1, mom3, mom6, mom12, smom1, smom3, smom6, smom12), na.rm = TRUE),
    
    # Aggregate Volatility Signals
    vol = rowMeans(pick(vol1, vol2, vol3, vol4), na.rm = TRUE),
    
    # Aggregate Reversal Signals
    rev = rowMeans(pick(rev1, rev2, rev3), na.rm = TRUE),
    
    # Aggregate Characteristics Spread Signals
    char = rowMeans(pick(char1, char2, char3), na.rm = TRUE),
    
    # Aggregate All Signals
    all = rowMeans(pick(mom, vol, rev, char), na.rm = TRUE)
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
      signal == "all" ~ "all"
    )
  )


# Save --------------------------------------------------------------------


data <- long

# Save the data frame
save(data, file = "data/timing.RData")
