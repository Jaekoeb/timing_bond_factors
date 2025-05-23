


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
    
    smom3 = pmax(pmin(mom3 / rollvol, 2), -2),
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
    
    smom6 = pmax(pmin(mom6 / rollvol, 2), -2),
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
    
    smom12 = pmax(pmin(mom12 / rollvol, 2), -2),
    mom12 = sign(mom12),
    
  ) |> 
  ungroup()





# Volatility --------------------------------------------------------------


data <- data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    
    vol1 = (rollvol - lag(rollvol)) / lag(rollvol)
    
    
  )












# Test Zone ---------------------------------------------------------------



df <- fact

df <- df |> 
  group_by(factor) |> 
  arrange(date) |> 
  mutate(
    mom1 = return * lag(mom1),
    mom3 = return * lag(mom3),
    mom6 = return * lag(mom6),
    mom12 = return * lag(mom12)
  ) |> 
  na.omit() |> 
  mutate(
    untimed = 100 * cumprod(1 + return) / first(1 + return),
    mom1 = 100 * cumprod(1 + mom1) / first(1 + mom1),
    mom3 = 100 * cumprod(1 + mom3) / first(1 + mom3),
    mom6 = 100 * cumprod(1 + mom6) / first(1 + mom6),
    mom12 = 100 * cumprod(1 + mom12) / first(1 + mom12)
  ) |> 
  ungroup()




name = "gspread"

df |>
  filter(factor == name) |>
  select(-return, -factor) |> 
  pivot_longer(!date, names_to = "strategy") |> 
  ggplot(aes(x = date, y = value, group = strategy, color = strategy)) +
  geom_line(size = 1) +
  theme_bw()

test <- fact |> filter(factor == "gspread")
