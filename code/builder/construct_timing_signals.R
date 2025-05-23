


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(zoo)
load("data/factors.RData")


# Simple Momentum ----------------------------------------------------------------

data <- data |> 
  group_by(factor) |> 
  arrange(date) |> 
  mutate(
    
    mom1 = sign(return),
    
    mom3 = sign(rollapplyr(return, 
               width = 3, 
               FUN = function(x) {
                 if(sum(!is.na(x)) < 2) {
                   NA
                 } else {
                   mean(x[!is.na(x)])
                 }
               }, 
               fill = NA,         
               align = "right")),
    
    mom6 = sign(rollapplyr(return, 
                           width = 6, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 4) {
                               NA
                             } else {
                               mean(x[!is.na(x)])
                             }
                           }, 
                           fill = NA,         
                           align = "right")),
    
    mom12 = sign(rollapplyr(return, 
                           width = 12, 
                           FUN = function(x) {
                             if(sum(!is.na(x)) < 9) {
                               NA
                             } else {
                               mean(x[!is.na(x)])
                             }
                           }, 
                           fill = NA,         
                           align = "right"))
    
  ) |> 
  ungroup()





# Scaled Momentum ---------------------------------------------------------















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
