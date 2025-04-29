


treasury_yld <- treasury_yld |> 
  mutate(
    eom = MCALDT,
    duration = TMDURATN / 365,
    yield = 100 * TMYLD * 365
  ) |> 
  select(
    eom, duration, yield
  )


rand_date = sample(unique(treasury_yld$eom), 1)

treasury_yld |> 
  filter(eom == rand_date) |> 
  ggplot(aes(x = duration, y = yield)) +
  geom_point()



ylds <- bond |> select(eom, duration) |> unique()


library(data.table)

test <- setDT(bond)
treas <- setDT(treasury_yld |> na.omit())

# Set keys for both tables
setkey(test, eom, duration)
setkey(treas, eom, duration)


# Perform the rolling join: for each row in df1, find the closest matching duration in df2
# The join below adds columns from df2 (e.g., yield) to df1 based on eom and the nearest duration.
result <- treas[test, roll = "nearest"]
