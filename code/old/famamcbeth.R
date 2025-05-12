

# Prepare data frame
df <- bond |> 
  select(eom, cusip, ret_exc) |> 
  left_join(fact |> select(date, def, term, vola), join_by(eom == date)) |> 
  rename(date := eom) |> 
  na.omit()



fm <- estimate_fama_macbeth(
  data      = df,
  model     = "ret_exc ~ def + term + vola"
)


fm <- df |> 
  group_by(cusip) |> 
  do(tidy(lm(ret_exc ~ def + term + vola, data = .))) |> 
  ungroup()


test <- fm |> 
  select(cusip, term, estimate) |> 
  pivot_wider(names_from = term, values_from = estimate)

test <- bond |> 
  select(eom, cusip, ret_exc) |>
  left_join(test, join_by(cusip == cusip))

final <- test |> 
  group_by(eom) |> 
  na.omit() |> 
  do(tidy(lm(ret_exc ~ def + term + vola, data = .))) |> 
  ungroup()


# 1. Compute upper/lower bounds for a 95% CI
plot_data <- final %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

# 2. Plot
plot_data |>
  filter(eom > "2021-01-01") |> 
  ggplot(aes(x = eom, y = estimate)) +
  # shaded CI ribbon
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  # estimate line
  geom_line(color = "steelblue", size = 1) +
  # one panel per term
  facet_grid(term ~ ., scales = "free_y") +
  # labels
  labs(
    title   = "Evolution of Factor Estimates Over Time",
    subtitle= "Shaded areas = 95% confidence bands",
    x       = "Date (EOM)",
    y       = "Estimate"
  ) +
  # clean theme + tweaks
  theme_bw() +
  theme(
    plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5),
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text       = element_text(size = 12, face = "bold"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10)
  )
