



# Libraries and Data ------------------------------------------------------

load("data/factors.RData")

fact <- data |> select(eom, factor, return)



stol <- read_csv("data/bond_factors.csv", 
                 col_types = cols(date = col_date(format = "%Y-%m-%d")))

factors <- c(
  "value", "mkt_val", "amt_out", "dura", "bond_age",
  "btm", "gspread", "yields", "var5", "vola", "es10",
  "def_beta", "term_beta", "var10", "skew", "mom3",
  "mom6", "mom9", "mom12", "mom_equ", "str", "ltr"
)

colnames(stol) <- c("date", factors)

stol <- stol |> pivot_longer(!date, names_to = "factor", values_to = "stol")


# Compute ID to merge
stol <- stol |> mutate(id = format(date, "%Y-%m"))
fact <- fact |> mutate(id = format(eom, "%Y-%m"))


data <- left_join(fact, stol |> select(id, factor, stol), join_by(id == id, factor == factor))
data <- data |> select(-id) |> rename(pertl = return)

rm(fact, stol, factors)


data <- data |> filter(!is.na(pertl))

# Analysis ----------------------------------------------------------------


data <-  data |> 
  group_by(factor) |> 
  arrange(eom) |> 
  mutate(
    pertl = 100 * cumprod(1 + pertl) / first(1+pertl),
    stol = 100 * cumprod(1 + stol) / first(1+stol)
  ) |> 
  ungroup()


# Get the unique factors
unique_factors <- unique(data$factor)

# Loop through each factor and create a plot
for (fact in unique_factors) {
  # Filter the data for the current factor
  df_subset <- subset(data, factor == fact)
  
  # Create the line plot using ggplot2
  plot <- ggplot(df_subset, aes(x = eom)) +
    geom_line(aes(y = pertl, color = "pertl")) +
    geom_line(aes(y = stol, color = "stol")) +
    scale_color_manual(values = c("pertl" = "blue", "stol" = "red")) + # Customize colors if needed
    labs(
      title = paste("Comparison of pertl and stol for Factor:", fact),
      x = "Date",
      y = "Value",
      color = "Legend"
    ) +
    theme_bw() # Use a minimal theme for cleaner look
  
  # File path
  file_path <- paste0("results/cross_section/robustness/reproducibility/", fact, ".pdf")
  ggsave(filename = file_path,
         plot = plot,
         units = "cm",
         height = 10,
         width = 15)
  
  
}


