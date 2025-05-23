



# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(corrplot)

load("data/timing.RData")




# Correlation -------------------------------------------------------------


df <- data[, 8:30] |> as.matrix()

# 1. Compute Kendall's Tau Correlation Matrix
# Use method = "spearman" for Spearman's Rho
# Use use = "pairwise.complete.obs" for pairwise deletion of NAs
cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")


pdf(
  file = "results/timing/correlation.pdf",
  height = 5,
  width = 5
)

# Generate the plot
corrplot(cor_matrix,
         method = "color",       # Use color to represent correlation strength
         order = "hclust",       # Order rows/columns using hierarchical clustering
         tl.col = "black",       # Color of text labels
         tl.srt = 45,            # Rotate text labels for better readability
         tl.cex = 0.7,           # Size of text labels (adjust as needed)
         col = COL2('RdYlBu'),    # Use a Red-Yellow-Blue color palette (good for correlations)
         title = "Familiarity between Timing Signals",
         mar = c(0,0,1,0)        # Adjust plot margins c(bottom, left, top, right)
)


dev.off()

rm(df, cor_matrix)
