# --------------------------------------------------
# 1) EXAMPLE DATA
#    Suppose you have a data frame with columns:
#    - source: character name
#    - prop_0, prop_1, prop_2plus: numeric proportions
# --------------------------------------------------


# TODO Get from data not from figures

df <- data.frame(
  source   = c("Twitter", "Mastodon", "External", "MuSPAD", "Cosmo"),
  prop_0   = c(0.28,      0.38,       0.34,       0.33,     0.50),
  prop_1   = c(0.55,      0.50,       0.56,       0.56,     0.42),
  prop_2plus= c(0.17,     0.12,       0.10,       0.11,     0.08)
)

# Check that each row sums to 1
# (Optional, but good practice to confirm)
df$row_sum <- df$prop_0 + df$prop_1 + df$prop_2plus
df

# --------------------------------------------------
# 2) DEFINE FUNCTIONS FOR:
#    - Overlap
#    - Total Variation Distance (TVD)
# --------------------------------------------------

overlap <- function(pA, pB) {
  # pA and pB are numeric vectors of the same length
  sum(pmin(pA, pB))
}

tvd <- function(pA, pB) {
  # Total Variation Distance = 0.5 * L1 distance
  0.5 * sum(abs(pA - pB))
}

# --------------------------------------------------
# 3) PAIRWISE COMPARISONS
#    We'll compare each pair of sources (i,j) only once.
#    combn(...) gives all unique pairs of row indices.
# --------------------------------------------------

pairs <- combn(nrow(df), 2)  # all pairs of row indices
results <- data.frame(
  sourceA        = character(),
  sourceB        = character(),
  overlap        = numeric(),
  overlapPercent = numeric(),
  tvd            = numeric(),
  similarity     = numeric(),      # 1 - TVD
  similarityPerc = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each pair
for (k in 1:ncol(pairs)) {
  i <- pairs[1, k]
  j <- pairs[2, k]
  
  # Extract the proportion vectors for the two sources
  pA <- as.numeric(df[i, c("prop_0","prop_1","prop_2plus")])
  pB <- as.numeric(df[j, c("prop_0","prop_1","prop_2plus")])
  
  # Calculate Overlap and TVD
  ov  <- overlap(pA, pB)
  d   <- tvd(pA, pB)
  
  # Append row to 'results'
  results <- rbind(
    results,
    data.frame(
      sourceA        = df$source[i],
      sourceB        = df$source[j],
      overlap        = ov,           # fraction in [0, 1]
      overlapPercent = 100 * ov,
      tvd            = d,            # distance in [0, 1]
      similarity     = 1 - d,        # in [0, 1]
      similarityPerc = 100 * (1 - d),
      stringsAsFactors = FALSE
    )
  )
}

# --------------------------------------------------
# 4) INSPECT RESULTS
# --------------------------------------------------
results 
