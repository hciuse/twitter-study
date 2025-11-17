# Author: Leonard Stellbrink
# Calculate Pearson and Spearman correlations between infection timelines 
# for external survey, RKI, and MuSPAD data

library(here)
library(tidyverse)
library(lubridate)

# Load data
ext_survey_df <- readRDS(file = here("data", "cleaned_data.rds"))
source(here("R", "MuSPADPreprocessing.R"))

# Process External Survey Data ----------------------------------------------------
ext_survey_infections <- ext_survey_df %>% 
  select(date_f1_inf, date_s2_inf, date_t3_inf) %>%
  pivot_longer(cols = everything(), names_to = "infection", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month")
  ) %>%
  filter(date >= as.Date("2019-01-01")) %>%
  count(month, name = "ext_survey_count") %>%
  arrange(month)

# Process MuSPAD Data ----------------------------------------------------
muspad_infections <- MuSPAD_df %>%
  mutate(
    firstinfection = make_date(s22_positive_PCR_year_1, 
                               s22_positive_PCR_month_1, 
                               s22_positive_PCR_day_1),
    secondinfection = make_date(s22_positive_PCR_year_2, 
                                s22_positive_PCR_month_2, 
                                s22_positive_PCR_day_2),
    thirdinfection = make_date(s22_positive_PCR_year_3, 
                               s22_positive_PCR_month_3, 
                               s22_positive_PCR_day_3)
  ) %>%
  select(firstinfection, secondinfection, thirdinfection, 
         s23_test_covid_2023, w22_positive_PCR_day_1) %>%
  pivot_longer(cols = everything(), names_to = "infection", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month")
  ) %>%
  filter(date >= as.Date("2019-01-01")) %>%
  count(month, name = "muspad_count") %>%
  arrange(month)

# Load and Process RKI Data ----------------------------------------------------
rki_infections <- read_csv(
  "https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(Meldedatum),
    month = floor_date(date, "month")
  ) %>%
  group_by(month) %>%
  summarise(rki_count = sum(`Inzidenz_7-Tage`, na.rm = TRUE), .groups = "drop") %>%
  arrange(month)

# Merge datasets ----------------------------------------------------
merged_data <- ext_survey_infections %>%
  inner_join(muspad_infections, by = "month") %>%
  inner_join(rki_infections, by = "month") %>%
  arrange(month)

# Check if we have sufficient data
if (nrow(merged_data) < 3) {
  stop("Insufficient overlapping data points for correlation analysis. Need at least 3 observations.")
}

# Calculate Correlations with Tests ----------------------------------------------------
cat("=== Correlation Analysis: Infection Timelines ===\n")
cat("Number of overlapping months:", nrow(merged_data), "\n")
cat("Date range:", min(merged_data$month), "to", max(merged_data$month), "\n\n")

# Function to perform and report correlation tests
perform_correlation_tests <- function(x, y, label) {
  pearson <- cor.test(x, y, method = "pearson")
  spearman <- cor.test(x, y, method = "spearman")
  
  cat(label, ":\n")
  cat(sprintf("  Pearson:  r = %.4f, p = %.4e\n", 
              pearson$estimate, pearson$p.value))
  cat(sprintf("  Spearman: ρ = %.4f, p = %.4e\n\n", 
              spearman$estimate, spearman$p.value))
  
  return(list(
    pearson_r = pearson$estimate,
    pearson_p = pearson$p.value,
    spearman_rho = spearman$estimate,
    spearman_p = spearman$p.value
  ))
}

# Run correlation tests
ext_muspad <- perform_correlation_tests(
  merged_data$ext_survey_count, 
  merged_data$muspad_count, 
  "External Survey vs MuSPAD"
)

ext_rki <- perform_correlation_tests(
  merged_data$ext_survey_count, 
  merged_data$rki_count, 
  "External Survey vs RKI"
)

muspad_rki <- perform_correlation_tests(
  merged_data$muspad_count, 
  merged_data$rki_count, 
  "MuSPAD vs RKI"
)

# Save correlation results
correlation_results <- tibble(
  comparison = c("ExtSurvey_vs_MuSPAD", "ExtSurvey_vs_RKI", "MuSPAD_vs_RKI"),
  pearson_r = c(ext_muspad$pearson_r, ext_rki$pearson_r, muspad_rki$pearson_r),
  pearson_p = c(ext_muspad$pearson_p, ext_rki$pearson_p, muspad_rki$pearson_p),
  spearman_rho = c(ext_muspad$spearman_rho, ext_rki$spearman_rho, muspad_rki$spearman_rho),
  spearman_p = c(ext_muspad$spearman_p, ext_rki$spearman_p, muspad_rki$spearman_p),
  n_observations = nrow(merged_data)
)

# Save results
#saveRDS(correlation_results, here("output", "correlation_results.rds"))
#write_csv(correlation_results, here("output", "correlation_results.csv"))

#cat("Results saved to output directory.\n")