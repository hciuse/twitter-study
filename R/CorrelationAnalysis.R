# Author: Leonard Stellbrink
# Calculate Pearson and Spearman correlations between infection timelines for external survey, RKI, and MuSPAD data

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
  mutate(date = as.Date(date),
         week = floor_date(date, "week"),
         month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(ext_survey_count = n()) %>%
  arrange(month)

# Process MuSPAD Data ----------------------------------------------------
muspad_infections <- MuSPAD_df %>%
  mutate(firstinfection = make_date(s22_positive_PCR_year_1, 
                                    s22_positive_PCR_month_1, 
                                    s22_positive_PCR_day_1),
         secondinfection = make_date(s22_positive_PCR_year_2, 
                                     s22_positive_PCR_month_2, 
                                     s22_positive_PCR_day_2),
         thirdinfection = make_date(s22_positive_PCR_year_3, 
                                    s22_positive_PCR_month_3, 
                                    s22_positive_PCR_day_3)) %>%
  select(firstinfection, secondinfection, thirdinfection) %>%
  pivot_longer(cols = everything(), names_to = "infection", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(date = as.Date(date),
         week = floor_date(date, "week"),
         month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(muspad_count = n()) %>%
  arrange(month)

# Load RKI Data ----------------------------------------------------
rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")

# Check column names and structure
cat("RKI data columns:", paste(colnames(rkidata), collapse = ", "), "\n")

rki_infections <- rkidata %>%
  mutate(date = as.Date(Meldedatum),  # Using Meldedatum instead of Datum
         month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(rki_count = sum(`Inzidenz_7-Tage`, na.rm = TRUE)) %>%  # Using 7-day incidence
  arrange(month)

# Merge datasets ----------------------------------------------------
merged_data <- ext_survey_infections %>%
  full_join(muspad_infections, by = "month") %>%
  full_join(rki_infections, by = "month") %>%
  arrange(month) %>%
  filter(!is.na(ext_survey_count) & !is.na(muspad_count) & !is.na(rki_count))  # Keep only overlapping periods

# Calculate Correlations ----------------------------------------------------
cat("=== Correlation Analysis: Infection Timelines ===\n\n")

# External Survey vs MuSPAD
cat("External Survey vs MuSPAD:\n")
cat("Pearson correlation: ", 
    cor(merged_data$ext_survey_count, merged_data$muspad_count, 
        method = "pearson", use = "complete.obs"), "\n")
cat("Spearman correlation: ", 
    cor(merged_data$ext_survey_count, merged_data$muspad_count, 
        method = "spearman", use = "complete.obs"), "\n\n")

# External Survey vs RKI
cat("External Survey vs RKI:\n")
cat("Pearson correlation: ", 
    cor(merged_data$ext_survey_count, merged_data$rki_count, 
        method = "pearson", use = "complete.obs"), "\n")
cat("Spearman correlation: ", 
    cor(merged_data$ext_survey_count, merged_data$rki_count, 
        method = "spearman", use = "complete.obs"), "\n\n")

# MuSPAD vs RKI
cat("MuSPAD vs RKI:\n")
cat("Pearson correlation: ", 
    cor(merged_data$muspad_count, merged_data$rki_count, 
        method = "pearson", use = "complete.obs"), "\n")
cat("Spearman correlation: ", 
    cor(merged_data$muspad_count, merged_data$rki_count, 
        method = "spearman", use = "complete.obs"), "\n\n")

# Correlation tests with p-values
pearson_ext_muspad <- cor.test(merged_data$ext_survey_count, 
                               merged_data$muspad_count, 
                               method = "pearson")
spearman_ext_muspad <- cor.test(merged_data$ext_survey_count, 
                                merged_data$muspad_count, 
                                method = "spearman")

pearson_ext_rki <- cor.test(merged_data$ext_survey_count, 
                            merged_data$rki_count, 
                            method = "pearson")
spearman_ext_rki <- cor.test(merged_data$ext_survey_count, 
                             merged_data$rki_count, 
                             method = "spearman")

pearson_muspad_rki <- cor.test(merged_data$muspad_count, 
                               merged_data$rki_count, 
                               method = "pearson")
spearman_muspad_rki <- cor.test(merged_data$muspad_count, 
                                merged_data$rki_count, 
                                method = "spearman")

cat("P-values:\n")
cat("ExtSurvey-MuSPAD: Pearson p=", pearson_ext_muspad$p.value, 
    ", Spearman p=", spearman_ext_muspad$p.value, "\n")
cat("ExtSurvey-RKI: Pearson p=", pearson_ext_rki$p.value, 
    ", Spearman p=", spearman_ext_rki$p.value, "\n")
cat("MuSPAD-RKI: Pearson p=", pearson_muspad_rki$p.value, 
    ", Spearman p=", spearman_muspad_rki$p.value, "\n\n")

# Save correlation results
correlation_results <- data.frame(
  comparison = c("ExtSurvey_vs_MuSPAD", "ExtSurvey_vs_RKI", "MuSPAD_vs_RKI"),
  pearson_r = c(
    cor(merged_data$ext_survey_count, merged_data$muspad_count, method = "pearson"),
    cor(merged_data$ext_survey_count, merged_data$rki_count, method = "pearson"),
    cor(merged_data$muspad_count, merged_data$rki_count, method = "pearson")
  ),
  pearson_p = c(
    pearson_ext_muspad$p.value,
    pearson_ext_rki$p.value,
    pearson_muspad_rki$p.value
  ),
  spearman_rho = c(
    cor(merged_data$ext_survey_count, merged_data$muspad_count, method = "spearman"),
    cor(merged_data$ext_survey_count, merged_data$rki_count, method = "spearman"),
    cor(merged_data$muspad_count, merged_data$rki_count, method = "spearman")
  ),
  spearman_p = c(
    spearman_ext_muspad$p.value,
    spearman_ext_rki$p.value,
    spearman_muspad_rki$p.value
  )
)

#write_csv(correlation_results, "./correlation_results.csv")
cat("Results saved to ./correlation_results.csv\n")
