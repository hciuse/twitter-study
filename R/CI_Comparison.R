library(tidyverse)
library(DescTools)

# Read in the external survey data
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")

# Process vaccination data (following original code logic)
vaccinationData <- ext_survey_df %>% 
  select(year_of_birth, c19_vaccination_status, 
         c19_vaccination_details_vaccine_dose_1, 
         c19_vaccination_details_vaccine_dose_2, 
         c19_vaccination_details_vaccine_dose_3, 
         c19_vaccination_details_vaccine_dose_4)

# Create age groups
vaccinationData <- vaccinationData %>%  
  mutate(agegroup = case_when(
    2023 - year_of_birth >= 80 ~ "80-99",
    2023 - year_of_birth >= 60 ~ "60-79",
    2023 - year_of_birth >= 40 ~ "40-59",
    2023 - year_of_birth >= 18 ~ "18-39"
  ))

# Code dose receipt (following original logic)
vaccinationData <- vaccinationData %>%
  mutate(dose_1_received = case_when(
    c19_vaccination_details_vaccine_dose_1 %in% 
      c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", 
        "AstraZeneca", "Andere") ~ "Yes",
    c19_vaccination_details_vaccine_dose_1 == "Nicht zutreffend" ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(dose_2_received = case_when(
    c19_vaccination_details_vaccine_dose_2 %in% 
      c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", 
        "AstraZeneca", "Andere") ~ "Yes",
    c19_vaccination_details_vaccine_dose_2 == "Nicht zutreffend" ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(dose_3_received = case_when(
    c19_vaccination_details_vaccine_dose_3 %in% 
      c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", 
        "AstraZeneca", "Andere") ~ "Yes",
    c19_vaccination_details_vaccine_dose_3 == "Nicht zutreffend" ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(dose_4_received = case_when(
    c19_vaccination_details_vaccine_dose_4 %in% 
      c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", 
        "AstraZeneca", "Andere") ~ "Yes",
    c19_vaccination_details_vaccine_dose_4 == "Nicht zutreffend" ~ "No",
    TRUE ~ NA_character_
  ))

# Calculate sample sizes and successes by age group and dose
ci_summary <- vaccinationData %>%
  filter(!is.na(agegroup)) %>%
  pivot_longer(cols = c(dose_1_received, dose_2_received, 
                        dose_3_received, dose_4_received),
               names_to = "dose", values_to = "received") %>%
  filter(!is.na(received)) %>%
  group_by(agegroup, dose) %>%
  summarise(
    n = n(),
    x = sum(received == "Yes"),
    .groups = "drop"
  ) %>%
  mutate(
    dose_label = case_when(
      dose == "dose_1_received" ~ "Dose 1",
      dose == "dose_2_received" ~ "Dose 2",
      dose == "dose_3_received" ~ "Dose 3",
      dose == "dose_4_received" ~ "Dose 4"
    ),
    p = x / n
  )

# Calculate confidence intervals using BinomCI
ci_comparison <- ci_summary %>%
  rowwise() %>%
  mutate(
    # Wald interval (as used in original code)
    wald_lci = BinomCI(x, n, method = "wald")[2],
    wald_uci = BinomCI(x, n, method = "wald")[3],
    
    # Wilson score interval
    wilson_lci = BinomCI(x, n, method = "wilson")[2],
    wilson_uci = BinomCI(x, n, method = "wilson")[3],
    
    # Jeffreys interval
    jeffreys_lci = BinomCI(x, n, method = "jeffreys")[2],
    jeffreys_uci = BinomCI(x, n, method = "jeffreys")[3]
  ) %>%
  ungroup() %>%
  mutate(
    wald_width = wald_uci - wald_lci,
    wilson_width = wilson_uci - wilson_lci,
    jeffreys_width = jeffreys_uci - jeffreys_lci
  )

# Print summary table
print("Confidence Interval Comparison")
print("=" %>% rep(80) %>% paste(collapse = ""))
ci_comparison %>%
  select(agegroup, dose_label, n, x, p, 
         wald_lci, wald_uci, wilson_lci, wilson_uci, 
         jeffreys_lci, jeffreys_uci) %>%
  print(n = Inf)

# Calculate interval width differences
print("\nInterval Width Comparison")
print("=" %>% rep(80) %>% paste(collapse = ""))
ci_comparison %>%
  select(agegroup, dose_label, n, p,
         wald_width, wilson_width, jeffreys_width) %>%
  mutate(
    wilson_vs_wald = wilson_width - wald_width,
    jeffreys_vs_wald = jeffreys_width - wald_width
  ) %>%
  print(n = Inf)

# Visualization comparing the three methods
plot_data <- ci_comparison %>%
  select(agegroup, dose_label, p, n, x,
         wald_lci, wald_uci, wilson_lci, wilson_uci, 
         jeffreys_lci, jeffreys_uci) %>%
  pivot_longer(cols = c(wald_lci, wald_uci, wilson_lci, wilson_uci, 
                        jeffreys_lci, jeffreys_uci), 
               names_to = c("method", "bound"),
               names_pattern = "(.*)_(lci|uci)") %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(method = factor(method, 
                         levels = c("wald", "wilson", "jeffreys"),
                         labels = c("Wald", "Wilson", "Jeffreys")))

ggplot(plot_data, aes(x = dose_label, y = p, color = method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                position = position_dodge(width = 0.5), 
                width = 0.3, linewidth = 1) +
  facet_wrap(~agegroup, nrow = 2) +
  theme_minimal() +
  labs(title = "Comparison of Confidence Interval Methods",
       subtitle = "Wald vs Wilson vs Jeffreys Intervals",
       x = "Vaccination Dose",
       y = "Proportion Vaccinated",
       color = "CI Method") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./plots/CI_Comparison.pdf", width = 12, height = 8)
ggsave("./plots/CI_Comparison.png", width = 12, height = 8, dpi = 300)