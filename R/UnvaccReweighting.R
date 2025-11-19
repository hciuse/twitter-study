library(dplyr)
library(ggplot2)
library(knitr)
library(here)
library(tidyr)

# Load and prepare data
cleaned_data <- readRDS(file = here("data", "cleaned_data.rds"))

# Create English mapping for infection frequencies
cleaned_data <- cleaned_data %>%
  mutate(
    num_c19_infs_eng = case_when(
      num_c19_infs == "Nie" ~ "0",
      num_c19_infs == "Einmal" ~ "1",
      num_c19_infs == "Zweimal" ~ "2+",
      num_c19_infs == "Dreimal" ~ "2+",
      num_c19_infs == "Mehr als dreimal" ~ "2+",
      num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
      TRUE ~ NA_character_
    )
  )

# Filter groups
unvacc <- cleaned_data %>% filter(c19_vaccination_status == "Nein")
vacc <- cleaned_data %>% filter(c19_vaccination_status == "Ja")
total_sample <- cleaned_data %>% filter(!is.na(num_c19_infs))

# Sample sizes
n_unvacc <- nrow(unvacc)
n_vacc <- nrow(vacc)
n_total <- 867  # Total sample size as specified
n_na_vacc_status <- n_total - n_unvacc - n_vacc

# Calculate percentages for all three groups using the English mapping
unvacc_counts <- table(unvacc$num_c19_infs_eng)
unvacc_pct <- prop.table(unvacc_counts) * 100

vacc_counts <- table(vacc$num_c19_infs_eng)
vacc_pct <- prop.table(vacc_counts) * 100

total_counts <- table(total_sample$num_c19_infs_eng)
total_pct <- prop.table(total_counts) * 100

# Define infection categories in English
infection_categories_eng <- c("0", "1", "2+", "I Don't Want To Answer")

# Create comparison data frame
comparison_df <- data.frame(
  Category = infection_categories_eng,
  Unvaccinated = sapply(infection_categories_eng, function(x) {
    ifelse(x %in% names(unvacc_pct), unvacc_pct[x], 0)
  }),
  Vaccinated = sapply(infection_categories_eng, function(x) {
    ifelse(x %in% names(vacc_pct), vacc_pct[x], 0)
  }),
  Total_Sample = sapply(infection_categories_eng, function(x) {
    ifelse(x %in% names(total_pct), total_pct[x], 0)
  })
)

# Reshape for plotting
comparison_long <- comparison_df %>%
  pivot_longer(cols = c(Unvaccinated, Vaccinated, Total_Sample), 
               names_to = "Group", 
               values_to = "Percentage") %>%
  mutate(Group = recode(Group, Total_Sample = "Total Sample"))

# Set factor order for categories
comparison_long$Category <- factor(comparison_long$Category, levels = infection_categories_eng)
comparison_long$Group <- factor(comparison_long$Group, levels = c("Unvaccinated", "Vaccinated", "Total Sample"))

# Define target proportions based on total sample of 867
target_unvacc_prop <- 0.13
target_vacc_prop <- 0.87  # Assuming NA participants would follow vaccinated distribution

# Current proportions
current_unvacc_prop <- n_unvacc / n_total
current_vacc_prop <- (n_vacc + n_na_vacc_status) / n_total

# Hypothetical sample sizes for 13% unvaccinated scenario
hypothetical_n_unvacc <- round(n_total * target_unvacc_prop)
hypothetical_n_vacc <- n_total - hypothetical_n_unvacc

# Define infection categories in German (for actual counting)
infection_categories <- c("Nie", "Einmal", "Zweimal", "Dreimal", 
                          "Mehr als dreimal", "Ich möchte nicht antworten")

# Calculate actual counts for each infection frequency category
actual_counts_unvacc <- sapply(infection_categories, function(x) {
  sum(unvacc$num_c19_infs == x, na.rm = TRUE)
})

actual_counts_vacc_and_na <- sapply(infection_categories, function(x) {
  sum(total_sample$num_c19_infs == x & 
        (total_sample$c19_vaccination_status == "Ja" | 
           is.na(total_sample$c19_vaccination_status)), na.rm = TRUE)
})

# Calculate proportions within each group
prop_unvacc <- actual_counts_unvacc / n_unvacc
prop_vacc_and_na <- actual_counts_vacc_and_na / (n_vacc + n_na_vacc_status)

# Apply these proportions to hypothetical sample sizes
expected_counts_unvacc <- hypothetical_n_unvacc * prop_unvacc
expected_counts_vacc <- hypothetical_n_vacc * prop_vacc_and_na

# Calculate total counts for actual and reweighted scenarios
actual_total_counts <- sapply(infection_categories, function(x) {
  sum(total_sample$num_c19_infs == x, na.rm = TRUE)
})
expected_total_counts <- expected_counts_unvacc + expected_counts_vacc

# Map German categories to English for display
category_mapping <- data.frame(
  German = infection_categories,
  English = c("Never", "Once", "Twice", "Three times", 
              "More than three times", "Prefer not to answer")
)

# Create comparison data for actual vs expected (as percentages)
reweight_comparison <- data.frame(
  Category_DE = infection_categories,
  Actual_Count = actual_total_counts,
  Expected_Count = expected_total_counts,
  Actual_Pct = (actual_total_counts / n_total) * 100,
  Expected_Pct = (expected_total_counts / n_total) * 100
) %>%
  left_join(category_mapping, by = c("Category_DE" = "German")) %>%
  rename(Category = English)

reweight_comparison$Change_Pct <- reweight_comparison$Expected_Pct - reweight_comparison$Actual_Pct
reweight_comparison$Category <- factor(reweight_comparison$Category, 
                                       levels = c("Never", "Once", "Twice", "Three times", 
                                                  "More than three times", "Prefer not to answer"))

# Reshape for plotting
reweight_long <- reweight_comparison %>%
  select(Category, Actual_Pct, Expected_Pct) %>%
  filter(Category == c("Never", "Once", "Prefer not to answer")) %>% 
  pivot_longer(cols = c(Actual_Pct, Expected_Pct), 
               names_to = "Scenario", 
               values_to = "Percentage") %>%
  mutate(Scenario = recode(Scenario, 
                           Actual_Pct = "Actual", 
                           Expected_Pct = "Reweighted (13% Unvacc)"))

comp <- ggplot(reweight_long, aes(x = Category, y = Percentage, fill = Scenario)) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.7) +
  scale_fill_manual(values = c(
    "Actual" = "#95A5A6",
    "Reweighted (13% Unvacc)" = "#E67E22"
  )) +
  labs(title = "Total Infection Percentages: Actual vs. Reweighted to 13% Unvaccinated (N=867)",
       x = "Infection Frequency",
       y = "Percentage of Total Sample (%)",
       fill = "Scenario") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top",
    legend.background = element_rect("white"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(1, "cm")) +
  theme(
    text = element_text(size = 50),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks.x = element_line(size = 0.9),
    axis.ticks.y = element_line(size = 1),
    axis.ticks.length = unit(20, "pt"),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
  

ggsave(here("plots", "VaccReweighting.pdf"), plot = comp, width = 35, height = 15, dpi = 300)
ggsave(here("plots", "VaccReweighting.png"), plot = comp, width = 35, height = 15, dpi = 300)