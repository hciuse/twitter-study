library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(readxl)
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")
source("./R/MuspadPreprocessing.R")

# Creation of palette for comparison - now with 3 vaccination status groups
palette_vacc_bars <- function() {
  c("#9900CC", "#CC6600", "#009900")  # Purple for Vaccinated, Orange for Unvaccinated, Green for NA
}
palette_vacc_errorbars <- function() {
  c("#640085", "#804000", "#006600")
}

# Helper function to add vaccination status label
add_vacc_label <- function(vacc_status) {
  case_when(
    vacc_status == "Ja" ~ "Vaccinated",
    vacc_status == "Nein" ~ "Unvaccinated",
    is.na(vacc_status) | vacc_status == "NA" ~ "No Answer",
    TRUE ~ "No Answer"  # Catch-all for any other cases
  )
}

# Gender ------------------------------------------------------------------

# Processing of external survey data - all three groups
GenderData <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(gender, vacc_group) %>%
  mutate(gender = case_when(
    gender == "Weiblich" ~ "female", 
    gender == "Männlich" ~ "male",
    gender == "Divers" ~ "diverse",
    gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"
  )) %>% 
  filter(gender != "I Don't Want To Answer")

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse"))
GenderData$vacc_group <- factor(GenderData$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
GenderPlot <- GenderData %>% 
  group_by(vacc_group) %>%
  count(gender) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(gender, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = gender, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, size = 1.3) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Gender") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Age ---------------------------------------------------------------------

# Processing of external survey data - all three groups
AgeData <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(year_of_birth, vacc_group) %>% 
  mutate(age = 2023 - year_of_birth) %>%
  mutate(age_bracket = case_when(
    age < 20 ~ "18-39",
    age < 40 ~ "18-39",
    age < 60 ~ "40-59",
    age < 80 ~ "60-79",
    age < 100 ~ "80-99"
  ))

AgeData$age_bracket <- factor(AgeData$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))
AgeData$vacc_group <- factor(AgeData$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
AgePlot <- AgeData %>% 
  filter(!is.na(age_bracket)) %>% 
  group_by(vacc_group) %>%
  count(age_bracket) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(age_bracket, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = age_bracket, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, size = 1.3) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Age Bracket") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Household Size ----------------------------------------------------

# Processing of external survey data - all three groups
HouseholdData <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(hsld_size_01_2023_, vacc_group) %>%
  mutate(value = case_when(
    hsld_size_01_2023_ == 1 ~ "1", 
    hsld_size_01_2023_ == 2 ~ "2", 
    hsld_size_01_2023_ == 3 ~ "3", 
    hsld_size_01_2023_ == 4 ~ "4", 
    hsld_size_01_2023_ >= 5 ~ "5+"
  ))

HouseholdData$vacc_group <- factor(HouseholdData$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
HouseholdPlot <- HouseholdData %>% 
  filter(!is.na(value)) %>% 
  group_by(vacc_group) %>%
  count(value) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(value, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = value, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, alpha = 0.9, size = 1.3) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Household size [# Members]") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Children under 14 ------------------------------------------------------------------

# Processing of external survey data - all three groups
Children <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(total_hsld_size_persons_under_14, vacc_group) %>%
  mutate(total_hsld_size_persons_under_14 = case_when(
    total_hsld_size_persons_under_14 == 0 ~ "0",
    total_hsld_size_persons_under_14 == 1 ~ "1",
    total_hsld_size_persons_under_14 == 2 ~ "2",
    total_hsld_size_persons_under_14 == 3 ~ "3+",
    total_hsld_size_persons_under_14 == 4 ~ "3+"
  ))

Children$total_hsld_size_persons_under_14 <- factor(Children$total_hsld_size_persons_under_14, levels = c("0", "1", "2", "3+"))
Children$vacc_group <- factor(Children$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
ChildrenPlot <- Children %>% 
  filter(!is.na(total_hsld_size_persons_under_14)) %>% 
  group_by(vacc_group) %>%
  count(total_hsld_size_persons_under_14) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(total_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = total_hsld_size_persons_under_14, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, alpha = 0.9, size = 1.3) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Education --------------------------------------------------

# Processing of external survey data - all three groups
educationLevel <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(highest_educational_qualification, vacc_group) %>%
  mutate(highest_educational_qualification = case_when(
    highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
    highest_educational_qualification == "Realschulabschluss" ~ "Certification\nafter 10 years",
    highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
    highest_educational_qualification == "Anderer" ~ "Other/None"
  ))

educationLevel$highest_educational_qualification <- factor(educationLevel$highest_educational_qualification, 
                                                           levels = c("Higher Education", "Certification\nafter 10 years", 
                                                                      "Certification\nafter 9 years", "Other/None"))
educationLevel$vacc_group <- factor(educationLevel$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
EducationPlot <- educationLevel %>% 
  filter(!is.na(highest_educational_qualification)) %>% 
  filter(highest_educational_qualification != "Other") %>%
  group_by(vacc_group) %>%
  count(highest_educational_qualification) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(highest_educational_qualification, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = highest_educational_qualification, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, size = 1.3) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Education") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.7),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Occupation --------------------------------------------------

# Processing of external survey data - all three groups
currentOccupation <- ext_survey_df %>% 
  mutate(vacc_group = add_vacc_label(c19_vaccination_status)) %>%
  select(current_occupation, vacc_group) %>%
  mutate(current_occupation = case_when(
    current_occupation == "Ich bin in einem anderen Beruf tätig." ~ "Other",
    current_occupation == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector", 
    current_occupation == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
    current_occupation == "Ich bin arbeitssuchend." ~ "Unemployed", 
    current_occupation == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
    current_occupation == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector", 
    current_occupation == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
    current_occupation == "Ich möchte nicht antworten" ~ "Unknown",
    is.na(current_occupation) ~ "Unknown"
  )) %>%
  filter(current_occupation != "Unknown")

currentOccupation$vacc_group <- factor(currentOccupation$vacc_group, levels = c("Vaccinated", "Unvaccinated", "No Answer"))

# Creation of plot
OccupationPlot <- currentOccupation %>% 
  filter(!is.na(current_occupation)) %>% 
  group_by(vacc_group) %>%
  count(current_occupation) %>% 
  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill = vacc_group), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x = current_occupation, ymin = lci, ymax = uci, colour = vacc_group), 
                position = position_dodge(0.8), width = 0.3, size = 1.3) +
  scale_color_manual(values = palette_vacc_errorbars()) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette_vacc_bars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25,50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.7),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Layout and save plots
ggarrange(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, 
          labels = c("A", "B", "C", "D", "E", "F"), 
          nrow = 3, ncol = 2,
          font.label = list(size = 37), 
          heights = c(1,1,1.25), 
          common.legend = TRUE, 
          legend = "bottom")
ggsave("./plots/DemographicComparison_ByVaccStatus.pdf", dpi = 500, w = 24, h = 30)
ggsave("./plots/DemographicComparison_ByVaccStatus.png", dpi = 500, w = 24, h = 30)