library(table1)
library(dplyr)
library(readr)

# Load external survey data
survey_data <- readRDS(file = "./data/cleaned_data.rds")
external_survey <- survey_data %>%
  select(gender, num_c19_infs, year_of_birth, origin) %>% 
  mutate(
    age = 2023 - year_of_birth,
    subsample_group = case_when(
      origin == "b73c2" ~ "Twitter",
      origin == "6080d" ~ "Mastodon",
      TRUE ~ NA_character_  # Exclude other origins
    )
  ) %>%
  filter(!is.na(subsample_group)) %>%  # Keep only Twitter & Mastodon
  mutate(
    gender = case_when(
      gender == "Männlich" ~ "male",
      gender == "Weiblich" ~ "female",
      gender == "Divers" ~ "other",
      TRUE ~ "no answer"
    ),
    num_c19_infs = case_when(
      num_c19_infs == "Nie" ~ "0",
      num_c19_infs == "Einmal" ~ "1",
      num_c19_infs == "Ich möchte nicht antworten" ~ "no answer",
      TRUE ~ "2+"  
    ),
    sample_group = "External Survey"
  ) %>%
  select(-origin)

# Load MuSPAD data (from MuSPADPreprocessing.R)
source("./R/MuSPADPreprocessing.R")
muspad_data <- MuSPAD_s22 %>%
  select(sex, birth_date_yyyy, positive_test) %>%
  mutate(
    gender = case_when(
      is.na(sex) | sex == "" ~ "no answer",
      sex == "diverse" ~ "other",
      TRUE ~ sex
    ),
    age = 2023 - birth_date_yyyy,
    num_c19_infs = case_when(
      positive_test == "Nie" ~ "0",
      positive_test == "Einmal" ~ "1",
      positive_test == "" ~ "no answer",
      TRUE ~ "2+"  
    ),
    sample_group = "MuSPAD",
    subsample_group = "MuSPAD"
  ) %>%
  rename(year_of_birth = birth_date_yyyy) %>%
  select(-sex, -positive_test)

# Merge datasets (WITHOUT adding an aggregated External Survey total)
data <- bind_rows(external_survey, muspad_data)

# Convert categorical variables to factors
data$sample_group <- factor(data$sample_group, levels = c("External Survey", "MuSPAD"))
data$subsample_group <- factor(data$subsample_group, levels = c("Twitter", "Mastodon", "MuSPAD"))

# Remove variable labels if present
for(i in seq_along(data)) {
  attributes(data)$variable.labels[i] <- ""
}

# Assign proper labels for Table 1
label(data$gender) <- "Sex"
label(data$num_c19_infs) <- "Number of COVID-19 Infections"
label(data$age) <- "Age"
label(data$sample_group) <- "Sample Group"

# Create Table 1 comparing Twitter, Mastodon, and MuSPAD
table1(~ age + gender + num_c19_infs | subsample_group, data = data, test = TRUE, test.type = "t")





