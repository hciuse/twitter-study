library(tidyverse)
library(here)

# Author: LST
ext_survey_df <- readRDS(file = here("data", "cleaned_data.rds"))
source(here("R", "MuSPADPreprocessing.R"))

# Number of infections ----------------------------------------------------

# Procession of Twitter data
InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1191, 28.25, "Twitter", 1191 +
                                                                2310 + 716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 2310, 54.77, "Twitter", 1191 +
                                                                2310 + 716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 716, 16.98, "Twitter", 1191 +
                                                                2310 + 716)
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng,
                                                 levels = c("0", "1", "2+"))
InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)
InfectionsDataTwitter$sum <- as.double(InfectionsDataTwitter$sum)

# Procesion of Mastodon data
InfectionsDataMastodon <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMastodon) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c(
  "0",
  1802 * 0.37,
  100 * 1802 * 0.37 / (1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12),
  "Mastodon",
  1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12
)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c(
  "1",
  1802 * 0.49,
  100 * 1802 * 0.49 / (1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12),
  "Mastodon",
  1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12
)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c(
  "2+",
  1802 * 0.12,
  100 * 1802 * 0.12 / (1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12),
  "Mastodon",
  1802 * 0.37 + 1802 * 0.49 + 1802 * 0.12
)
InfectionsDataMastodon$num_c19_infs_eng <- factor(InfectionsDataMastodon$num_c19_infs_eng,
                                                  levels = c("0", "1", "2+"))
InfectionsDataMastodon$n <- as.integer(InfectionsDataMastodon$n)
InfectionsDataMastodon$percent <- as.double(InfectionsDataMastodon$percent)
InfectionsDataMastodon$sum <- as.double(InfectionsDataMastodon$sum)

# Procession of external survey data
ext_survey_df <- ext_survey_df %>% mutate(
  num_c19_infs_eng = case_when(
    num_c19_infs == "Nie" ~ "0",
    num_c19_infs == "Einmal" ~ "1",
    num_c19_infs == "Zweimal" ~ "2+",
    num_c19_infs == "Dreimal" ~ "2+",
    num_c19_infs == "Mehr als dreimal" ~ "2+",
    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"
  )
)

ext_survey_df$num_c19_infs_eng <- factor(ext_survey_df$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of MuSPAD data
InfectionsMuspad <- MuSPAD_df %>% select(w22_positive_test, s23_test_covid_2023)  %>%
  mutate(
    w22_positive_test = case_when(
      w22_positive_test == "Nie" ~ "0",
      w22_positive_test == "Einmal" ~ "1",
      w22_positive_test == "Zweimal" ~ "2+",
      w22_positive_test == "Dreimal" ~ "2+",
      w22_positive_test == "Mehr als dreimal" ~ "2+"
    )
  ) %>%
  mutate(s23_positive_test = case_when((!is.na(s23_test_covid_2023) &
                                          w22_positive_test == "0") ~ "1",
                                       (!is.na(s23_test_covid_2023) &
                                          w22_positive_test == "1") ~ "2+",
                                       .default = w22_positive_test
  )) %>%
  count(s23_positive_test)
InfectionsMuspad <- InfectionsMuspad %>% filter(!is.na(s23_positive_test))
InfectionsDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMuspad) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c(
  "0",
  (InfectionsMuspad %>% filter(s23_positive_test == "0"))$n,
  100 * (InfectionsMuspad %>% filter(s23_positive_test == "0"))$n / sum(InfectionsMuspad$n),
  "MuSPAD",
  sum(InfectionsMuspad$n)
)
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c(
  "1",
  (InfectionsMuspad %>% filter(s23_positive_test == "1"))$n,
  100 * (InfectionsMuspad %>% filter(s23_positive_test == "1"))$n / sum(InfectionsMuspad$n),
  "MuSPAD",
  sum(InfectionsMuspad$n)
)
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c(
  "2+",
  (InfectionsMuspad %>% filter(s23_positive_test == "2+"))$n,
  100 * (InfectionsMuspad %>% filter(s23_positive_test == "2+"))$n / sum(InfectionsMuspad$n),
  "MuSPAD",
  sum(InfectionsMuspad$n)
)
InfectionsDataMuspad$num_c19_infs_eng <- factor(InfectionsDataMuspad$num_c19_infs_eng,
                                                levels = c("0", "1", "2+"))
InfectionsDataMuspad$n <- as.integer(InfectionsDataMuspad$n)
InfectionsDataMuspad$percent <- as.double(InfectionsDataMuspad$percent)
InfectionsDataMuspad$sum <- as.double(InfectionsDataMuspad$sum)

# Procession of COSMO data
# Data comes from https://projekte.uni-erfurt.de/cosmo2020/files/COSMO_W70.pdf [accessed: 2025-02-12]
InfectionsDataCOSMO <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataCOSMO) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("0", 1003 * 0.5, 50, "COSMO", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("1", 1003 * 0.42, 42, "COSMO", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("2+", 1003 * 0.08, 8, "COSMO", 1003)
InfectionsDataCOSMO$num_c19_infs_eng <- factor(InfectionsDataCOSMO$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataCOSMO$n <- as.integer(InfectionsDataCOSMO$n)
InfectionsDataCOSMO$percent <- as.double(InfectionsDataCOSMO$percent)
InfectionsDataCOSMO$sum <- as.double(InfectionsDataCOSMO$sum)

InfectionsDataExternal <- ext_survey_df %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  count(num_c19_infs_eng)

# Chi-squared tests comparing each source to MuSPAD reference distribution

# External survey chi-squared test
ext_ctest <- chisq.test(InfectionsDataExternal$n, p = InfectionsDataMuspad$n /
                          sum(InfectionsDataMuspad$n))
w_ext <- sqrt(ext_ctest$statistic / sum(InfectionsDataExternal$n))

# Twitter chi-squared test
twitter_ctest <- chisq.test(InfectionsDataTwitter$n, p = InfectionsDataMuspad$n /
                              sum(InfectionsDataMuspad$n))
w_twitter <- sqrt(twitter_ctest$statistic / sum(InfectionsDataTwitter$n))

# Mastodon chi-squared test
mastodon_ctest <- chisq.test(InfectionsDataMastodon$n, p = InfectionsDataMuspad$n /
                               sum(InfectionsDataMuspad$n))
w_mastodon <- sqrt(mastodon_ctest$statistic / sum(InfectionsDataMastodon$n))

# COSMO chi-squared test
cosmo_ctest <- chisq.test(InfectionsDataCOSMO$n, p = InfectionsDataMuspad$n /
                            sum(InfectionsDataMuspad$n))
w_cosmo <- sqrt(cosmo_ctest$statistic / sum(InfectionsDataCOSMO$n))
