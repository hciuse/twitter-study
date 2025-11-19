library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(readxl)
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")
source("./R/Timeline.R")
source("./R/MuSPADPreprocessing.R") #Todo: Update once repo has been reorganized

# Number of infections ----------------------------------------------------

# Creation of palette for comparison of twittwr, mastodon, external survey, MuSPAD, and COSMO
palette_twittermastodonsurvey_bars <- function() {
  c("#41b6c4", "#663300", "#9900CC", "#990000", "#CC3300")
}

palette_twittermastodonsurvey_errorbars <- function() {
  c("#2d7e87", "#261300", "#640085", "#5c0000", "#771f01")
}

# Procession of Twitter data
InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1191, 28.25, "Twitter", 1191+2310+716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1",	2310, 54.77, "Twitter", 1191+2310+716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 716, 16.98, "Twitter", 1191+2310+716)
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)
InfectionsDataTwitter$sum <- as.double(InfectionsDataTwitter$sum)

# Procesion of Mastodon data
InfectionsDataMastodon <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMastodon) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("0", 1802*0.37, 100*1802*0.37/(1802*0.37+1802*0.49+1802*0.12), "Mastodon", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("1",	1802*0.49, 100*1802*0.49/(1802*0.37+1802*0.49+1802*0.12), "Mastodon", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("2+", 1802*0.12, 100*1802*0.12/(1802*0.37+1802*0.49+1802*0.12), "Mastodon", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon$num_c19_infs_eng <- factor(InfectionsDataMastodon$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataMastodon$n <- as.integer(InfectionsDataMastodon$n)
InfectionsDataMastodon$percent <- as.double(InfectionsDataMastodon$percent)
InfectionsDataMastodon$sum <- as.double(InfectionsDataMastodon$sum)

# Procession of external survey data
ext_survey_df <- ext_survey_df %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2+",
                                                                    num_c19_infs == "Dreimal" ~ "2+",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "2+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

ext_survey_df$num_c19_infs_eng <- factor(ext_survey_df$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of MuSPAD data
InfectionsMuspad <- MuSPAD_df %>% select(w22_positive_test, s23_test_covid_2023)  %>% 
                                  mutate(w22_positive_test = case_when(w22_positive_test == "Nie" ~ "0",
                                                                    w22_positive_test == "Einmal" ~ "1",
                                                                    w22_positive_test == "Zweimal" ~ "2+",
                                                                    w22_positive_test == "Dreimal" ~ "2+",
                                                                    w22_positive_test == "Mehr als dreimal" ~ "2+")) %>%
                                  mutate(s23_positive_test = case_when((!is.na(s23_test_covid_2023) &  w22_positive_test == "0") ~ "1",
                                  (!is.na(s23_test_covid_2023) &  w22_positive_test == "1") ~ "2+",
                                  .default = w22_positive_test)) %>% 
                                  count(s23_positive_test)
InfectionsMuspad <- InfectionsMuspad %>% filter(!is.na(s23_positive_test))
InfectionsDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMuspad) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("0", (InfectionsMuspad %>% filter(s23_positive_test=="0"))$n, 100*(InfectionsMuspad %>% filter(s23_positive_test=="0"))$n/sum(InfectionsMuspad$n), "MuSPAD", sum(InfectionsMuspad$n))
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("1",	(InfectionsMuspad %>% filter(s23_positive_test=="1"))$n, 100*(InfectionsMuspad %>% filter(s23_positive_test=="1"))$n/sum(InfectionsMuspad$n), "MuSPAD", sum(InfectionsMuspad$n))
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("2+", (InfectionsMuspad %>% filter(s23_positive_test=="2+"))$n, 100*(InfectionsMuspad %>% filter(s23_positive_test=="2+"))$n/sum(InfectionsMuspad$n), "MuSPAD", sum(InfectionsMuspad$n))
InfectionsDataMuspad$num_c19_infs_eng <- factor(InfectionsDataMuspad$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataMuspad$n <- as.integer(InfectionsDataMuspad$n)
InfectionsDataMuspad$percent <- as.double(InfectionsDataMuspad$percent)
InfectionsDataMuspad$sum <- as.double(InfectionsDataMuspad$sum)
 
# Procession of COSMO data
# Data comes from https://projekte.uni-erfurt.de/cosmo2020/files/COSMO_W70.pdf [accessed: 2025-02-12]
InfectionsDataCOSMO <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataCOSMO) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("0", 1003*0.5, 50, "COSMO", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("1",	1003*0.42, 42, "COSMO", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("2+", 1003*0.08, 8, "COSMO", 1003)
InfectionsDataCOSMO$num_c19_infs_eng <- factor(InfectionsDataCOSMO$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataCOSMO$n <- as.integer(InfectionsDataCOSMO$n)
InfectionsDataCOSMO$percent <- as.double(InfectionsDataCOSMO$percent)
InfectionsDataCOSMO$sum <- as.double(InfectionsDataCOSMO$sum)

#Upper panel of Fig. 2
#Confidence intervals based on: http://www.stat.yale.edu/Courses/1997-98/101/catinf.htm
# Create a data frame for the sample size labels
# Define new facet labels with sample sizes
facet_labels <- c(
  "Twitter" = "Twitter\n(N = 4,217)",
  "Mastodon" = "Mastodon\n(N = 1,764)",
  "External Survey" = "External Survey\n(N = 866)",
  "MuSPAD" = "MuSPAD\n(N = 4,997)",
  "COSMO" = "COSMO\n(N = 1,003)"
)

# Create the plot with custom facet labels
upper_panel <- ext_survey_df %>%
  filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  count(num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(Source = "External Survey") %>%
  mutate(sum = sum(n)) %>%
  rbind(InfectionsDataTwitter) %>%
  rbind(InfectionsDataMuspad) %>%
  rbind(InfectionsDataCOSMO) %>%
  rbind(InfectionsDataMastodon) %>%
  mutate(Source = factor(Source, levels = c("Twitter", "Mastodon", "External Survey", "MuSPAD", "COSMO"))) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge2", width = 0.85) +
  theme_minimal() +
  facet_wrap(~Source, nrow = 1, labeller = labeller(Source = facet_labels)) +
  theme(panel.spacing = unit(1, "cm")) +
  ylab("Share (Percentage)") +
  ggtitle("Number of Infections") +
  xlab("") +
  scale_fill_manual(values = palette_twittermastodonsurvey_bars()) +
  scale_color_manual(values = palette_twittermastodonsurvey_errorbars()) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 0.5),
    breaks = c(0, 12.5, 25, 37.5, 50, 75, 100)
  ) +
  theme(
    text = element_text(size = 50),
    legend.position = "none",
    legend.title = element_blank(),
    legend.background = element_rect("white"),
    axis.ticks.x = element_line(size = 0.9),
    axis.ticks.y = element_line(size = 1),
    axis.ticks.length = unit(20, "pt"),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggarrange(upper_panel, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), timelineplot, nrow = 3, labels = c("A", "", "B"), font.label = list(size = 37), heights = c(1,0.01,0.5))

ggsave("./plots/NoInfections_Comparison_NoCI.pdf", dpi = 500, w = 24, h = 18)
ggsave("./plots/NoInfections_Comparison_NoCI.png", dpi = 500, w = 24, h = 18)

# Number of infections (by recruiter) ----------------------------------------------------

palette_recruiters_bars <- function(){
  c("#253494", "#ffffcc", "#7fcdbb", "#2c7fb8", "#c7e9b4", "#663300")
}
palette_recruiters_errorbars <- function(){
  c("#1a2569", "#c9c99f", "#5e978a", "#1d577d", "#8ba37d", "#261300")
}

# Procession of data collected by recruiter 1
InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "recruiter", "sum")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 2120*(0.3030), 2120*(0.3030)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Recruiter 1 (Twitter)", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 2120*(0.5270), 2120*(0.5270)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Recruiter 1 (Twitter)", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 2120*(0.1390), 2120*(0.1390)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Recruiter 1 (Twitter)", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of data collected by recruiter 5
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 101*(0.1780), 101*(0.1780)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "Recruiter 5", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 101*(0.5740), 101*(0.5740)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "Recruiter 5", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 101*(0.2180), 101*(0.2180)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "Recruiter 5", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of data collected by recruiter 4
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 111*(0.2610), 111*(0.2610)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Recruiter 4", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 111*(0.5410), 111*(0.5410)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Recruiter 4", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 111*(0.1530), 111*(0.1530)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Recruiter 4", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of data collected by recruiter 3
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 371*(0.2210), 371*(0.2210)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Recruiter 3", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 371*(0.5690), 371*(0.5690)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Recruiter 3", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 371*(0.1640), 371*(0.1640)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Recruiter 3", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of data collected by recruiter 2
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1667*(0.2520), 1667*(0.2520)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Recruiter 2", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 1667*(0.5180), 1667*(0.5180)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Recruiter 2", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 1667*(0.1930),  1667*(0.1930)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Recruiter 2", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

# Procession of data collected by recruiter 1 (Mastodon)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1802*(0.37), 1802*(0.37)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Recruiter 1 (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 1802*(0.49), 1802*(0.49)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Recruiter 1 (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 1802*(0.1200), 1802*(0.1200)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Recruiter 1 (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)
InfectionsDataTwitter$percent <- 100*(InfectionsDataTwitter$percent)
InfectionsDataTwitter$sum <- as.double(InfectionsDataTwitter$sum)

# Creation of plot
no_ci_plot <- InfectionsDataTwitter %>% group_by(recruiter) %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(aes(fill=factor(recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Number of Infections (Raw)") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_color_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.5), breaks = c(0,12.5,25, 37.5, 50,75,100)) +
  theme(text = element_text(size = 33)) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect("white")) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

ggsave("./plots/NoInfections_Comparison_Recruiter_NoCI.pdf", dpi = 500, w = 10, h = 7.5)
ggsave("./plots/NoInfections_Comparison_Recruiter_NoCI.png", dpi = 500,  w = 10, h = 7.5)

ggarrange(no_ci_plot, glm_plot, labels = c("A", "B"), common.legend = TRUE, legend = "bottom")
ggsave("./plots/NoInfections_Comparison_Recruiter_NoCI_GLM.pdf", dpi = 500, w = 20, h = 7.5)
ggsave("./plots/NoInfections_Comparison_Recruiter_NoCI_GLM.png", dpi = 500,  w = 20, h = 7.5)
