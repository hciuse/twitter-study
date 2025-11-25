library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(readxl)
library(here)
library(ggpattern)

# Author: S. Paltra, contact: paltra@tu-berlin.de

ext_survey_df <- readRDS(file = here("data", "cleaned_data.rds"))
source(here("R", "Timeline.R"))
source(here("R", "MuSPADPreprocessing.R"))

# Number of Vaccinations -------------------------------------------------------------

# Procession of external survey data
vaccinationData <- ext_survey_df %>% select(year_of_birth, c19_vaccination_status, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
vaccinationData <- vaccinationData %>%  mutate(agegroup = case_when(2023-year_of_birth >= 60 ~ "60+",
                                                                    2023-year_of_birth >= 40 ~ "40-59",
                                                                    2023-year_of_birth >= 18 ~ "18-39"
)) %>%
  mutate(dose_1_received = case_when(c19_vaccination_details_vaccine_dose_1 == "BioNTech" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_1 == "Moderna" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_1 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_1 == "AstraZeneca" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_1 == "Andere" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_1 == "Nicht zutreffend" ~ "No",
                                     c19_vaccination_details_vaccine_dose_1 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                     .default =  "NA")) %>%
  mutate(dose_2_received = case_when(c19_vaccination_details_vaccine_dose_2 == "BioNTech" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_2 == "Moderna" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_2 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_2 == "AstraZeneca" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_2 == "Andere" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_2 == "Nicht zutreffend" ~ "No",
                                     c19_vaccination_details_vaccine_dose_2 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                     .default =  "NA")) %>%
  mutate(dose_3_received = case_when(c19_vaccination_details_vaccine_dose_3 == "BioNTech" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_3 == "Moderna" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_3 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_3 == "AstraZeneca" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_3 == "Andere" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_3 == "Nicht zutreffend" ~ "No",
                                     c19_vaccination_details_vaccine_dose_3 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                     .default =  "NA")) %>%
  mutate(dose_4_received = case_when(c19_vaccination_details_vaccine_dose_4 == "BioNTech" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_4 == "Moderna" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_4 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_4 == "AstraZeneca" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_4 == "Andere" ~ "Yes",
                                     c19_vaccination_details_vaccine_dose_4 == "Nicht zutreffend" ~ "No",
                                     c19_vaccination_details_vaccine_dose_4 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                     .default =  "NA"))

vaccinationData$dose_1_received <- factor(vaccinationData$dose_1_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_2_received <- factor(vaccinationData$dose_2_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_3_received <- factor(vaccinationData$dose_3_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_4_received<- factor(vaccinationData$dose_4_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))

vaccinationData <- vaccinationData %>% group_by(agegroup) %>% pivot_longer(cols=c(dose_1_received, dose_2_received, dose_3_received, dose_4_received)) %>%
  filter(value %in% c("Yes", "Not Vaccinated")) %>% 
  mutate(Source = "External Survey") %>%
  mutate(name = case_when(name == "dose_1_received" ~ "Received at\nleast 1 dose",
                          name == "dose_2_received" ~ "Received at\nleast 2 doses",
                          name == "dose_3_received" ~ "Received at\nleast 3 doses",
                          name == "dose_4_received" ~ "Received at\nleast 4 doses")) %>% count(name) %>% mutate(Source = "External Survey") 
NotVacc <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(NotVacc) <- c("name", "n", "Source", "agegroup")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 1, "External Survey", "18-39")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 8, "External Survey", "40-59")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 1, "External Survey", "60+")
NotVacc$n <- as.double(NotVacc$n)                                                              

vaccinationData <- rbind(vaccinationData, NotVacc)
vaccinationData <- vaccinationData %>% filter(!is.na(agegroup)) %>% 
  mutate(groupsize = case_when(agegroup == "18-39" ~ 105+1,
                               agegroup == "40-59" ~ 351+8,
                               agegroup == "60+"  ~ 88+1+2)) %>%
  group_by(agegroup, name) %>% mutate(percent = n/groupsize) %>% select(n, name, percent, Source, agegroup, groupsize)

# Procession of RKI data
Rki <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/refs/heads/main/Archiv/2023-09-12_Deutschland_Impfquoten_COVID-19.csv") %>%
  filter(Bundesland == "Deutschland")
RkiVacc <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(RkiVacc) <- c("name", "percent", "Source", "agegroup", "groupsize")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 0 doses", 100-Rki$Impfquote_18bis59_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 1 dose", Rki$Impfquote_18bis59_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 2 doses", Rki$Impfquote_18bis59_gi,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 3 doses", Rki$Impfquote_18bis59_boost1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 4 doses", Rki$Impfquote_18bis59_boost2,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 0 doses", 100-Rki$Impfquote_60plus_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 1 dose", Rki$Impfquote_60plus_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 2 doses", Rki$Impfquote_60plus_gi,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 3 doses", Rki$Impfquote_60plus_boost1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at\nleast 4 doses", Rki$Impfquote_60plus_boost2,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc$percent <- as.double(RkiVacc$percent)
RkiVacc$percent <- RkiVacc$percent/100
RkiVacc$groupsize <- as.double(RkiVacc$groupsize)
RkiVacc <- RkiVacc %>% mutate(n= groupsize*percent)

# Procession of MuSPAD data
MuSPADVacc <- MuSPAD_df %>% select(c(s22_birth_date_yyyy, s23_vacc_COVID_2019_2023, s23_vacc_type_1, s23_vacc_type_2, s23_vacc_type_3, s23_vacc_type_4))
MuSPADVacc <- MuSPADVacc %>% mutate(s23_vacc_type_1 = case_when(s23_vacc_type_1 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                                                s23_vacc_type_1 == NA ~ NA,
                                                                s23_vacc_type_1 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
  mutate(s23_vacc_type_2 = case_when(s23_vacc_type_2 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                     s23_vacc_type_2 == NA ~ NA,
                                     s23_vacc_type_2 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
  mutate(s23_vacc_type_3 = case_when(s23_vacc_type_3 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                     s23_vacc_type_3 == NA ~ NA,
                                     s23_vacc_type_3 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
  mutate(s23_vacc_type_4 = case_when(s23_vacc_type_4 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                     s23_vacc_type_4 == NA ~ NA,
                                     s23_vacc_type_4 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
  mutate(agegroup = case_when(2023-s22_birth_date_yyyy >= 80 ~ "80-99",
                              2023-s22_birth_date_yyyy >= 60 ~ "60-79",
                              2023-s22_birth_date_yyyy >= 40 ~ "40-59",
                              2023-s22_birth_date_yyyy >= 18 ~ "18-39"
  ))                   
MuSPADVacc <- MuSPADVacc %>% pivot_longer(cols = c(s23_vacc_type_1, s23_vacc_type_2, s23_vacc_type_3, s23_vacc_type_4)) %>% 
  group_by(name, value, agegroup) %>% 
  count() %>% 
  filter(value %in% c("Yes", "Nein")) %>%
  mutate(name = case_when(name == "s23_vacc_COVID_2019_2023" ~ "Received 0 doses",
                          name == "s23_vacc_type_1" ~ "Received at\nleast 1 dose",
                          name == "s23_vacc_type_2" ~ "Received at\nleast 2 doses",
                          name == "s23_vacc_type_3" ~ "Received at\nleast 3 doses",
                          name == "s23_vacc_type_4" ~ "Received at\nleast 4 doses")) %>%
  mutate(Source = "MuSPAD")

NotVacc <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(NotVacc) <- c("name", "n", "Source", "agegroup")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 30, "MuSPAD", "18-39")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 80, "MuSPAD", "40-59")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 68 , "MuSPAD", "60-79")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 6 , "MuSPAD", "80-99") ##Use c19_vaccination_status to find unvaccinated
NotVacc$n <- as.double(NotVacc$n)                                                              

MuSPADVacc <- rbind(MuSPADVacc, NotVacc)

MuSPADVacc <- MuSPADVacc %>% 
  mutate(groupsize = case_when(agegroup == "18-39" ~ 639 + 30,
                               agegroup == "40-59" ~ 1403 + 80,
                               agegroup == "60-79" ~ 1678 + 68,
                               agegroup == "80-99" ~ 133+6)) %>%
  group_by(agegroup, name) %>%
  mutate(percent = n/groupsize)
MuSPADVacc$percent <- as.double(MuSPADVacc$percent)

# Creation of external survey plot

palette_survey_bars <- function() {
  c("#c084d4", "#b646db", "#9900CC")
}
palette_survey_errorbars <- function() {
  c("#9900CC", "#730099", "#400155")
}
survey_doses <- ggplot(
  vaccinationData %>%
    filter(name != "Received 0 doses") %>%
    mutate(lci = groupsize * (n / groupsize - 1.96 *
                                (((n / groupsize * (1 - n / groupsize)) / groupsize
                                )^0.5))) %>%
    mutate(lci = lci / groupsize) %>%
    mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
    mutate(uci = groupsize * (n / groupsize + 1.96 *
                                (((n / groupsize * (1 - n / groupsize)) / groupsize
                                )^0.5))) %>%
    mutate(uci = uci / groupsize) %>%
    mutate(uci = case_when(uci > 1 ~ 1, .default = uci)) %>%
    mutate(
      agegroup = factor(agegroup),  # Ensure it's a factor
      bar_color = palette_survey_bars()[as.numeric(agegroup)], 
      errorbar_color = palette_survey_errorbars()[as.numeric(agegroup)]
    ),
  aes(x = name, y = percent)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    aes(color = bar_color, group = agegroup),
    linewidth = 1
  ) +
  
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    color = NA,
    aes(pattern_fill = agegroup, group = agegroup),
    pattern = "stripe",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.02
  ) +
  geom_errorbar(
    aes(
      x = name,
      ymin = lci,
      ymax = uci,
      color = errorbar_color,
      group = agegroup
    ),
    position = position_dodge(0.9),
    width = 0.5,
    size = 2,
    show.legend = FALSE
  ) +
  
  scale_pattern_fill_manual(values = palette_survey_bars()) +
  scale_color_identity() +
  
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect("white")
  ) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("External Survey (N = 554)") +
  theme(text = element_text(size = 50)) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(5, "pt")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Creation of RKI plot
palette_rki_bars <- function() {
  c("#c0cad2", "#6d7b88")
}
palette_rki_errorbars <- function() {
  c("#3e464d", "#1f2326")
}
rki_doses <- ggplot(RkiVacc %>%
                      filter(name != "Received 0 doses") %>%
                      mutate(lci = groupsize*(n/groupsize - 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                      mutate(lci = lci/groupsize) %>%
                      mutate(lci = case_when(lci < 0 ~ 0, lci == 1 ~ 0, .default= lci)) %>%
                      mutate(uci = groupsize*(n/groupsize + 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                      mutate(uci = uci/groupsize) %>%
                      mutate(
                        agegroup = factor(agegroup),
                        bar_color = palette_rki_bars()[as.numeric(agegroup)],
                        errorbar_color = palette_rki_errorbars()[as.numeric(agegroup)]
                      ), 
                    aes(x = name,  y = percent)) +
  geom_bar(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    aes(color = bar_color, group = agegroup),
    linewidth = 1
  ) +
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    color = NA,
    aes(pattern_fill = agegroup, group = agegroup),
    pattern = "stripe",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.02
  ) +
  geom_errorbar(
    aes(
      x = name,
      ymin = lci,
      ymax = uci,
      color = errorbar_color,
      group = agegroup
    ),
    position = position_dodge(0.9),
    width = 0.5,
    size = 2,
    show.legend = FALSE
  ) +
  
  scale_pattern_fill_manual(values = palette_rki_bars()) +
  scale_color_identity() +
  
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect("white")) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("RKI (Population)") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Creation of MuSPAD plot
palette_muspad_bars <- function() {
  c("#e79393", "#d24747", "#cc0202", "#6d0101")
}
palette_muspad_errorbars <- function() {
  c("#cc0202", "#6d0101", "#350000", "#260000")
}
muspad_doses <- ggplot(MuSPADVacc %>%
                         filter(!is.na(agegroup)) %>%
                         filter(name != "Received 0 doses") %>%
                         mutate(lci = groupsize*(n/groupsize - 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                         mutate(lci = lci/groupsize) %>%
                         mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                         mutate(uci = groupsize*(n/groupsize + 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                         mutate(uci = uci/groupsize) %>%
                         mutate(
                           agegroup = factor(agegroup),
                           bar_color = palette_muspad_bars()[as.numeric(agegroup)],
                           errorbar_color = palette_muspad_errorbars()[as.numeric(agegroup)]
                         ), 
                       aes(x = name,  y = percent)) +
  geom_bar(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    aes(color = bar_color, group = agegroup),
    linewidth = 1
  ) +
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge2(width = 0.9, preserve = "single"),
    fill = NA,
    color = NA,
    aes(pattern_fill = agegroup, group = agegroup),
    pattern = "stripe",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.02
  ) +
  geom_errorbar(
    aes(
      x = name,
      ymin = lci,
      ymax = uci,
      color = errorbar_color,
      group = agegroup
    ),
    position = position_dodge(0.9),
    width = 0.5,
    size = 2,
    show.legend = FALSE
  ) +
  
  scale_pattern_fill_manual(values = palette_muspad_bars()) +
  scale_color_identity() +
  
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect("white")) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("MuSPAD study (N = 4037)") + 
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))
# Layout and save plots
ggarrange(survey_doses, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), muspad_doses,  ggparagraph(text="   ", face = "italic", size = 14, color = "black"), rki_doses,  ggparagraph(text="   ", face = "italic", size = 14, color = "black"), timelineplot2, ncol = 1,  nrow = 7, labels=c("A", "", "", "", "", "", "B"), font.label = list(size = 37), heights=c(1,0.05,1,0.05,1, 0.05,0.5), widths=c(1, 1, 1, 1, 1,1,1))

ggsave(here("plots", "NoVaccinations_Comparison_No8099.pdf"), dpi = 500, w = 24, h = 36)

ggsave(here("plots", "NoVaccinations_Comparison_No8099.png"), dpi = 500, w = 24, h = 36)