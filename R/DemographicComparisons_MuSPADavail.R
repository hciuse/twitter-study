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

#Creation of palette for comparison of external survey, federal office of statistics, and MuSPAD
palette_surveyfedmuspad_bars <- function() {
  c("#9900CC", "#151515", "#990000")
}
palette_surveyfedmuspad_errorbars <- function() {
   c("#640085", "#000000", "#5c0000")
}

# Gender ------------------------------------------------------------------

# Processing of external survey data
GenderData <- ext_survey_df %>% select(gender) %>%
              mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer")) %>% 
                                                        filter(gender != "I Don't Want To Answer")

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))


# Processing of federal statistical office data
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/deutsche-nichtdeutsche-bevoelkerung-nach-geschlecht-deutschland.html [accessed: 2025-02-10]
GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "Source", "sum")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("diverse",	0, 0, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male", "diverse"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$sum <- as.integer(GenderDataStatBundesamt$sum)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

# Processing of MuSPAD data
GenderMus <- MuSPAD_s22 %>% select(sex) %>% count(sex) %>% filter(!is.na(sex)) %>% filter(sex != "")
GenderDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(GenderDataMuspad) <- c("gender", "n", "percent", "Source", "sum")
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("female", (GenderMus %>% filter(sex == "female"))$n, 100*(GenderMus %>% filter(sex == "female"))$n/sum(GenderMus$n), "MuSPAD", (GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("male",	(GenderMus %>% filter(sex == "male"))$n, 100*(GenderMus %>% filter(sex == "male"))$n/sum(GenderMus$n), "MuSPAD",(GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("diverse",	(GenderMus %>% filter(sex == "diverse"))$n, 100*(GenderMus %>% filter(sex == "diverse"))$n/sum(GenderMus$n),"MuSPAD", (GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad$gender <- factor(GenderDataMuspad$gender, levels = c("female", "male", "diverse"))
GenderDataMuspad$n <- as.integer(GenderDataMuspad$n)
GenderDataMuspad$sum <- as.integer(GenderDataMuspad$sum)
GenderDataMuspad$percent <- as.double(GenderDataMuspad$percent)

# Creation of plot
GenderPlot <- GenderData %>% count(gender) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% mutate(Source = "External Survey") %>% 
                  rbind(GenderDataStatBundesamt) %>%
                  rbind(GenderDataMuspad) %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
    geom_errorbar(aes(x=gender, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Gender") +
  scale_fill_manual(values = palette_surveyfedmuspad_bars()) +
  scale_color_manual(values = palette_surveyfedmuspad_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

#ggsave("./plots/Gender_Comparison.pdf", GenderPlot, dpi = 500, w = 9.5, h = 6)
#ggsave("./plots/Gender_Comparison.png", GenderPlot, dpi = 500, w = 9.5, h = 6)

# Age ---------------------------------------------------------------------

# Processing of external survey data
AgeData <- ext_survey_df %>% select(year_of_birth) %>% mutate(age = 2023-year_of_birth) %>%
          mutate(age_bracket = case_when(age < 20 ~ "18-39",
                                        age < 40 ~ "18-39",
                                        age < 60 ~ "40-59",
                                        age < 80 ~ "60-79",
                                        age < 100 ~ "80-99")) 
AgeData$age_bracket <- factor(AgeData$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

# Processing of federal statistical office data
# Data from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html [accessed: 2025-02-10]
# https://de.statista.com/statistik/daten/studie/1174053/umfrage/minderjaehrige-in-deutschland-nach-altersgruppen/#:~:text=Kinder%20und%20Jugendliche%20in%20Deutschland%20nach%20Altersgruppen%202023&text=Zum%2031.,sechs%20bis%20einschlie%C3%9Flich%2014%20Jahren. [accessed: 2025-02-10]
AgeDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeDataStatBundesamt) <- c("age_bracket", "n", "source", "sum")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("18-39", 84669326*0.188-14300000+84669326*0.245, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("40-59", 84669326*0.268, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("60-79", 84669326*0.226, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("80-99", 84669326*0.072, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt$n <- as.integer(AgeDataStatBundesamt$n)
AgeDataStatBundesamt$sum <- as.integer(AgeDataStatBundesamt$sum)
AgeDataStatBundesamt <- AgeDataStatBundesamt %>% mutate(percent = 100*n/sum)
AgeDataStatBundesamt$age_bracket <- factor(AgeDataStatBundesamt$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

# Processing of MuSPAD data
AgeMuspad <- MuSPAD_s22 %>% select(birth_date_yyyy, age, age_floor, age_group) %>% 
                        filter(age_floor != 17) %>% filter(!is.na(age_floor)) %>% filter(is.finite(age_floor)) %>%
                        mutate(age_bracket = case_when(age_floor >= 80 ~ "80-99",
                                                                age_floor >= 60 ~ "60-79",
                                                                age_floor >= 40 ~ "40-59",
                                                                age_floor >= 20 ~ "18-39",
                                                                age_floor < 20 ~ "18-39")) %>% count(age_bracket)
AgeDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(AgeDataMuspad) <- c("age_bracket", "n", "percent", "source", "sum")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("18-39", (AgeMuspad %>% filter(age_bracket=="18-39"))$n, 100*(AgeMuspad %>% filter(age_bracket=="18-39"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("40-59", (AgeMuspad %>% filter(age_bracket=="40-59"))$n, 100*(AgeMuspad %>% filter(age_bracket=="40-59"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("60-79", (AgeMuspad %>% filter(age_bracket=="60-79"))$n, 100*(AgeMuspad %>% filter(age_bracket=="60-79"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("80-99", (AgeMuspad %>% filter(age_bracket=="80-99"))$n, 100*(AgeMuspad %>% filter(age_bracket=="80-99"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad$n <- as.integer(AgeDataMuspad$n)
AgeDataMuspad$sum <- as.integer(AgeDataMuspad$sum)
AgeDataMuspad$percent <- as.double(AgeDataMuspad$percent)
AgeDataMuspad$age_bracket <- factor(AgeDataMuspad$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

# Creation of plot
AgePlot <- AgeData %>% filter(!is.na(age_bracket)) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(AgeDataStatBundesamt) %>%
            rbind(AgeDataMuspad) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(age_bracket, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  scale_color_manual(values = palette_surveyfedmuspad_errorbars())+
  theme_minimal() +
    theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Age Bracket") +
  scale_fill_manual(values = palette_surveyfedmuspad_bars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

#ggsave("./plots/Age_Comparison.pdf", dpi = 500, w = 9.5, h = 6)
#ggsave("./plots/Age_Comparison.png", dpi = 500, w = 9.5, h = 6)

# Household Size ----------------------------------------------------

# Processing of external survey data
HouseholdData <- ext_survey_df %>% select(hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, total_hsld_size_persons_under_14, number_of_children_under_18)
HouseholdData <- HouseholdData %>% pivot_longer(cols = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_", "total_hsld_size_persons_under_14", "number_of_children_under_18"))
HouseholdData <- HouseholdData %>% mutate(value = case_when(value == 1 ~ "1", value == 2 ~ "2", value == 3 ~ "3", value == 4 ~ "4", value >= 5 ~ "5+")) %>%
                                   mutate(name = case_when(name == "hsld_size_2019_" ~ "Household size 2019",
                                                          name == "hsld_size_03_2020_" ~ "Household size 3/20",
                                                          name == "hsld_size_summer_2021_" ~ "Household size Summer/21",
                                                          name == "hsld_size_01_2023_" ~ "Household size 1/23",
                                                          name == "total_hsld_size_persons_under_14" ~ "Children < 14 in household",
                                                          name == "number_of_children_under_18" ~ "# Children < 18"))
HouseholdData$name <- factor(HouseholdData$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))

# Processing of federal statistical office data
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Haushalte-Familien/Tabellen/1-1-privathaushalte-haushaltsmitglieder.html
HouseholdDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(HouseholdDataStatBundesamt) <- c("name", "value", "n", "percent", "Source", "sum")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency",84669326)
HouseholdDataStatBundesamt$n <- as.integer(HouseholdDataStatBundesamt$n)
HouseholdDataStatBundesamt$sum <- as.integer(HouseholdDataStatBundesamt$sum)
HouseholdDataStatBundesamt$percent <- as.double(HouseholdDataStatBundesamt$percent)
HouseholdDataStatBundesamt$name <- factor(HouseholdDataStatBundesamt$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))

# Processing of MuSPAD data
MuSPADHousehold <- MuSPAD_s22 %>%  
                         select(haushalt_gesamt, haushalt_gesamt_g) %>%
                         filter(haushalt_gesamt != 0) %>%
                         filter(!is.na(haushalt_gesamt)) %>%
                         mutate(haushalt_gesamt = case_when(haushalt_gesamt == 1 ~ "1", 
                         haushalt_gesamt == 2 ~ "2",
                         haushalt_gesamt == 3 ~ "3",
                         haushalt_gesamt == 4 ~ "4",
                         haushalt_gesamt >= 5 ~ "5+")) %>%
                         count(haushalt_gesamt)
HouseholdDataMuspad <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(HouseholdDataMuspad) <- c("name", "value", "n", "percent", "Source", "sum")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad$n <- as.integer(HouseholdDataMuspad$n)
HouseholdDataMuspad$sum <- as.integer(HouseholdDataMuspad$sum)
HouseholdDataMuspad$percent <- as.double(HouseholdDataMuspad$percent)
HouseholdDataMuspad$Source <- as.character(HouseholdDataMuspad$Source)
HouseholdDataMuspad$name <- factor(HouseholdDataMuspad$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))

# Creation of plot
HouseholdPlot <- HouseholdData %>% filter(name != "Children < 14 in household") %>%
                  filter(name != "# Children < 18") %>%  filter(!is.na(name)) %>% filter(!is.na(value)) %>% 
                  group_by(name) %>% count(value) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% mutate(Source = "External Survey") %>% 
                  rbind(HouseholdDataStatBundesamt) %>% 
                  rbind(HouseholdDataMuspad) %>% filter(name == "Household size 1/23") %>% filter(!is.na(Source)) %>%
                  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
 ggplot(aes(value, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha=0.8) +
  geom_errorbar(aes(x=value, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (Percentage)") +
  xlab("Household size [# Members]") +
  scale_fill_manual(values = palette_surveyfedmuspad_bars()) +
  scale_color_manual(values = palette_surveyfedmuspad_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

#ggsave("./plots/HouseholdSize.png", HouseholdPlot, dpi = 500, w = 9.5, h = 6)
#ggsave("./plots/HouseholdSize.pdf", HouseholdPlot, dpi = 500, w = 9.5, h = 6)

# Children under 14 ------------------------------------------------------------------

# Creation of external survey/MuSPAD color palette (no data available by federal statistical office)
palette_surveymuspad_bars <- function() {
  c("#9900CC", "#990000")
}

palette_surveymuspad_errorbars <- function() {
   c("#640085", "#5c0000")
}

# Processing of external survey data
Children <- ext_survey_df %>% select(total_hsld_size_persons_under_14) %>%
                            mutate(total_hsld_size_persons_under_14 = case_when(total_hsld_size_persons_under_14  == 0 ~ "0",
                            total_hsld_size_persons_under_14  == 1 ~ "1",
                            total_hsld_size_persons_under_14  == 2 ~ "2",
                            total_hsld_size_persons_under_14  == 3 ~ "3+",
                            total_hsld_size_persons_under_14  == 4 ~ "3+"))

# Processing of MuSPAD data
MuSPAD_s22 <- MuSPAD_s22 %>% rename(ChildrenUnder14 = household_under14) 
ChildrenMuspad <- MuSPAD_s22 %>% select(ChildrenUnder14) %>%
                            mutate(ChildrenUnder14 = case_when(ChildrenUnder14  == 0 ~ "0",
                            ChildrenUnder14  == 1 ~ "1",
                            ChildrenUnder14  == 2 ~ "2",
                            ChildrenUnder14  == 3 ~ "3+",
                            ChildrenUnder14  > 3 ~ "3+")) %>%
                            count(ChildrenUnder14) %>% 
                            filter(!is.na(ChildrenUnder14))
ChildrenDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(ChildrenDataMuspad) <- c("total_hsld_size_persons_under_14", "n", "percent", "Source", "sum")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("0", (ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("1",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("2",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("3+",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad$n <- as.integer(ChildrenDataMuspad$n)
ChildrenDataMuspad$sum <- as.integer(ChildrenDataMuspad$sum)
ChildrenDataMuspad$total_hsld_size_persons_under_14 <- factor(ChildrenDataMuspad$total_hsld_size_persons_under_14, levels = c("0", "1", "2", "3+"))
ChildrenDataMuspad$n <- as.integer(ChildrenDataMuspad$n)
ChildrenDataMuspad$percent <- as.double(ChildrenDataMuspad$percent)

# Creation of plot
ChildrenPlot <- Children %>% filter(!is.na(total_hsld_size_persons_under_14)) %>% 
                  count(total_hsld_size_persons_under_14) %>% 
                  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
                  mutate(Source = "External Survey") %>% 
                  rbind(ChildrenDataMuspad) %>% 
                  filter(!is.na(Source)) %>%
                  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(total_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x=total_hsld_size_persons_under_14, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette_surveymuspad_bars()) +
  scale_color_manual(values=palette_surveymuspad_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

#ggsave("./plots/Children_Comparison.png", ChildrenPlot, dpi = 500, w = 9.5, h = 6)
#ggsave("./plots/Children_Comparison.pdf", ChildrenPlot, dpi = 500, w = 9.5, h = 6)


# Education --------------------------------------------------

# Processing of external survey data
educationLevel <- ext_survey_df %>% select(highest_educational_qualification) %>%
                  mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                                                                                          highest_educational_qualification == "Realschulabschluss" ~ "Certification\nafter 10 years",
                                                                                          highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                                                                                          highest_educational_qualification == "Anderer" ~ "Other/None"))
educationLevel$highest_educational_qualification <- factor(educationLevel$highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

# Processing of federal statistical office data
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Bildungsstand/Tabellen/bildungsabschluss.html [accessed 2025-02-10]
EducationDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataStatBundesamt ) <- c("highest_educational_qualification", "n", "percent", "source", "sum")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Higher Education", 82000000*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 10 years", 82000000*0.3, 23.5+6.5, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 9 years", 82000000*0.286, 28.6, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Other/None", 82000000*0.077, 3.5+0.2+4.0, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt$n <- as.integer(EducationDataStatBundesamt $n)
EducationDataStatBundesamt$sum  <- as.integer(EducationDataStatBundesamt$sum)
EducationDataStatBundesamt $percent <- as.double(EducationDataStatBundesamt $percent)
EducationDataStatBundesamt $highest_educational_qualification <- factor(EducationDataStatBundesamt $highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

# Processing of MuSPAD data
Education <- MuSPAD_s22 %>% 
                        select(education) %>% 
                        filter(!is.na(education)) %>% 
                        filter(education != "") %>% 
                        filter(education != "Ich möchte nicht antworten") %>% 
                        mutate(education = case_when(education == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                        education == "Abitur/Fachhochschulabitur" ~ "Higher Education",
                        education == "Anderer" ~ "Other/None",
                        education == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                        education == "Haupt-/Volksschulabschluss" ~ "Certification\nafter 9 years",
                        education == "Keinen Schulabschluss" ~ "Other/None",
                        education == "Realschulabschluss" ~ "Certification\nafter 10 years")) %>% 
                        count(education) 
EducationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataMuspad) <- c("highest_educational_qualification", "n", "percent", "source", "sum")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Higher Education", (Education %>% filter(education == "Higher Education"))$n, 100*(Education %>% filter(education == "Higher Education"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification\nafter 10 years", (Education %>% filter(education == "Certification\nafter 10 years"))$n, 100*(Education %>% filter(education == "Certification\nafter 10 years"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification\nafter 9 years", (Education %>% filter(education == "Certification\nafter 9 years"))$n, 100*(Education %>% filter(education == "Certification\nafter 9 years"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Other/None", (Education %>% filter(education == "Other/None"))$n, 100*(Education %>% filter(education == "Other/None"))$n/sum(Education$n), "MuSPAD", sum(Education$n)) 
EducationDataMuspad$n <- as.integer(EducationDataMuspad$n)
EducationDataMuspad$sum <- as.integer(EducationDataMuspad$sum)
EducationDataMuspad$percent <- as.double(EducationDataMuspad$percent)
EducationDataMuspad$highest_educational_qualification <- factor(EducationDataMuspad$highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

# Creation of plot
EducationPlot <- educationLevel %>% filter(!is.na(highest_educational_qualification)) %>% 
            filter(highest_educational_qualification != "Other") %>%
            count(highest_educational_qualification) %>% 
            mutate(percent = 100 * n / sum(n), sum=sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(EducationDataMuspad) %>%
            rbind(EducationDataStatBundesamt) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(highest_educational_qualification, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
      geom_errorbar(aes(x=highest_educational_qualification, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Education") +
  scale_fill_manual(values = palette_surveyfedmuspad_bars()) +
  scale_color_manual(values = palette_surveyfedmuspad_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

#ggsave("./plots/EducationLevel_Comparison.pdf", EducationPlot, dpi = 500, w =9.5, h = 9)
#ggsave("./plots/EducationLevel_Comparison.png", EducationPlot, dpi = 500, w =9.5, h = 9)

# Occupation --------------------------------------------------

# Processing of external survey data
currentOccupation <- ext_survey_df %>% select(current_occupation) %>%
                    mutate(current_occupation = case_when(current_occupation == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                                                 current_occupation == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector", 
                                                                                 current_occupation == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                                                 current_occupation == "Ich bin arbeitssuchend." ~ "Unemployed", 
                                                                                 current_occupation == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                                                 current_occupation == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector", 
                                                                                 current_occupation == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                                                 current_occupation == "Ich möchte nicht antworten" ~ "Unknown",
                                                                                 is.na(current_occupation) ~ "Unknown")) %>%
                                            filter(current_occupation != "Unknown")

# Processing of MuSPAD data
OccupationMuSPAD <- MuSPAD_s22 %>% count(employment) %>% filter(employment != "") %>%
                    mutate(employment = case_when(employment == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                  employment == "Mutterschafts-, Erziehungsurlaub, Elternzeit oder sonstige Beurlaubung" ~ "Other",
                                                  employment == "Ich bin Rentner:In oder Pensionär:In" ~ "Retired",
                                                  employment == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                  employment == "Ich bin als Lehrer:In oder Erzieher:In tätig" ~ "Teaching Sector",
                                                  employment == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector",
                                                  employment == "Ich bin arbeitssuchend" ~ "Unemployed",
                                                  employment == "Ich bin arbeitssuchend." ~ "Unemployed",
                                                  employment == "Ich bin im Studium oder in der Ausbildung" ~ "Student",
                                                  employment == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                  employment == "Ich bin in einem anderen Beruf tätig" ~ "Other",
                                                  employment == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                  employment == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig" ~ "Medical Sector",
                                                  employment == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector")) %>%
                                                  group_by(employment) %>% summarise(n = sum(n))
OccupationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Other", (OccupationMuSPAD %>% filter(employment == "Other"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Other"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Teaching Sector", (OccupationMuSPAD %>% filter(employment == "Teaching Sector"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Teaching Sector"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Medical Sector", (OccupationMuSPAD %>% filter(employment == "Medical Sector"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Medical Sector"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Unemployed", (OccupationMuSPAD %>% filter(employment == "Unemployed"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Unemployed"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Student", (OccupationMuSPAD %>% filter(employment == "Student"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Student"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Retired", (OccupationMuSPAD %>% filter(employment == "Retired"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Retired"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
colnames(OccupationDataMuspad) <- c("current_occupation", "n", "percent", "source", "sum")
OccupationDataMuspad$n <- as.integer(OccupationDataMuspad$n)
OccupationDataMuspad$sum <- as.integer(OccupationDataMuspad$sum)
OccupationDataMuspad$percent <- as.double(OccupationDataMuspad$percent)

# Processing of federal statistical office data
#Data stems from https://de.statista.com/statistik/daten/studie/1099494/umfrage/beschaeftigte-in-deutschland-nach-berufsgruppen/ [accessed: 2025-02-10]
FedEmploymentAgency <- read_xlsx("./data/employment-germany-by-occupation-2023.xlsx", sheet = 2)
colnames(FedEmploymentAgency) <- c("occupation", "n")
OccupationDataFedEmploymentAgency <- data.frame(matrix(nrow = 0, ncol = 3))
other <- c("Berufe in Unternehmensführung, -organisation (Büro)", "Verkaufsberufe", "Verkehr, Logistik (außer Fahrzeugführung)", "Erziehung, soz., hauswirt. Berufe, Theologie", 
"Maschinen- und Fahrzeugtechnikberufe", "Reinigungsberufe", "Tourismus-, Hotel- und Gaststättenberufe", "FührerInnen von Fahrzeug- und Transportgeräten",
"Berufe in Finanzdienstleistungen, Rechnungswesen und Steuerberatung", "Berufe in Recht und Verwaltung", "Metallerzeugung, -bearbeitung, Metallbau", 
"Technische Forschungs-, Entwicklungs-, Konstruktions- und Produktionssteuerungsberufe", "Einkaufs-, Vertriebs- und Handelsberufe", "Mechatronik-, Energie- und Elektroberufe",                                             
"Informatik- und andere IKT-Berufe", "Lebensmittelherstellung und -verarbeitung", "Nichtmed. Gesundheits-, Körperpflege-/ Wellnessberufe, Medizintechnik", "Gebäude- und versorgungstechnische Berufe",    
"Hoch- und Tiefbauberufe", "Werbung, Marketing, kaufmännische und redaktionelle Medienberufe", "Kunststoff- und Holz- herstellung, -verarbeitung", "Schutz-, Sicherheits-, Überwachungsberufe",                                            
"(Innen-) Ausbauberufe", "Mathematik-, Biologie-, Chemie-, Physikberufe", "Keine Zuordnung möglich" , "Land-, Tier-, Forstwirtschaftsberufe", "Gartenbauberufe, Floristik",                                                           
"Bauplanung, Architektur, Vermessungsberufe","Papier-, Druckberufe, tech. Mediengestaltung" , "Darstellende, unterhaltende Berufe", "Sprach-/ Literatur-/ Geistes-/ Gesellschafts-/ Wirtschaftswissenschaften",         
"Textil- und Lederberufe","Rohstoffgewinnung, Glas-, Keramikverarbeitung","Produktdesign, Kunsthandwerk", "Geologie-, Geografie-, Umweltschutzberufe")
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Other", sum((FedEmploymentAgency %>% filter(occupation %in% other))$n)*1000, "Federal Employment Agency")
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Teaching Sector", (FedEmploymentAgency %>% filter(occupation == "Lehrende und ausbildende Berufe"))$n*1000, "Federal Employment Agency")
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Medical Sector", (FedEmploymentAgency %>% filter(occupation == "Medizinische Gesundheitsberufe"))$n*1000, "Federal Employment Agency")
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Unemployed", 2554982, "Federal Employment Agency") #https://statistik.arbeitsagentur.de/Statistikdaten/Detail/Aktuell/iiia4/zr-alo-bl/zr-alo-bl-dwolr-0-xlsx.xlsx?__blob=publicationFile
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Student", 2868300 + 1220000, "Federal Employment Agency") #https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Hochschulen/_inhalt.html https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Berufliche-Bildung/_inhalt.html
OccupationDataFedEmploymentAgency[nrow(OccupationDataFedEmploymentAgency) + 1, ] <- c("Retired", 21229000, "Federal Employment Agency") #https://www.deutsche-rentenversicherung.de/DRV/DE/Experten/Zahlen-und-Fakten/Statistiken-und-Berichte/statistiken-und-berichte_node.html
colnames(OccupationDataFedEmploymentAgency) <- c("current_occupation", "n", "source")
OccupationDataFedEmploymentAgency$n <- as.double(OccupationDataFedEmploymentAgency$n)
OccupationDataFedEmploymentAgency <- OccupationDataFedEmploymentAgency %>% mutate(sum = sum(n), percent = n/sum*100)

# Creation of plot
OccupationPlot <- currentOccupation %>% filter(!is.na(current_occupation)) %>% 
            count(current_occupation) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(OccupationDataFedEmploymentAgency) %>%
            rbind(OccupationDataMuspad) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "Federal Employment Agency", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8, alpha = 0.8) +
  geom_errorbar(aes(x=current_occupation, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  scale_color_manual(values = palette_surveyfedmuspad_errorbars()) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette_surveyfedmuspad_bars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

#ggsave("./plots/Occupation_Comparison.pdf", OccupationPlot, dpi = 500, w =9.5, h = 9)
#ggsave("./plots/Occupation_Comparison.png", OccupationPlot, dpi = 500, w =9.5, h = 9)

# Layout and save plots
ggarrange(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")
ggsave("./plots/DemographicComparison.pdf", dpi = 500, w = 24, h = 30)
ggsave("./plots/DemographicComparison.png", dpi = 500, w = 24, h = 30)
