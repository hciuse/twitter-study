library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(here)

here()
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")

palette_recruiters_bars <- function(){
  c("#253494", "#ffffcc", "#7fcdbb", "#2c7fb8", "#c7e9b4", "#663300", "#151515")
}
palette_recruiters_errorbars <- function(){
  c("#1a2569", "#c9c99f", "#5e978a", "#1d577d", "#8ba37d", "#261300", "#000000")
}


# Gender ------------------------------------------------------------------

# Processing of external survey data
GenderData <- ext_survey_df %>% select(gender, ref, origin) %>% 
    filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
    mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                            ref == "4a76b" ~ "Recruiter 2",
                            ref == "008b5" ~ "Recruiter 3", 
                            ref == "7b598" ~ "Recruiter 4", 
                            ref == "6c8d7" ~ "Recruiter 5", 
                            ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)")) %>%
    mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer")) %>% 
                                                        filter(gender != "I Don't Want To Answer")

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))

GenderAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(GenderAdd) <- c("ref", "gender", "n", "percent")
    if(scenario == "Twitter"){
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Recruiter 2", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Recruiter 3", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Recruiter 5", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Recruiter 4", "diverse", 0, 0)
}

GenderAdd$n <- as.double(GenderAdd$n)
GenderAdd$percent <- as.double(GenderAdd$percent)
GenderAdd$ref <- as.character(GenderAdd$ref)
GenderAdd$gender <- as.character(GenderAdd$gender)

# Processing of federal statistical office data
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/deutsche-nichtdeutsche-bevoelkerung-nach-geschlecht-deutschland.html [accessed 2025-02-11]
GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "ref", "sum")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("diverse",	0, 0, "Federal Statistical Office, Federal Employment Agency", 84669326)
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male", "diverse"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$sum <- as.integer(GenderDataStatBundesamt$sum)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

# Creation of plot
GenderPlot <- GenderData %>% group_by(ref) %>% filter(!is.na(ref)) %>% count(gender) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
rbind(GenderDataStatBundesamt) %>%
rbind(GenderAdd) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  mutate(gender = factor(gender, levels = c("female", "male", "diverse"))) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=gender, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Gender") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        guides(fill=guide_legend(nrow=4), color=guide_legend(nrow=4))


# Age ---------------------------------------------------------------------

# Processing of external survey data
AgeData <- ext_survey_df %>% select(year_of_birth, ref, origin) %>% mutate(age = 2023-year_of_birth) %>%
            mutate(age_bracket = case_when(age < 20 ~ "18-39",
                                            age < 40 ~ "18-39",
                                            age < 60 ~ "40-59",
                                            age < 80 ~ "60-79",
                                            age < 100 ~ "80-99"))  %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                            ref ==  "6c8d7" ~ "Recruiter 5", 
                            ref == "7b598" ~ "Recruiter 4", 
                            ref == "008b5" ~ "Recruiter 3", 
                            ref == "4a76b" ~ "Recruiter 2",
                            ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)"))

AgeAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeAdd) <- c("ref", "age_bracket", "n", "percent")
if(scenario == "Twitter"){
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 3", "80-99", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 5", "60-79", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 5", "80-99", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 4", "60-79", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 4", "80-99", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Recruiter 1 (Mastodon)", "80-99", 0, 0)
}

AgeAdd$n <- as.double(AgeAdd$n)
AgeAdd$percent <- as.double(AgeAdd$percent)
AgeAdd$ref <- as.character(AgeAdd$ref)
AgeAdd$age_bracket <- as.character(AgeAdd$age_bracket)
 
# Processing of federal statistical office data
# Data from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html [accessed 2025-02-11]
# https://de.statista.com/statistik/daten/studie/1174053/umfrage/minderjaehrige-in-deutschland-nach-altersgruppen/#:~:text=Kinder%20und%20Jugendliche%20in%20Deutschland%20nach%20Altersgruppen%202023&text=Zum%2031.,sechs%20bis%20einschlie%C3%9Flich%2014%20Jahren. [accessed 2025-02-11]
AgeDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeDataStatBundesamt) <- c("age_bracket", "n", "ref", "sum")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("18-39", 84669326*0.188-14300000+84669326*0.245, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("40-59", 84669326*0.268, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("60-79", 84669326*0.226, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("80-99", 84669326*0.072, "Federal Statistical Office, Federal Employment Agency", 84669326-14300000)
AgeDataStatBundesamt$n <- as.integer(AgeDataStatBundesamt$n)
AgeDataStatBundesamt$sum <- as.integer(AgeDataStatBundesamt$sum)
AgeDataStatBundesamt <- AgeDataStatBundesamt %>% mutate(percent = 100*n/sum)
AgeDataStatBundesamt$age_bracket <- factor(AgeDataStatBundesamt$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

# Creation of plot
AgePlot <- AgeData %>% filter(!is.na(age_bracket)) %>% filter(!is.na(ref)) %>% group_by(ref) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(AgeAdd) %>%
            rbind(AgeDataStatBundesamt) %>%
              mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                mutate(lci = 100/sum*lci) %>%
                mutate(lci = case_when(lci < 0 ~ 0,
                                .default = lci)) %>%
                mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                mutate(uci = 100/sum*uci) %>%
                mutate(ref = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))) %>%
ggplot(aes(factor(age_bracket, levels = c("18-39", "40-59", "60-79", "80-99")), percent)) +
  geom_bar(aes(fill=ref), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = ref), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("") +
  ylab("Share (Percentage)") +
  xlab("Age Bracket (2023)") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #guides(fill=guide_legend(nrow=3,byrow=TRUE), color=guide_legend(nrow=3,byrow=TRUE)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt"))

# Household Size ----------------------------------------------------

# Processing of external survey data
HouseholdData <- ext_survey_df %>% select(ref, respondent_hsld_size_01_2023, origin) %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                            ref ==  "6c8d7" ~ "Recruiter 5", 
                            ref == "7b598" ~ "Recruiter 4", 
                            ref == "008b5" ~ "Recruiter 3", 
                            ref == "4a76b" ~ "Recruiter 2",
                            ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)"))

HouseholdData <- HouseholdData %>% mutate(respondent_hsld_size_01_2023 = case_when(respondent_hsld_size_01_2023 == 1 ~ "1", respondent_hsld_size_01_2023 == 2 ~ "2", respondent_hsld_size_01_2023 == 3 ~ "3", respondent_hsld_size_01_2023 == 4 ~ "4", respondent_hsld_size_01_2023 >= 5 ~ "5+"))

HouseholdAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(HouseholdAdd) <- c("ref", "respondent_hsld_size_01_2023", "n", "percent")
if(scenario == "Twitter"){
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("Recruiter 5", "3", 0, 0)
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("Recruiter 5", "4", 0, 0)
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("Recruiter 4", "5+", 0, 0)
}

HouseholdAdd$ref <- as.character(HouseholdAdd$ref)
HouseholdAdd$respondent_hsld_size_01_2023 <- as.character(HouseholdAdd$respondent_hsld_size_01_2023)
HouseholdAdd$n <- as.double(HouseholdAdd$n)
HouseholdAdd$percent <- as.double(HouseholdAdd$percent)

# Processing of federal statistical office data
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Haushalte-Familien/Tabellen/1-1-privathaushalte-haushaltsmitglieder.html [accessed 2025-02-11]
HouseholdDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(HouseholdDataStatBundesamt) <- c("respondent_hsld_size_01_2023", "n", "percent", "ref", "sum")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt$n <- as.integer(HouseholdDataStatBundesamt$n)
HouseholdDataStatBundesamt$sum <- as.integer(HouseholdDataStatBundesamt$sum)
HouseholdDataStatBundesamt$percent <- as.double(HouseholdDataStatBundesamt$percent)

HouseholdPlot <- HouseholdData %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(respondent_hsld_size_01_2023)) %>% 
                  count(respondent_hsld_size_01_2023) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(HouseholdAdd) %>%
                  rbind(HouseholdDataStatBundesamt)  %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0,
                                    .default = lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
 ggplot(aes(respondent_hsld_size_01_2023 , percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=respondent_hsld_size_01_2023, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Household size (# Members)") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Children under 14 ------------------------------------------------------------------

# Processing of external survey data
Children <- ext_survey_df  %>% select(ref, respondent_hsld_size_persons_under_14, origin) %>%
                        filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                        ref ==  "6c8d7" ~ "Recruiter 5", 
                        ref == "7b598" ~ "Recruiter 4", 
                        ref == "008b5" ~ "Recruiter 3", 
                        ref == "4a76b" ~ "Recruiter 2",
                        ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)")) %>%
                            mutate(respondent_hsld_size_persons_under_14 = case_when(respondent_hsld_size_persons_under_14  == 0 ~ "0",
                            respondent_hsld_size_persons_under_14  == 1 ~ "1",
                            respondent_hsld_size_persons_under_14  == 2 ~ "2",
                            respondent_hsld_size_persons_under_14  == 3 ~ "3+",
                            respondent_hsld_size_persons_under_14  == 4 ~ "3+"))

ChildrenAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(ChildrenAdd) <- c("ref", "respondent_hsld_size_persons_under_14", "n", "percent")
if(scenario == "Twitter"){
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Recruiter 3", "3+", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Recruiter 5", "1", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Recruiter 5", "2", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Recruiter 5", "3+", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Recruiter 4", "3+", 0, 0)
}

ChildrenAdd$ref <- as.character(ChildrenAdd$ref)
ChildrenAdd$respondent_hsld_size_persons_under_14 <- as.character(ChildrenAdd$respondent_hsld_size_persons_under_14)
ChildrenAdd$n <- as.double(ChildrenAdd$n)
ChildrenAdd$percent <- as.double(ChildrenAdd$percent)

ChildrenPlot <- Children %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(respondent_hsld_size_persons_under_14)) %>% 
                  count(respondent_hsld_size_persons_under_14) %>% filter(!is.na(respondent_hsld_size_persons_under_14)) %>%
                  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(ChildrenAdd) %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
                 mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                 mutate(uci = 100/sum*uci) %>%
ggplot(aes(respondent_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=respondent_hsld_size_persons_under_14, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  #ylab("Share (Percentage)") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) 

# Education / Occupation --------------------------------------------------

# Processing external survey data
educationLevel <- ext_survey_df %>% filter(!is.na(ref))  %>% select(highest_educational_qualification, ref, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                            ref ==  "6c8d7" ~ "Recruiter 5", 
                            ref == "7b598" ~ "Recruiter 4", 
                            ref == "008b5" ~ "Recruiter 3", 
                            ref == "4a76b" ~ "Recruiter 2",
                            ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)"))    

educationLevel <- educationLevel %>% mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                                                                                          highest_educational_qualification == "Realschulabschluss" ~ "Certification\nafter 10 years",
                                                                                          highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                                                                                          highest_educational_qualification == "Anderer" ~ "Other/None"))

EducationAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(EducationAdd) <- c("ref", "highest_educational_qualification", "n", "percent")
if(scenario == "Twitter"){
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Recruiter 2", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Recruiter 3", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Recruiter 5", "Certification\nafter 10 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Recruiter 5", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Recruiter 4", "Certification\nafter 9 years", 0, 0)
}

EducationAdd$ref <- as.character(EducationAdd$ref)
EducationAdd$highest_educational_qualification <- as.character(EducationAdd$highest_educational_qualification)
EducationAdd$n <- as.double(EducationAdd$n)
EducationAdd$percent <- as.double(EducationAdd$percent)

# Processing federal statistical office data
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Bildungsstand/Tabellen/bildungsabschluss.html [accessed 2025-02-11]
EducationDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataStatBundesamt ) <- c("highest_educational_qualification", "n", "percent", "ref", "sum")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Higher Education", 83166711*0.335*100, 33.5, "Federal Statistical Office, Federal Employment Agency", 83166711)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 10 years", 83166711*0.3*100, 23.5+6.5,"Federal Statistical Office, Federal Employment Agency", 83166711)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 9 years", 83166711*0.286*100, 28.6, "Federal Statistical Office, Federal Employment Agency", 83166711)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Other/None", 83166711*0.077*100, 3.5+0.2+4.0, "Federal Statistical Office, Federal Employment Agency", 83166711)
EducationDataStatBundesamt$n <- as.double(EducationDataStatBundesamt $n)
EducationDataStatBundesamt$sum  <- as.integer(EducationDataStatBundesamt$sum)
EducationDataStatBundesamt$percent <- as.double(EducationDataStatBundesamt $percent)
EducationDataStatBundesamt$highest_educational_qualification <- factor(EducationDataStatBundesamt $highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

EducationPlot <- educationLevel %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(highest_educational_qualification)) %>% 
            count(highest_educational_qualification) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(EducationAdd) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(lci = 100/sum*lci) %>%
            mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
            mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(uci = 100/sum*uci) %>% 
            rbind(EducationDataStatBundesamt) %>%
ggplot(aes(factor(highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None")), percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=highest_educational_qualification, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Statistical Office, Federal Employment Agency"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Education") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7)) +
  guides(fill=guide_legend(nrow=4), color=guide_legend(nrow=4))

#Occupation

# Processing external survey data
currentOccupation <- ext_survey_df %>% filter(!is.na(ref))  %>% select(ref, current_occupation, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Recruiter 1 (Twitter)",
                            ref ==  "6c8d7" ~ "Recruiter 5", 
                            ref == "7b598" ~ "Recruiter 4", 
                            ref == "008b5" ~ "Recruiter 3", 
                            ref == "4a76b" ~ "Recruiter 2",
                            ref == "dec9d" & origin == "6080d" ~ "Recruiter 1 (Mastodon)"))

currentOccupation <- currentOccupation %>% mutate(current_occupation = case_when(current_occupation == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                                                 current_occupation == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector", 
                                                                                 current_occupation == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                                                 current_occupation == "Ich bin arbeitssuchend." ~ "Unemployed", 
                                                                                 current_occupation == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                                                 current_occupation == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector", 
                                                                                 current_occupation == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                                                 current_occupation == "Ich möchte nicht antworten" ~ "Unknown",
                                                                                 is.na(current_occupation) ~ "Unknown")) %>%
                                            filter(current_occupation != "Unknown")

OccupationAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(OccupationAdd) <- c("ref", "current_occupation", "n", "percent")
if(scenario == "Twitter"){
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 3", "Unemployed", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 5", "Medical Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 5", "Retired", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 5", "Student", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 5", "Teaching Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 5", "Unemployed", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 4", "Medical Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 4", "Student", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Recruiter 4", "Unemployed", 0, 0)
}

OccupationAdd$ref <- as.character(OccupationAdd$ref)
OccupationAdd$current_occupation <- as.character(OccupationAdd$current_occupation)
OccupationAdd$n <- as.double(OccupationAdd$n)
OccupationAdd$percent <- as.double(OccupationAdd$percent)

# Processing federal statistical office data
# Data stems from https://de.statista.com/statistik/daten/studie/1099494/umfrage/beschaeftigte-in-deutschland-nach-berufsgruppen/ [accessed 2025-02-11]
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
colnames(OccupationDataFedEmploymentAgency) <- c("current_occupation", "n", "ref")
OccupationDataFedEmploymentAgency$n <- as.double(OccupationDataFedEmploymentAgency$n)
OccupationDataFedEmploymentAgency <- OccupationDataFedEmploymentAgency %>% mutate(sum = sum(n), percent = n/sum*100)


OccupationPlot <- currentOccupation %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(current_occupation)) %>% 
            count(current_occupation) %>% filter(!is.na(current_occupation)) %>%
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            rbind(OccupationAdd) %>%
            rbind(OccupationDataFedEmploymentAgency) %>%
              mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(lci = 100/sum*lci) %>%
            mutate(lci = case_when(lci <= 0 ~ 0,
                lci == 100 ~ 0,
                .default = lci)) %>%
            mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(uci = 100/sum*uci) %>%
ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Employment Agency"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=current_occupation, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "Federal Employment Agency"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_colour_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 50)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))+
    guides(fill=guide_legend(nrow=7), color=guide_legend(nrow=7))


# Layout and save plots

ggarrange(GenderPlot, AgePlot, HouseholdPlot, labels = c("A", "B", "C"), nrow = 3, ncol = 1, font.label = list(size = 37), common.legend = TRUE, legend = "bottom")

ggsave("./plots/DemographicComparison_TwitterRecruiter_FirstFour.pdf", dpi = 500, w = 24, h = 37)
ggsave("./plots/DemographicComparison_TwitterRecruiter_FirstFour.png", dpi = 500, w = 24, h = 37)

ggarrange(EducationPlot, OccupationPlot, ChildrenPlot, labels = c("A", "B", "C"), nrow = 3, ncol = 1, font.label = list(size = 37), common.legend = TRUE, legend = "bottom", heights = c(1,1,0.75))

ggsave("./plots/DemographicComparison_TwitterRecruiter_FinalTwo.pdf", dpi = 500, w = 24, h = 37)
ggsave("./plots/DemographicComparison_TwitterRecruiter_FinalTwo.png", dpi = 500, w = 24, h = 37)











