library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(readxl)
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")
source("./R/Timeline.R")
source("./R/MuSPADPreprocessing.R")

palette_surveymuspadrki_bars <- function() {
  c("#9900CC", "#93A3b1", "#990000")
}

palette_surveymuspadrki_errorbars <- function() {
   c("#430059", "#1c2020", "#390000") 
}


# Vaccination Supplier ----------------------------------------------------

# Procession of external survey data
vaccinationData <- ext_survey_df %>% select(year_of_birth, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
vaccinationData <- na.omit(vaccinationData)
vaccinationData <- vaccinationData %>% pivot_longer(cols = c("c19_vaccination_details_vaccine_dose_1", "c19_vaccination_details_vaccine_dose_2", "c19_vaccination_details_vaccine_dose_3", "c19_vaccination_details_vaccine_dose_4"))
vaccinationData$value <- factor(vaccinationData$value, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "Andere", "Ich möchte nicht antworten", "Nicht zutreffend"))
vaccinationData <- vaccinationData %>% mutate(value_eng = case_when(value == "BioNTech" ~ "BioNTech",
                                                                    value == "Moderna" ~ "Moderna",
                                                                    value == "AstraZeneca" ~ "AstraZeneca", 
                                                                    value == "Janssen/ Johnson & Johnson" ~ "Janssen/Johnson &\nJohnson",
                                                                    value == "Gamaleya Sputnik V" ~ "Other",
                                                                    value == "Andere" ~ "Other",
                                                                    value == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                    value == "Nicht zutreffend" ~ "Does Not Apply"))
vaccinationData$value_eng <- factor(vaccinationData$value_eng, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson &\nJohnson", "Other", "I Don't Want To Answer", "Does Not Apply"))
vaccinationData <- vaccinationData %>% mutate(Impfserie = case_when(name == "c19_vaccination_details_vaccine_dose_1" ~ "1", 
                                                                    name == "c19_vaccination_details_vaccine_dose_2" ~ "2",
                                                                    name == "c19_vaccination_details_vaccine_dose_3" ~ "3",
                                                                    name == "c19_vaccination_details_vaccine_dose_4" ~ "4"))
vaccinationData$Impfserie <- factor(vaccinationData$Impfserie, levels = c("1", "2", "3", "4"))
vaccinationData <- vaccinationData %>% mutate(Source = "External Survey")
vaccinationData<- vaccinationData %>% select(Impfserie, value_eng, Source)

# Procession of RKI data
rkiVaccinations <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
rkiVaccinations <- rkiVaccinations %>% mutate(value_eng = case_when(Impfstoff == "Comirnaty" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Omicron XBB.1.5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.1" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.4-5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty bivalent (Original/Omikron)" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty-Kleinkinder" ~ "BioNTech",
                                                                    Impfstoff == "Jcovden" ~ "Janssen/Johnson &\nJohnson",
                                                                    Impfstoff == "Nuvaxovid" ~ "Other",
                                                                    Impfstoff == "Vaxzevria" ~ "AstraZeneca",
                                                                    Impfstoff == "Spikevax" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent (Original/Omikron)" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.1" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.4-5" ~ "Moderna",
                                                                    Impfstoff == "Valneva" ~ "Other",
                                                                    Impfstoff == "VidPrevtyn Beta" ~ "Other"))

rkiVaccinations <- rkiVaccinations %>% filter(Impfdatum < "2023-09-01") %>%
                  mutate(Source = "RKI") %>% filter(Impfserie %in% c(1,2,3,4))
rkiVaccinations <- rkiVaccinations %>% uncount(Anzahl) %>% select(Impfserie, value_eng, Source)

vaccinationData <- rbind(vaccinationData, rkiVaccinations)
vaccinationData <- vaccinationData %>% mutate(vaccineNo = case_when(Impfserie == "1" ~ "1st COVID-19 Vaccination Dosis",
                                                                    Impfserie == "2" ~ "2nd COVID-19 Vaccination Dosis",
                                                                    Impfserie == "3" ~ "3rd COVID-19 Vaccination Dosis",
                                                                    Impfserie == "4" ~ "4th COVID-19 Vaccination Dosis"
                                                                    ))

# Procession of MuSPAD data

VaccinationSupplierMuspad <- MuSPAD_df %>% select(s23_vacc_type_1, s23_vacc_type_2, s23_vacc_type_3, s23_vacc_type_4)
VaccinationSupplierMuspad1 <- VaccinationSupplierMuspad %>% count(s23_vacc_type_1) %>% filter(!is.na(s23_vacc_type_1)) %>%
                                                            mutate(s23_vacc_type_1 = case_when(s23_vacc_type_1 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = s23_vacc_type_1))  %>% filter(s23_vacc_type_1 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad2 <- VaccinationSupplierMuspad %>% count(s23_vacc_type_2) %>% filter(!is.na(s23_vacc_type_2)) %>%
                                                            mutate(s23_vacc_type_2 = case_when(s23_vacc_type_2 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = s23_vacc_type_2))  %>% filter(s23_vacc_type_2 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad3 <- VaccinationSupplierMuspad %>% count(s23_vacc_type_3) %>% filter(!is.na(s23_vacc_type_3)) %>%
                                                            mutate(s23_vacc_type_3 = case_when(s23_vacc_type_3 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = s23_vacc_type_3))  %>% filter(s23_vacc_type_3 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad4 <- VaccinationSupplierMuspad %>% count(s23_vacc_type_4) %>% filter(!is.na(s23_vacc_type_4)) %>%
                                                            mutate(s23_vacc_type_4 = case_when(s23_vacc_type_4 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = s23_vacc_type_4)) %>% filter(s23_vacc_type_4 != "keine (weitere) Impfung erhalten")

VaccinationSupplierDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(VaccinationSupplierDataMuspad) <- c("vaccineNo", "Source", "value_eng", "n", "percent")
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "BioNTech"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Moderna", (VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Moderna"))$n,100*(VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Moderna"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson &\nJohnson", (VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Other"))$n), 100*sum((VaccinationSupplierMuspad1 %>% filter(s23_vacc_type_1 == "Other"))$n)/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "BioNTech"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Moderna"))$n, 100* (VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Moderna"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson &\nJohnson", (VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Other"))$n), 100*sum((VaccinationSupplierMuspad2 %>% filter(s23_vacc_type_2 == "Other"))$n)/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "BioNTech"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Moderna"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Moderna"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson &\nJohnson", (VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Other"))$n), 100*sum((VaccinationSupplierMuspad3 %>% filter(s23_vacc_type_3 == "Other"))$n)/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "BioNTech"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Moderna"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Moderna"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson &\nJohnson", (VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Other"))$n), 100*sum((VaccinationSupplierMuspad4 %>% filter(s23_vacc_type_4 == "Other"))$n)/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad$n <- as.integer(VaccinationSupplierDataMuspad$n)
VaccinationSupplierDataMuspad$percent <- as.double(VaccinationSupplierDataMuspad$percent)

# Creation of plot
vaccinationData %>% filter(value_eng != "Does Not Apply") %>% filter(value_eng != "I Don't Want To Answer") %>% group_by(vaccineNo, Source) %>% count(value_eng) %>%
                    mutate(percent = 100 * n / sum(n)) %>% mutate(percent = round(percent, digits = 2)) %>%
                    rbind(VaccinationSupplierDataMuspad) %>% 
                    group_by(vaccineNo, Source) %>% 
                    mutate(value_eng = factor(value_eng, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson &\nJohnson", "Gamaleya Sputnik V", "Other", "I Don't Want To Answer", "Does Not Apply"))) %>%
                    mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                    mutate(lci = 100/sum(n)*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                    mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                    mutate(uci = 100/sum(n)*uci) %>%
                    mutate(Source = factor(Source, levels = c("External Survey", "RKI", "MuSPAD"))) %>%
ggplot(aes(value_eng, percent)) +
  geom_bar(aes(fill = Source), stat = "identity", position = "dodge", width = 0.95) +
  geom_errorbar(aes(x=value_eng, ymin=lci, ymax=uci, colour = Source), position = position_dodge(0.95), width = 0.5, alpha=0.9, size=1.3) +
  theme_minimal() +
  facet_wrap(~vaccineNo, nrow=2) +
  ylab("Share (Percentage)") +
  scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Vaccination Supplier") +
  scale_fill_manual(values = palette_surveymuspadrki_bars()) +
  scale_color_manual(values = palette_surveymuspadrki_errorbars()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(text = element_text(size = 55)) +
  theme(panel.spacing = unit(3, "lines")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("./plots/ShareVaccinationSupplier.pdf", dpi = 500, w = 24, h = 18)
ggsave("./plots/ShareVaccinationSupplier.png", dpi = 500, w = 24, h = 18)

# Number of Vaccinations -------------------------------------------------------------

# Procession of external survey data
vaccinationData <- ext_survey_df %>% select(year_of_birth, c19_vaccination_status, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
vaccinationData <- vaccinationData %>%  mutate(agegroup = case_when(2023-year_of_birth >= 80 ~ "80-99",
                                                                    2023-year_of_birth >= 60 ~ "60-79",
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
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 1 , "External Survey", "60-79")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 0 , "External Survey", "80-99") ##Use c19_vaccination_status to find unvaccinated
NotVacc$n <- as.double(NotVacc$n)                                                              

vaccinationData <- rbind(vaccinationData, NotVacc)
vaccinationData <- vaccinationData %>% filter(!is.na(agegroup)) %>% 
  mutate(groupsize = case_when(agegroup == "18-39" ~ 105+1,
                                agegroup == "40-59" ~ 351+8,
                                agegroup == "60-79"  ~ 88+1,
                                agegroup == "80-99"  ~ 2)) %>%
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
  c("#c084d4", "#b646db", "#9900CC", "#730099")
}

palette_survey_errorbars <- function() {
  c("#9900CC", "#730099", "#400155", "#260133")
}

survey_doses <- ggplot(vaccinationData %>%
filter(name != "Received 0 doses") %>%
            mutate(lci = groupsize*(n/groupsize - 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(lci = lci/groupsize) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = groupsize*(n/groupsize + 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(uci = uci/groupsize) %>%
                    mutate(uci = case_when(uci > 1 ~ 1, .default= uci)),
aes(x = name,  y = percent)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = agegroup)) +
  geom_errorbar(aes(x=name, ymin=lci, ymax=uci, colour = agegroup), position = position_dodge(0.9), width = 0.5, size=2) +
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette_survey_bars()) +
  scale_color_manual(values = palette_survey_errorbars()) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("External Survey") +
  theme(text = element_text(size = 50)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
        theme(plot.title = element_text(hjust = 0.5))

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
                    mutate(uci = uci/groupsize), 
aes(x = name,  y = percent)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = agegroup)) +
  geom_errorbar(aes(x=name, ymin=lci, ymax=uci, colour = agegroup), position = position_dodge(0.9), width = 0.5, size=2) +
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette_rki_bars()) +
  scale_color_manual(values = palette_rki_errorbars()) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("RKI") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        theme(plot.title = element_text(hjust = 0.5))

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
                    mutate(uci = uci/groupsize), 
aes(x = name,  y = percent)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = agegroup)) +
  geom_errorbar(aes(x=name, ymin=lci, ymax=uci, colour = agegroup), position = position_dodge(0.9), width = 0.5, size=2) +
  theme_minimal() +
  theme(text = element_text(size = 55)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette_muspad_bars()) +
  scale_color_manual(values = palette_muspad_errorbars()) +
  xlab("") +
  ylab("Share (Percentage)") +
  ggtitle("MuSPAD study") + 
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        theme(plot.title = element_text(hjust = 0.5)) 

# Layout and save plots
ggarrange(survey_doses, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), muspad_doses,  ggparagraph(text="   ", face = "italic", size = 14, color = "black"), rki_doses,  ggparagraph(text="   ", face = "italic", size = 14, color = "black"), timelineplot2, ncol = 1,  nrow = 7, labels=c("A", "", "", "", "", "", "B"), font.label = list(size = 37), heights=c(1,0.05,1,0.05,1, 0.05,0.5), widths=c(1, 1, 1, 1, 1,1,1))

ggsave("./plots/NoVaccinations_Comparison.pdf", dpi = 500, w = 24, h = 36)
ggsave("./plots/NoVaccinations_Comparison.png", dpi = 500, w = 24, h = 36)
