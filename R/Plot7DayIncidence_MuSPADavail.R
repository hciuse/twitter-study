# Author: S. Paltra, contact: paltra@tu-berlin.de

library(here)
library(tidyverse)

here() 
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")
source("./R/MuSPADPreprocessing.R")
source("./R/IncidencePlot.R")

# Incidence Plot without Bootstrapping ----------------------------------------------------

# Processing of external survey data
ext_survey_df <- ext_survey_df %>% select(num_c19_infs, date_f1_inf, f1_pcr_doc, f1_pcr_center, date_s2_inf, s2_pcr_doc, s2_pcr_center, date_t3_inf, t3_pcr_doc, t3_pcr_center, year_of_birth) %>% 
                mutate(age = 2023-year_of_birth) %>%
                mutate(age_bracket = case_when(age < 35 ~ "15-34",
                                                age < 60 ~ "35-59",
                                                age < 80 ~ "60-79",
                                                age < 100 ~ "80+")) %>% 
                filter(!is.na(num_c19_infs)) %>%
                select(-num_c19_infs)
    
# Application of function
IncidencePlot(bootstrapping = "no", MuSPADavail = "yes")

# Incidence Plot with Bootstrapping ----------------------------------------------------

# Creation of age groups
# Based on https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html
eighteen_thirtynine <- 31.9
fourty_fitynine <- 32.2
sixty_seventynine <- 27.2 + 8.7 #Moved 80+ year olds to 60-79 year olds as there are so few (only 2) respondents in the external survey in that age group that they weirdly influence the sample others
age_groups <- c("18-39", "40-59", "60+")


#Processing of external survey data
data <- data.frame(matrix(nrow = 0, ncol = 13))
colnames(data) <- c("date_f1_inf","f1_pcr_doc","f1_pcr_center","date_s2_inf","s2_pcr_doc","s2_pcr_center","date_t3_inf","t3_pcr_doc","t3_pcr_center","year_of_birth","age","age_bracket", "iteration")

for(i in 1:1000){
    for(age_group in age_groups){

        raw_data <- readRDS(file = "./data/cleaned_data.rds")
        ext_survey_df <- raw_data %>% select(num_c19_infs, date_f1_inf, f1_pcr_doc, f1_pcr_center, date_s2_inf, s2_pcr_doc, s2_pcr_center, date_t3_inf, t3_pcr_doc, t3_pcr_center, year_of_birth) %>% 
                    mutate(age = 2023-year_of_birth) %>%
                    mutate(age_bracket = case_when(age < 39 ~ "18-39",
                                                age < 60 ~ "40-59",
                                                age < 100 ~ "60+")) 
        ext_survey_df <- ext_survey_df %>% filter(!is.na(num_c19_infs)) %>%
        select(-num_c19_infs) %>% filter(age_bracket == age_group) %>% mutate(iteration = i)

        if(age_group == "18-39"){
        size = round(867*eighteen_thirtynine/100)
        }
        if(age_group == "40-59"){
        size = round(867*fourty_fitynine/100)
        }
        if(age_group == "60+"){
        size = round(867*sixty_seventynine/100)
        }
        if(age_group == "80+"){
        size = round(867*eightyplus/100)
        }

        slices <- slice_sample(ext_survey_df, n = size, replace = TRUE)

        data <- rbind(data, slices)
    }
}

ext_survey_df <- data

# Processing of MuSPAD data
data <- data.frame(matrix(nrow = 0, ncol = ncol(MuSPAD_df)))
colnames(data) <- colnames(MuSPAD_df)

for(i in 1:1000){
for(age_group in age_groups){
    MuSPAD <- MuSPAD_df %>% 
    mutate(firstinfection = make_date(MuSPAD_df$s22_positive_PCR_year_1, MuSPAD_df$s22_positive_PCR_month_1, MuSPAD_df$s22_positive_PCR_day_1)) %>%
    mutate(secondinfection = make_date(MuSPAD_df$s22_positive_PCR_year_2, MuSPAD_df$s22_positive_PCR_month_2, MuSPAD_df$s22_positive_PCR_day_2)) %>%
    mutate(thirdinfection = make_date(MuSPAD_df$s22_positive_PCR_year_3, MuSPAD_df$s22_positive_PCR_month_3, MuSPAD_df$s22_positive_PCR_day_3)) %>%
    select(s22_birth_date_yyyy, firstinfection, secondinfection, thirdinfection, s23_test_covid_2023, w22_positive_PCR_day_1) %>%
    mutate(age = 2023-s22_birth_date_yyyy) %>%
              mutate(age_bracket = case_when(age < 39 ~ "18-39",
                                             age < 60 ~ "40-59",
                                             age < 100 ~ "60+")) %>% mutate(iteration = i)

        if(age_group == "18-39"){
        size = round(9921*eighteen_thirtynine/100)
        }
        if(age_group == "40-59"){
        size = round(9921*fourty_fitynine/100)
        }
        if(age_group == "60+"){
        size = round(9921*sixty_seventynine/100)
        }
        if(age_group == "80+"){
        size = round(9921*eightyplus/100)
        }

        slices <- slice_sample(MuSPAD, n = size, replace = TRUE)

        data <- rbind(data, slices)
    }
}

MuSPAD_df <- data

# Application of function
IncidencePlot(bootstrapping = "yes", MuSPADavail = "yes")