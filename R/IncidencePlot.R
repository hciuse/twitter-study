library(tidyverse)
library(MMWRweek)
library(here)
library(ggpubr)

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
ext_survey_df <- readRDS(file = "./data/cleaned_data.rds")
ext_survey_df <-  ext_survey_df %>% mutate(age = 2023-year_of_birth) %>%
  mutate(age_bracket = case_when(age < 35 ~ "15-34",
                                 age < 60 ~ "35-59",
                                 age < 80 ~ "60-79",
                                 age < 100 ~ "80+"))
source("./R/MuSPADPreprocessing.R")
source("./R/timeline.R")

# The following function constructs the 7-Day-Incidence/100,000 plot
# This function is used in Plot7DayIncidence.R

IncidencePlot <- function(bootstrapping = "no", MuSPADavail = "yes"){
  if(bootstrapping == "no"){
   age_groups <- c("15-34", "35-59", "60-79", "80+", "00+")
  }
  if(bootstrapping == "yes"){
   age_groups <- c("00+")
  }
  for(age_group in age_groups){
    if(age_group != "00+"){
      ext_survey_df_t <- ext_survey_df %>% filter(age_bracket == age_group)
    }
    if(age_group == "00+"){
      ext_survey_df_t <- ext_survey_df
    }
    no_time_infections <- ext_survey_df_t %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
    no_time_infections <- no_time_infections %>% filter((name == "date_f1_inf" & f1_pcr_doc == "Ja") | (name == "date_f1_inf" & f1_pcr_center == "Ja") | (name == "date_s2_inf" & s2_pcr_doc == "Ja") | (name == "date_s2_inf" & s2_pcr_center == "Ja") | (name == "date_t3_inf" & t3_pcr_doc == "Ja") | (name == "date_t3_inf" & t3_pcr_center == "Ja"))
    no_time_infections <- no_time_infections %>%
    filter(!is.na(value)) %>%
    filter(value > "2020-01-01")
    no_time_infections <- no_time_infections %>% rename(
                                   "CounterInfection" = "name",
                                   "DateInfection" = "value")

    no_time_infections <- no_time_infections %>%
    mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
    mutate(date = MMWRweek2Date(MMWRyear = year,
                            MMWRweek = week+1,
                            MMWRday = 1))

    if(bootstrapping == "no"){
      count_no_infections <- no_time_infections %>% group_by(date) %>% count()
      colnames(count_no_infections) <- c("Date", "CountPer1096")
      count_no_infections <- count_no_infections %>% 
                            ungroup() %>% # https://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html
                            mutate(Incidence100000 = CountPer1096 / nrow(ext_survey_df_t)*100000) %>%
                            mutate(lci = nrow(ext_survey_df_t)*(CountPer1096/nrow(ext_survey_df_t) - 1.96*(((CountPer1096/nrow(ext_survey_df_t)*(1-CountPer1096/nrow(ext_survey_df_t)))/nrow(ext_survey_df_t))^0.5))) %>%
                            mutate(lci = lci/nrow(ext_survey_df_t)*100000) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, 
                                                      .default = lci)) %>%
                            mutate(uci = nrow(ext_survey_df_t)*(CountPer1096/nrow(ext_survey_df_t) + 1.96*(((CountPer1096/nrow(ext_survey_df_t)*(1-CountPer1096/nrow(ext_survey_df_t)))/nrow(ext_survey_df_t))^0.5))) %>%
                            mutate(uci = uci/nrow(ext_survey_df_t)*100000) %>%
                            mutate(Incidence100000 = CountPer1096 / nrow(ext_survey_df_t)*100000)
      DatesInfections <- unique(count_no_infections$Date)
      dates <- c()
      date <- as.Date("2020-01-05")
      while(date < "2023-10-01"){
          dates <- append(dates, date)
          date <- date + 7
      }
      for(date in dates){ 
            date <- as.Date(date)
            if(date %in% unique(count_no_infections$Date)){
            }else{
                row <- c(date, 0, 0, 0, 0)
                count_no_infections <- rbind(count_no_infections, row)
            }
        }
    }
    if(bootstrapping == "yes"){
      count_no_infections <- no_time_infections %>% group_by(date, iteration) %>% count()
      colnames(count_no_infections) <- c("Date", "iteration", "CountPer1096")
      count_no_infections <- count_no_infections %>% 
                            ungroup() %>% 
                            mutate(Incidence100000_it = CountPer1096/867*100000)
      DatesInfections <- unique(count_no_infections$Date)
      dates <- c()
      date <- as.Date("2020-01-05")
      while(date < "2023-10-01"){
          dates <- append(dates, date)
          date <- date + 7
      }
      for(i in 1:length(unique(ext_survey_df$iteration))){
        for(date in dates){ 
            count_no_infections_reduced <- count_no_infections %>% filter(iteration == i)
            date <- as.Date(date)
            if(date %in% unique(count_no_infections_reduced$Date)){
            }else{
                row <- c(date, i, 0, 0, 0, 0)
                count_no_infections <- rbind(count_no_infections, row)
            }
        }
      }
        count_no_infections <- count_no_infections %>% group_by(Date) %>% summarize(lci = quantile(Incidence100000_it, probs =0.025)[[1]], uci = quantile(Incidence100000_it, probs =0.975)[[1]],  Incidence100000 = mean(Incidence100000_it))
    } 
   
   #Processing of RKI data
    rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
    if(age_group != "00+"){
    rkidata <- rkidata %>% 
            filter(Altersgruppe == age_group) %>% 
            filter(Meldedatum < "2023-08-30") %>%
            mutate(weekday = wday(Meldedatum)) %>%
            filter(weekday == 1)

    rkidata <- rkidata %>% select(Meldedatum, `Inzidenz_7-Tage`)
    colnames(rkidata) <- c("Date", "Incidence100000")
    }else if(age_group == "00+"){
    rkidata <- rkidata %>% 
        filter(Altersgruppe %in% c("15-34", "35-59", "60-79", "80+")) %>% 
        filter(Meldedatum < "2023-08-30") %>%
        mutate(weekday = wday(Meldedatum)) %>%
        filter(weekday == 1)
    rkidata <- rkidata %>% group_by(Meldedatum) %>% summarise(infections = sum(`Faelle_7-Tage`), population = sum(Bevoelkerung))
    rkidata <- rkidata %>% mutate(Incidence100000 = infections/population*100000)
    colnames(rkidata)[1] <- "Date"
    }
    rkidata <- rkidata %>% mutate(lci = Incidence100000, uci = Incidence100000, DataSet = "RKI") %>% select(Date, Incidence100000, lci, uci, DataSet)
  
    
    count_no_infections <- count_no_infections %>% 
                            select(Date, Incidence100000, lci, uci) %>%
                            mutate(DataSet = "External Survey")
    count_no_infections <- rbind(count_no_infections, rkidata)

    #Processing of MuSPAD data
    if(MuSPADavail == "yes"){
      if(bootstrapping == "no"){
        MuSPAD <- MuSPAD_df %>% 
        mutate(firstinfection = make_date(MuSPAD_df$s22_positive_PCR_year_1, MuSPAD_df$s22_positive_PCR_month_1, MuSPAD_df$s22_positive_PCR_day_1)) %>%
        mutate(secondinfection = make_date(MuSPAD_df$s22_positive_PCR_year_2, MuSPAD_df$s22_positive_PCR_month_2, MuSPAD_df$s22_positive_PCR_day_2)) %>%
        mutate(thirdinfection = make_date(MuSPAD_df$s22_positive_PCR_year_3, MuSPAD_df$s22_positive_PCR_month_3, MuSPAD_df$s22_positive_PCR_day_3)) %>%
        select(s22_birth_date_yyyy, firstinfection, secondinfection, thirdinfection, s23_test_covid_2023, w22_positive_PCR_day_1) %>%
        mutate(age = 2023-s22_birth_date_yyyy) %>%
                  mutate(age_bracket = case_when(age < 35 ~ "15-34",
                                                age < 60 ~ "35-59",
                                                age < 80 ~ "60-79",
                                                age < 100 ~ "80+"))
      }

      if(age_group != "00+"){
        MuSPAD <- MuSPAD %>% filter(age_bracket == age_group)
      }

      MuSPAD <- MuSPAD %>% mutate(w22_positive_PCR_day_1 = case_when(abs(as.Date(w22_positive_PCR_day_1) - as.Date(firstinfection)) < 14 ~ NA,
                                                        abs(as.Date(w22_positive_PCR_day_1) - as.Date(secondinfection)) < 14 ~ NA,
                                                        abs(as.Date(w22_positive_PCR_day_1) - as.Date(thirdinfection)) < 14 ~ NA,
                                                        abs(as.Date(w22_positive_PCR_day_1) - as.Date(s23_test_covid_2023)) < 14 ~ NA,
                                                        .default = w22_positive_PCR_day_1
                                                        ))

      no_time_infections <- MuSPAD %>% pivot_longer(cols=c("firstinfection", "secondinfection", "thirdinfection", "w22_positive_PCR_day_1", "s23_test_covid_2023"))
      no_time_infections <- no_time_infections %>%
      filter(!is.na(value)) %>%
      filter(value > "2020-01-01")
      no_time_infections <- no_time_infections %>% rename("CounterInfection" = "name",
                                            "DateInfection" = "value")

      no_time_infections <- no_time_infections %>% filter(DateInfection > "2020-01-01") %>% filter(DateInfection < "2023-08-31") %>%
      mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
      mutate(date = MMWRweek2Date(MMWRyear = year,
                              MMWRweek = week,
                              MMWRday = 1))


      if(age_group == "15-34"){
        group_size <- 1186
      } 
      if(age_group == "35-59"){
        group_size <- 3853
      }
      if(age_group ==  "60-79"){
        group_size <- 3892
      }
      if(age_group == "80+"){
        group_size <- 745
      }
      if(age_group == "00+"){
        if(bootstrapping == "no"){
          group_size <- 9921
        }
        if(bootstrapping == "yes"){
          group_size <- 9921
        }
      }
      if(bootstrapping == "no"){
        MuSPAD_time_inf <- no_time_infections %>% group_by(date) %>% count()
        colnames(MuSPAD_time_inf) <- c("Date", "CountPer1096")
        MuSPAD_time_inf <- MuSPAD_time_inf %>% 
                                ungroup() %>%
                                mutate(Incidence100000 = CountPer1096/group_size*100000) %>%
                                mutate(lci = group_size*(CountPer1096/group_size - 1.96*(((CountPer1096/group_size*(1-CountPer1096/group_size))/group_size)^0.5))) %>%
                                mutate(lci = lci/group_size*100000) %>%
                                mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                                mutate(uci = group_size*(CountPer1096/group_size + 1.96*(((CountPer1096/group_size*(1-CountPer1096/group_size))/group_size)^0.5))) %>%
                                mutate(uci = uci/group_size*100000) %>%
                                mutate(Incidence100000 = CountPer1096/group_size*100000) %>%
                                select(Date, Incidence100000, lci, uci)
          DatesInfections <- unique(MuSPAD_time_inf$Date)
          dates <- c()
          date <- as.Date("2020-01-05")
          while(date < "2023-08-31"){
              dates <- append(dates, date)
              date <- date + 7
          }
          for(date in dates){
              date <- as.Date(date)
              if(date %in% unique(MuSPAD_time_inf$Date)){
              }else{
                  row <- c(date, 0, 0, 0)
                  MuSPAD_time_inf <- rbind(MuSPAD_time_inf, row)
              }
          }
      } 
      if(bootstrapping == "yes"){
        MuSPAD_time_inf <- no_time_infections %>% group_by(date, iteration) %>% count()
        colnames(MuSPAD_time_inf) <- c("Date", "iteration", "CountPer1096")
        MuSPAD_time_inf <- MuSPAD_time_inf %>% 
                                ungroup() %>%
                                mutate(Incidence100000_it = CountPer1096/group_size*100000)
            DatesInfections <- unique(MuSPAD_time_inf$Date)
            dates <- c()
            date <- as.Date("2020-01-05")
            while(date < "2023-08-31"){
                dates <- append(dates, date)
                date <- date + 7
            }
            for(i in 1:length(unique(MuSPAD$iteration))){
              for(date in dates){
                  date <- as.Date(date)
                  MuSPAD_time_inf_reduced <- MuSPAD_time_inf %>% filter(iteration == i)
                  if(date %in% unique(MuSPAD_time_inf_reduced$Date)){
                  }else{
                      row <- c(date, i, 0, 0, 0)
                      MuSPAD_time_inf <- rbind(MuSPAD_time_inf, row)
                  }
              }
            }
        MuSPAD_time_inf <- MuSPAD_time_inf %>% group_by(Date) %>% summarize(lci = quantile(Incidence100000, probs =0.025)[[1]], uci = quantile(Incidence100000, probs =0.975)[[1]],  Incidence100000 = mean(Incidence100000_it))
      }

      MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(DataSet = "MuSPAD")

      MuSPAD_time_inf <- MuSPAD_time_inf %>% select(Date, Incidence100000, lci, uci, DataSet)

      MuSPAD_time_inf$Date <- as.Date(MuSPAD_time_inf$Date)
      count_no_infections <- rbind(count_no_infections, MuSPAD_time_inf)
    }

    if(MuSPADavail == "yes"){
    palette_surveyrki_bars <- function() {
      c("#9900CC", "#9fadaf", "#990000")
    }

    palette_surveyrki_errorbars <- function() {
      c("#640085", "#4a5253", "#5c0000")
    }
    } else if(MuSPADavail == "no"){
    palette_surveyrki_bars <- function() {
      c("#9900CC", "#9fadaf")
    }

    palette_surveyrki_errorbars <- function() {
      c("#640085", "#4a5253")
    }
    }

    count_no_infections$DataSet <- factor(count_no_infections$DataSet, levels = c("External Survey", "MuSPAD", "RKI"))
    
    facet_labels <- c(
      "External Survey" = "External Survey (N = 866)",
      "MuSPAD" = "MuSPAD (N = 4,997)",
      "RKI" = "RKI (Population)"
    )

    StopMusPadData <- data.frame(date=as.Date(c("2022-12-13")), 
                                   event=c("Last Infection\nreported to MuSPAD"))

    ComSurveyRki <- ggplot(data = count_no_infections %>% filter(Date > "2020-03-01") %>% filter(Date < "2023-09-01") %>% mutate(DataSet = factor(DataSet, levels = c("External Survey", "RKI", "MuSPAD")))) +
        geom_point (aes(x = Date, y = Incidence100000, color = DataSet), size = 4) +
        geom_ribbon(aes(ymin = lci, ymax = uci, x = Date, fill = DataSet), alpha = 0.15)+
        #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
        theme_minimal() +
        xlab("Date") +
        facet_wrap(~DataSet, nrow = 3, strip.position = "bottom", labeller = labeller(DataSet = facet_labels)) +
        geom_vline(data = count_no_infections, mapping = aes(xintercept = as.Date("2023-05-14")), color = "#990000", size = 1.5) +
        annotate("text", x=as.Date("2023-05-14"), y=1900, label="Last infection reported\nto MuSPAD", angle=90, size =10) +
        geom_vline(data = count_no_infections, mapping = aes(xintercept = as.Date("2023-08-13")), color = "#9900CC", size = 1.5) +
        annotate("text", x=as.Date("2023-08-13"), y=1900, label="Last infections reported\nto external survey", angle=90, size = 10) +
        scale_color_manual(values = palette_surveyrki_bars()) +
        scale_fill_manual(values = palette_surveyrki_errorbars()) +
        ggtitle("7-Day-Incidence per 100,000") +
        #geom_vline(data=StopMusPadData, mapping=aes(xintercept=date), color="#666666", size = 1.5) +
        #geom_text(data=StopMusPadData, mapping=aes(x=date, y=0.04, label=event), size=10, angle=90, nudge_y = 2600, hjust = 0.5) +
        ylab("7-Day-Incidence\nper 100,000") +
        #ggtitle(age_group) +
        theme(text = element_text(size = 55)) +
        scale_y_continuous(breaks = c (0,500,1000,1500,2000,2500), limits=c(0,2750)) +
        theme(legend.position = "none", legend.title = element_blank(), legend.background = element_rect("white")) +
          theme(axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.ticks.length = unit(12, "pt")) +
        scale_x_date(breaks= seq(min(count_no_infections$Date), as.Date("2023-08-31"), by = "6 months"), date_labels = "%Y/%m") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white")) 

    ggarrange(ComSurveyRki, timelineplot2, labels = c("A", "B"), nrow = 2, ncol = 1,  align = "hv", font.label = list(size = 37), heights = c(1.1,0.22))
    
    ggsave(paste0("./plots/VizComparisonIncidenceSurveyRKI_", age_group, "_bootstrapped_", bootstrapping, ".pdf"), dpi = 500, w = 25, h = 30)
    ggsave(paste0("./plots/VizComparisonIncidenceSurveyRKI_", age_group, "_bootstrapped_ ", bootstrapping, ".png"), dpi = 500, w = 25, h = 30)
  }

}

IncidencePlot()
