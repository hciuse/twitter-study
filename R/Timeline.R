library(tidyverse)
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Timeline plot including MuSPAD

tasks <- c("Twitter", "Twitter",
           "Mastodon", "Mastodon",
           "External Survey", "External Survey",
           "RKI",
           "MuSPAD", "MuSPAD", "MuSPAD", "MuSPAD",
           "COSMO", "COSMO")

dfr <- data.frame(
  name = factor(tasks, levels =  c("COSMO",
                "MuSPAD",
                "RKI",
                "External Survey",
                "Mastodon",
                "Twitter")),
  start.date = as.Date(c("2023-07-19", "2020-03-01", 
                         "2023-07-19", "2020-03-01",
                         "2023-07-18", "2020-12-27",
                        "2020-12-27", 
                         "2022-05-01", "2022-12-01", "2023-04-01", "2020-12-27",
                         "2022-11-26", "2020-03-01")),
                         
  end.date = as.Date(c("2023-07-26", "2023-07-26",
                      "2023-07-26", "2023-07-26",
                      "2023-08-30", "2023-08-30",
                      "2023-09-11",
                      "2022-11-03", "2023-04-01", "2023-08-31", "2023-08-31",
                      "2022-11-30", "2022-11-30")),
  source = c("Twitter", "Twitter", "Mastodon", "Mastodon", "External Survey", "External Survey", "RKI", "MuSPAD", "MuSPAD", "MuSPAD", "MuSPAD", "COSMO", "COSMO"),
  type = c("Data Collection", "Reference Period", "Data Collection", "Reference Period", "Data Collection", "Reference Period", "Data Collection", "Data Collection", "Data Collection", "Data Collection", "Reference Period", "Data Collection", "Reference Period")
)

mdfr <- reshape2::melt(dfr, measure.vars = c("start.date", "end.date"))

palette_twittermastodonsurvey_bars <- function(){
  c("#CC3300", "#9900CC",  "#663300", "#990000", "#41b6c4")
}

timelineplot <- ggplot(dfr %>% filter(source != "RKI")) +
      geom_linerange(aes(y = name, 
                         xmin = start.date,
                         xmax = end.date,
                         colour = source,
                         linetype = type),
                         linewidth = c(5, 2, 5, 2, 5, 2, 5, 5, 5, 2, 5, 2)) +
      scale_x_date(breaks= seq(min(dfr$start.date), as.Date("2023-09-01"), by = "6 months"), date_labels = "%Y/%m")+
      theme_minimal() +
      scale_color_manual(values = palette_twittermastodonsurvey_bars(), guide = 'none') +
      ylab("") +
      ggtitle("Data Collection/Infection Occurence Timeline") +
      theme(text = element_text(size = 48)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(linetype = guide_legend(override.aes = list(linewidth = 2)))+
      theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        theme(plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(1.5, 'cm'))

#Timeline plot excluding MuSPAD

tasks <- c("Twitter", "Twitter",
           "Mastodon", "Mastodon",
           "External Survey", "External Survey",
           "RKI",
           "COSMO", "COSMO")

dfr_noMuSPAD <- data.frame(
  name = factor(tasks, levels =  c("COSMO",
                "MuSPAD",
                "RKI",
                "External Survey",
                "Mastodon",
                "Twitter")),
  start.date = as.Date(c("2023-07-19", "2020-03-01", 
                         "2023-07-19", "2020-03-01",
                         "2023-07-18", "2020-12-27",
                        "2020-12-27", 
                         "2022-11-26", "2020-03-01")),
                         
  end.date = as.Date(c("2023-07-26", "2023-07-26",
                      "2023-07-26", "2023-07-26",
                      "2023-08-30", "2023-08-30",
                      "2023-09-11",
                      "2022-11-30", "2022-11-30")),
  source = c("Twitter", "Twitter", "Mastodon", "Mastodon", "External Survey", "External Survey", "RKI", "COSMO", "COSMO"),
  type = c("Data Collection", "Reference Period", "Data Collection", "Reference Period", "Data Collection", "Reference Period", "Data Collection", "Data Collection", "Reference Period")
)

mdfr <- reshape2::melt(dfr, measure.vars = c("start.date", "end.date"))

palette_twittermastodonsurvey_bars <- function(){
  c("#CC3300", "#9900CC",  "#663300", "#990000", "#41b6c4")
}

timelineplotnoMuSPAD <- ggplot(dfr_noMuSPAD %>% filter(source != "RKI")) +
      geom_linerange(aes(y = name, 
                         xmin = start.date,
                         xmax = end.date,
                         colour = source,
                         linetype = type),
                         linewidth = c(5, 2, 5, 2, 5, 2, 5, 2)) +
      scale_x_date(breaks= seq(min(dfr$start.date), as.Date("2023-09-01"), by = "6 months"), date_labels = "%Y/%m")+
      theme_minimal() +
      scale_color_manual(values = palette_twittermastodonsurvey_bars(), guide = 'none') +
      ylab("") +
      ggtitle("Data Collection/Infection Occurence Timeline") +
      theme(text = element_text(size = 48)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(linetype = guide_legend(override.aes = list(linewidth = 2)))+
      theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        theme(plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(1.5, 'cm'))

#Timeline plot for vaccination figures
palette_twittermastodonsurvey_bars <- function(){
  c("#9900CC", "#990000", "#9fadaf")
}

timelineplot2 <- ggplot(dfr %>% filter(source %in% c("External Survey", "RKI", "MuSPAD"))) +
      geom_linerange(aes(y = name, 
                         xmin = start.date,
                         xmax = end.date,
                         colour = source,
                         linetype = type),
                         linewidth = c(5, 2, 5, 5, 5, 5, 2)) +
      scale_x_date(breaks= seq(as.Date("2020-01-01"), as.Date("2023-09-30"), by = "6 months"), date_labels = "%Y/%m")+
      theme_minimal() +
      scale_color_manual(values = palette_twittermastodonsurvey_bars(), guide = 'none') +
      ylab("") +
      ggtitle("Data Collection/Infection Occurence Timeline") +
      theme(text = element_text(size = 55)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(linetype = guide_legend(override.aes = list(linewidth = 2)))+
      theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(10, "pt")) +
        theme(plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(1.5, 'cm'))



