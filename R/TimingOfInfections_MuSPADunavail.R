library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(ggpubr)

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()

# Procession of Twitter/Mastodon data
InfectionTimingTwitter <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(InfectionTimingTwitter) <- c("TimeFrame", "Share", "OverallNoParticipants", "Recruiter")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.227, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.228, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.192, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.352, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.14, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.18, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.15, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.54, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.13, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.174, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.261, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.435, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.231, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.333, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.051, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.385, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.355, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.151, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.076, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.419, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.196, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.204, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.147, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.453, 764, "Recruiter 2")
InfectionTimingTwitter$Share <- as.double(InfectionTimingTwitter$Share)
InfectionTimingTwitter$OverallNoParticipants <- as.double(InfectionTimingTwitter$OverallNoParticipants)
InfectionTimingTwitter <- InfectionTimingTwitter %>% mutate(NoVoted = Share * OverallNoParticipants)

# Procession of RKI data
rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
rkidata <- rkidata %>% 
        filter(Altersgruppe == "00+") %>% 
        filter(Meldedatum < "2023-10-01") %>%
        mutate(weekday = wday(Meldedatum)) %>%
        filter(weekday == 1)

rkidata <- rkidata %>% select(Meldedatum, `Faelle_7-Tage`)
rkidata <- rkidata %>% filter(Meldedatum > as.Date("2021/12/31")) %>%
filter(Meldedatum < as.Date("2023/01/01")) %>%
mutate(TimeFrame = case_when(Meldedatum <  as.Date("2022/04/01") ~ "Jan - Apr",
                          Meldedatum < as.Date("2022/09/01") ~ "May - Aug",
                          .default = "Sep - Dec"))
colnames(rkidata)[2] <- "CounterInfection"
colnames(rkidata)[1] <- "DateInfection"

rkidata <- rkidata %>% group_by(TimeFrame) %>% summarise(NoVoted = sum(CounterInfection)) %>% ungroup() %>% mutate(Share = 100*NoVoted/(sum(NoVoted))) %>% mutate(OverallNoParticipants = sum(NoVoted)) %>% mutate(Recruiter = "RKI")
colnames(rkidata)[2] <- "NoVoted"

InfectionTimingTwitter <- rbind(InfectionTimingTwitter, rkidata)

palette_recruiters_bars <- function(){
  c("#253494", "#ffffcc", "#7fcdbb", "#2c7fb8", "#c7e9b4", "#663300", "#9fadaf")
}
palette_recruiters_errorbars <- function(){
  c("#1a2569", "#c9c99f", "#5e978a", "#1d577d", "#8ba37d", "#261300", "#515859")
}

twitterplot <- InfectionTimingTwitter %>% filter(TimeFrame != "Show Me The Answer")  %>%
                            group_by(TimeFrame, Recruiter) %>% summarise(n = sum(NoVoted)) %>% group_by(Recruiter) %>%
                            mutate(percent = 100 * n / sum(n)) %>%
                            mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(lci = 100/sum(n)*lci) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                            mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(uci = 100/sum(n)*uci) %>%
                            mutate(Recruiter = factor(Recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "RKI"))) %>%
ggplot(aes(x = TimeFrame, y = percent)) +
geom_bar(aes(fill=Recruiter), stat = "identity", position = "dodge", width = 0.8) +
geom_errorbar(aes(x=TimeFrame, ymin=lci, ymax=uci, colour = Recruiter), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
theme_minimal() +
scale_fill_manual(values = palette_recruiters_bars()) +
scale_color_manual(values = palette_recruiters_errorbars()) +
scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,10,20,30,40,50, 60,70,80), limits = c(0,80)) +
ylab("Share (Percentage)") +
xlab("Timing of Infection (2022)") +
theme(text = element_text(size = 33)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
theme(panel.spacing = unit(4, "cm", data = NULL)) +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

#ggsave("TimingOfInfections_Twitter.pdf", dpi = 500, w = 12, h = 7.5)
#ggsave("TimingOfInfections_Twitter.png", dpi = 500, w = 12, h = 7.5)