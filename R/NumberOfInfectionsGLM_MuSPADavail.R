library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(readxl)
library(lme4)
library(boot)
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

ext_survey_df <- readRDS(file = here("data", "cleaned_data.rds"))
source(here("R", "Timeline.R"))
source(here("R", "MuSPADPreprocessing.R"))

# Number of infections (by recruiter) with Mixed Model ----------------------------------------------------

palette_recruiters_bars <- function(){
  c("#253494", "#ffffcc", "#7fcdbb", "#2c7fb8", "#c7e9b4", "#663300")
}
palette_recruiters_errorbars <- function(){
  c("#1a2569", "#c9c99f", "#5e978a", "#1d577d", "#8ba37d", "#261300")
}

# Create individual-level data for mixed model
create_individual_data <- function(n, proportions, recruiter_name) {
  data.frame(
    num_c19_infs_eng = factor(rep(c("0", "1", "2+"), times = round(n * proportions)),
                              levels = c("0", "1", "2+")),
    recruiter = recruiter_name
  )
}

# Combine all recruiter data
individual_data <- bind_rows(
  create_individual_data(2120, c(0.3030, 0.5270, 0.1390), "Recruiter 1 (Twitter)"),
  create_individual_data(101, c(0.1780, 0.5740, 0.2180), "Recruiter 5"),
  create_individual_data(111, c(0.2610, 0.5410, 0.1530), "Recruiter 4"),
  create_individual_data(371, c(0.2210, 0.5690, 0.1640), "Recruiter 3"),
  create_individual_data(1667, c(0.2520, 0.5180, 0.1930), "Recruiter 2"),
  create_individual_data(1802, c(0.37, 0.49, 0.12), "Recruiter 1 (Mastodon)")
)

# Convert to binary outcomes for each infection category
individual_data <- individual_data %>%
  mutate(
    is_zero = as.numeric(num_c19_infs_eng == "0"),
    is_one = as.numeric(num_c19_infs_eng == "1"),
    is_two_plus = as.numeric(num_c19_infs_eng == "2+")
  )

# Fit mixed models for each infection category
model_zero <- glmer(is_zero ~ 1 + (1|recruiter), 
                    data = individual_data, 
                    family = binomial)
model_one <- glmer(is_one ~ 1 + (1|recruiter), 
                   data = individual_data, 
                   family = binomial)
model_two_plus <- glmer(is_two_plus ~ 1 + (1|recruiter), 
                        data = individual_data, 
                        family = binomial)

# Function to get predictions with uncertainty from mixed model
get_predictions <- function(model, data) {
  # Get predictions on probability scale
  pred <- predict(model, newdata = data, type = "response", re.form = ~(1|recruiter))
  
  # Bootstrap confidence intervals
  boot_fun <- function(model) {
    predict(model, newdata = data, type = "response", re.form = ~(1|recruiter))
  }
  
  # Use parametric bootstrap
  boot_results <- bootMer(model, boot_fun, nsim = 500, seed = 123)
  
  # Calculate confidence intervals
  ci <- apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975))
  
  return(list(
    fit = pred,
    lci = ci[1, ],
    uci = ci[2, ]
  ))
}

# Create prediction data frame
pred_data <- data.frame(
  recruiter = unique(individual_data$recruiter)
)

# Get predictions for each category
pred_zero <- get_predictions(model_zero, pred_data)
pred_one <- get_predictions(model_one, pred_data)
pred_two_plus <- get_predictions(model_two_plus, pred_data)

# Combine predictions into plotting format
plot_data <- bind_rows(
  data.frame(
    recruiter = pred_data$recruiter,
    num_c19_infs_eng = "0",
    percent = pred_zero$fit * 100,
    lci = pred_zero$lci * 100,
    uci = pred_zero$uci * 100
  ),
  data.frame(
    recruiter = pred_data$recruiter,
    num_c19_infs_eng = "1",
    percent = pred_one$fit * 100,
    lci = pred_one$lci * 100,
    uci = pred_one$uci * 100
  ),
  data.frame(
    recruiter = pred_data$recruiter,
    num_c19_infs_eng = "2+",
    percent = pred_two_plus$fit * 100,
    lci = pred_two_plus$lci * 100,
    uci = pred_two_plus$uci * 100
  )
) %>%
  mutate(num_c19_infs_eng = factor(num_c19_infs_eng, levels = c("0", "1", "2+")))

# Create plot with mixed model uncertainty bands
plot_data %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(aes(fill = factor(recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", 
                                                   "Recruiter 3", "Recruiter 4", 
                                                   "Recruiter 5", "Recruiter 1 (Mastodon)"))), 
           stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x = num_c19_infs_eng, ymin = lci, ymax = uci, 
                    colour = factor(recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", 
                                                          "Recruiter 3", "Recruiter 4", 
                                                          "Recruiter 5", "Recruiter 1 (Mastodon)"))), 
                position = position_dodge(0.8), width = 0.3, alpha = 0.9, size = 1.3) +
  theme_minimal() +
  ylab("Share (Percentage)") +
  xlab("Number of Infections") +
  scale_fill_manual(values = palette_recruiters_bars()) +
  scale_color_manual(values = palette_recruiters_errorbars()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.5), 
                     breaks = c(0, 12.5, 25, 37.5, 50, 75, 100)) +
  theme(text = element_text(size = 33)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(here("plots", "NoInfections_Comparison_Recruiter_MixedModel.pdf"), dpi = 500, w = 10, h = 7.5)
ggsave(here("plots", "NoInfections_Comparison_Recruiter_MixedModel.png"), dpi = 500, w = 10, h = 7.5)

# Print model summaries
cat("\n=== Model Summary for '0 infections' ===\n")
print(summary(model_zero))
cat("\n=== Model Summary for '1 infection' ===\n")
print(summary(model_one))
cat("\n=== Model Summary for '2+ infections' ===\n")
print(summary(model_two_plus))

# Extract and print random effects variance
cat("\n=== Random Effects Variance ===\n")
cat("0 infections - Recruiter variance:", as.numeric(VarCorr(model_zero)$recruiter), "\n")
cat("1 infection - Recruiter variance:", as.numeric(VarCorr(model_one)$recruiter), "\n")
cat("2+ infections - Recruiter variance:", as.numeric(VarCorr(model_two_plus)$recruiter), "\n")
