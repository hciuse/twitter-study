# Participant Flow Diagram
library(DiagrammeR)
library(dplyr)
library(tidyr)
library(here)

# Load the cleaned data
data <- readRDS(here("data", "cleaned_data.rds"))

# Define flow numbers
twitter_q1 <- 4370
twitter_q2 <- 2129
mastodon_q1 <- 1802
mastodon_q2 <- 738

# Total social media poll voters
total_poll_voters <- twitter_q1 + twitter_q2 + mastodon_q1 + mastodon_q2

# External survey numbers
survey_started <- 867
survey_completed <- 398

# Calculate exclusions
# Speeders and age<18 are the exclusion criteria
excluded_speeders <- 1143 - survey_started

# Create the flow diagram
flow_diagram <- grViz("
digraph participant_flow {
  
  # Graph attributes
  graph [layout = dot, rankdir = TB, fontname = Arial]
  
  # Node attributes
  node [shape = box, style = 'rounded,filled', fillcolor = lightblue, 
        fontname = Arial, fontsize = 10, margin = 0.2]
  
  # Define nodes
  A [label = 'Twitter + Mastodon Poll Votes\n N = 9,039\n\nTwitter Q1: 4,370\nTwitter Q2: 2,129\nMastodon Q1: 1,802\nMastodon Q2: 738', 
     fillcolor = '#E8F4F8']
  
  B [label = 'Clicked Through to\nExternal Survey\n N = 1143', 
     fillcolor = '#D4E9F7']
  
  C [label = 'Excluded\n N = 276\n\nSpeeders: <1/3 median time\nAge < 18 years', 
     fillcolor = '#FFE6E6', shape = box]
  
  D [label = 'Completed Sufficient Items\nfor Analysis\n N = 867', 
     fillcolor = '#C8E6C9']
  
  # Define edges
  A -> B [label = '  1143 started survey  ', fontsize = 9]
  B -> C [label = '  276 excluded  ', fontsize = 9, style = dashed]
  B -> D [label = '  867 retained  ', fontsize = 9]
  
  # Subgraph for excluded participants
  {rank = same; C}
}
")

# Display the diagram
print(flow_diagram)

# Save the diagram
# Export as PNG
library(DiagrammeRsvg)
library(rsvg)

flow_diagram %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png(here("plots", "participant_flow_diagram.png"), width = 1200)

# Specify the variables of interest
vars <- c("gender", "year_of_birth", "num_c19_infs", "date_f1_inf", "date_s2_inf", "date_t3_inf", "c19_vaccination_status")

# Calculate percentage of missing values for each variable
missing_report <- sapply(vars, function(v) {
  mean(is.na(data[[v]])) * 100
})

# Convert to a neat data frame for better readability
missing_report <- data.frame(
  variable = vars,
  missing_percent = round(missing_report, 2)
)

# Print the report
print(missing_report)